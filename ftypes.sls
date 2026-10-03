(library (ftypes)
  (export ftype-copy-fields! with-ftypes define-with-ftypes lambda-ftypes)
  (import (chezscheme))

  (meta define (split-string-at-dot s)
    (let loop ([chars (string->list s)]
               [part '()]
               [parts '()])
      (cond
       [(null? chars)
        (reverse (cons (list->string (reverse part)) parts))]
       [(char=? (car chars) #\.)
        (loop (cdr chars) '()
              (cons (list->string (reverse part)) parts))]
       [else (loop (cdr chars) (cons (car chars) part) parts)])))

  (meta define (split-identifier stx)
    (syntax-case stx ()
      [id (identifier? #'id)
          (let* ([name (symbol->string (syntax->datum #'id))]
                 [parts (split-string-at-dot name)]
                 [symbols (map (lambda (part)
                                 (or (string->number part)
                                     (string->symbol part)))
                               parts)])
            (map (lambda (sym) (datum->syntax #'id sym)) symbols))]
      [_ (syntax-error #'id "expected an identifier")]))

  (meta define (contains-dot? stx)
    (syntax-case stx ()
      [id (identifier? #'id)
          (let* ([name (symbol->string (syntax->datum #'id))]
                 [parts (split-string-at-dot name)])
            (fx> (length parts) 1))]
      [_ (syntax-error #'id "expected an identifier")]))

  (meta define (is-reference? stx)
    (syntax-case stx ()
      [id (identifier? #'id)
          (let* ([name (symbol->string (syntax->datum #'id))])
            (and (fx> (string-length name) 1)
                 (char=? (string-ref name 0) #\&)))]
      [_ (syntax-error #'id "expected an identifier")]))

  (meta define (remove-reference stx)
    (syntax-case stx ()
      [id (identifier? #'id)
          (let* ([name (symbol->string (syntax->datum #'id))])
            (datum->syntax #'id
                           (string->symbol
                            (list->string (cdr (string->list name))))))]
      [_ (syntax-error #'id "expected an identifier")]))

  (meta define (head-identifier stx)
    (syntax-case stx ()
      [id (identifier? #'id)
          (let* ([name (symbol->string (syntax->datum #'id))]
                 [parts (split-string-at-dot name)]
                 [symbols (map string->symbol parts)])
            (datum->syntax #'id (car symbols)))]
      [_ (syntax-error #'id "expected an identifier")]))

  (meta define (lookup-identifier id env)
    (cond
     [(null? env) #f]
     [(free-identifier=? id (caar env)) (cadar env)]
     [else (lookup-identifier id (cdr env))]))

  (define-syntax (ftype-copy-fields! stx)
    (syntax-case stx ()
      [(_ ftype dest src)
       (let* ([blueprint-expr #'(cdr (ftype-pointer->sexpr
                                      (make-ftype-pointer ftype 0)))]
              [layout (eval (syntax->datum blueprint-expr)
                            (interaction-environment))])
         (define (generate-assignments layout path acc)
           (fold-left
            (lambda (acc field-spec)
              (let* ([field-name (datum->syntax #'ftype (car field-spec))]
                     [field-type (cadr field-spec)]
                     [full-path (append path (list field-name))])
                (cond
                 [(and (pair? field-type) (eq? (car field-type) 'struct))
                  (generate-assignments (cdr field-type) full-path acc)]
                 [(and (pair? field-type) (eq? (car field-type) 'bits))
                  (generate-assignments
                   (map (lambda (t) (list (car t) (cadr t)))
                        (cdr field-type))
                   full-path acc)]
                 [(and (pair? field-type) (eq? (car field-type) 'array))
                  (generate-assignments
                   (map (lambda (i) (list i (caddr field-type)))
                        (iota (cadr field-type)))
                   full-path acc)]
                 [else
                  (with-syntax ([(p ...) full-path])
                    (cons
                     #'(ftype-set! ftype (p ...) dest
                                   (ftype-ref ftype (p ...) src))
                     acc))])))
            acc layout))
         (with-syntax ([(stmt ...) (generate-assignments layout '() '())])
           #'(begin stmt ...)))]))

  (meta define (parse-body body env)
    (syntax-case body ()
      [(op id value) (and (identifier? #'op)
                          (literal-identifier=? #'op
                                                (datum->syntax #'op '::=)))
       (with-syntax ([type (or (lookup-identifier #'id env)
                               (lookup-identifier #'value env))]
                     [parsed-head (parse-body #'id env)]
                     [parsed-value (parse-body #'value env)])
         #'(ftype-copy-fields! type parsed-head parsed-value))]
      [(op id value) (and (identifier? #'op)
                          (literal-identifier=? #'op
                                                (datum->syntax #'op ':=))
                          (identifier? #'id)
                          (contains-dot? #'id)
                          (lookup-identifier (head-identifier #'id) env))
       (with-syntax ([head (head-identifier #'id)])
         (with-syntax ([type (lookup-identifier #'head env)]
                       [(fields ...) (cdr (split-identifier #'id))]
                       [parsed-value (parse-body #'value env)])
           #'(ftype-set! type (fields ...) head parsed-value)))]
      [id (and (identifier? #'id)
               (is-reference? #'id)
               (lookup-identifier
                (head-identifier (remove-reference #'id)) env))
          (with-syntax ([head (head-identifier (remove-reference #'id))])
            (with-syntax ([type (lookup-identifier #'head env)]
                          [(fields ...)
                           (cdr (split-identifier (remove-reference #'id)))])
              #'(ftype-&ref type (fields ...) head)))]
      [id (and (identifier? #'id)
               (contains-dot? #'id)
               (lookup-identifier (head-identifier #'id) env))
          (with-syntax ([head (head-identifier #'id)])
            (with-syntax ([type (lookup-identifier #'head env)]
                          [(fields ...) (cdr (split-identifier #'id))])
              #'(ftype-ref type (fields ...) head)))]
      [(head tail ...)
       (with-syntax ([head (parse-body #'head env)]
                     [(rest ...) (parse-body #'(tail ...) env)])
         #'(head rest ...))]
      [else body]))

  (define-syntax (with-ftypes stx)
    (syntax-case stx ()
      [(_ [form ...] body ...)
       (let ([env (fold-left
                   (lambda (acc form)
                     (syntax-case form ()
                       [(type ids ...)
                        (fold-left (lambda (acc id)
                                     (cons (list id #'type) acc))
                                   acc #'(ids ...))]))
                   '()
                   #'(form ...))])
         (with-syntax ([(body ...)
                        (map (lambda (body) (parse-body body env))
                             #'(body ...))])
           #'(begin body ...)))]))

  (define-syntax (define-with-ftypes stx)
    (syntax-case stx ()
      [(_ (id args* ...) body* ...)
       (with-syntax ([(args* ...)
                      (fold-left append '()
                                 (map (lambda (stx)
                                        (syntax-case stx ()
                                          [(_ args* ...) #'(args* ...)]
                                          [arg (list #'arg)]))
                                      #'(args* ...)))]
                     [(ann-ftypes* ...) (filter (lambda (stx)
                                                  (syntax-case stx ()
                                                    [(typ args* ...) #t]
                                                    [_ #f]))
                                                #'(args* ...))])
         #'(define (id args* ...)
             (with-ftypes (ann-ftypes* ...)
               body*) ...))]))

  (define-syntax (lambda-ftypes stx)
    (syntax-case stx ()
      [(_ (args* ...) body* ...)
       (with-syntax ([(args* ...)
                      (fold-left append '()
                                 (map (lambda (stx)
                                        (syntax-case stx ()
                                          [(_ args* ...) #'(args* ...)]
                                          [arg (list #'arg)]))
                                      #'(args* ...)))]
                     [(ann-ftypes* ...) (filter (lambda (stx)
                                                  (syntax-case stx ()
                                                    [(typ args* ...) #t]
                                                    [_ #f]))
                                                #'(args* ...))])
         #'(lambda (args* ...)
             (with-ftypes (ann-ftypes* ...)
               body*) ...))])))

;; Local Variables:
;; eval: (put 'meta 'scheme-indent-function 2)
;; eval: (put 'with-ftypes 'scheme-indent-function 1)
;; End:
