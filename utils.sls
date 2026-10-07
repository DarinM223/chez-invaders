(library (utils)
  (export define-toplevel alloc-struct -> ->> print-ftype-fields safe-call)
  (import (chezscheme))

  (define-syntax define-toplevel
    (lambda (stx)
      (syntax-case stx ()
        [(_ id value) (identifier? #'id)
         #'(unless (top-level-bound? (quote id))
             (define-top-level-value (quote id) value))])))

  (define-syntax (alloc-struct stx)
    (syntax-case stx ()
      [(_ ftype)
       #'(make-ftype-pointer ftype (foreign-alloc (ftype-sizeof ftype)))]))

  (define-syntax (-> stx)
    (syntax-case stx ()
      [(_ id f* ...)
       (fold-left (lambda (acc f)
                    (with-syntax ([acc acc] [(head members ...) f])
                      #'(head acc members ...))) #'id #'(f* ...))]))

  (define-syntax (->> stx)
    (syntax-case stx ()
      [(_ id f* ...)
       (fold-left (lambda (acc f)
                    (with-syntax ([acc acc] [(members ...) f])
                      #'(members ... acc))) #'id #'(f* ...))]))

  (define-syntax (print-ftype-fields stx)
    (syntax-case stx ()
      [(_ ftype-id)
       #'(let* ([dummy-ptr (make-ftype-pointer ftype-id 0)]
                [struct-sexpr (ftype-pointer->sexpr dummy-ptr)])
           (map car (cdr struct-sexpr)))]))

  (define (safe-call thunk)
    (guard (condition
            [else
             (display "[Scheme Error]: ")
             (display-condition condition)
             (newline)])
      (thunk))))
