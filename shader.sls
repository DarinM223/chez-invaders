(library (shader)
  (export foreign-alloc-array define-shader shader-init! shader-reload? shader-reload!)
  (import (chezscheme) (utils) (raylib raylib (0 3)))

  (meta define (syntax-append stx str)
    (datum->syntax
     stx
     (string->symbol
      (string-append (symbol->string (syntax->datum stx)) str))))

  (define (foreign-alloc-array type values)
    (let* ([element-size (foreign-sizeof type)]
           [ptr (foreign-alloc (fx* element-size (length values)))])
      (do ([xs values (cdr xs)]
           [i 0 (fx1+ i)])
          ((null? xs))
        (foreign-set! type ptr (fx* i element-size) (car xs)))
      ptr))

  (define (with-foreign-array type values proc)
    (let* ([element-size (foreign-sizeof type)]
           [ptr (foreign-alloc (* element-size (length values)))])
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (do ([xs values (cdr xs)]
               [i 0 (+ i 1)])
              ((null? xs))
            (foreign-set! type ptr (* i element-size) (car xs)))
          (proc ptr))
        (lambda () (foreign-free ptr)))))

  ;; Private identifiers to be used as keys into define-property
  (meta define vertex-shader)
  (meta define fragment-shader)
  (meta define uniforms)
  (meta define uniform-types)
  (meta define uniform-counts)

  (define-syntax (define-shader stx)
    (syntax-case stx ()
      [(_ shader vertex-file fragment-file ((uniforms* types* . counts*) ...))
       (with-syntax ([shader-mod-time (syntax-append #'shader "-modified")])
         #'(begin
             (define-toplevel shader #f)
             (define shader-mod-time (max (get-file-mod-time vertex-file)
                                          (get-file-mod-time fragment-file)))
             (define-property shader vertex-shader vertex-file)
             (define-property shader fragment-shader fragment-file)
             (define-property shader uniforms #'(uniforms* ...))
             (define-property shader uniform-types #'(types* ...))
             (define-property shader uniform-counts #'(counts* ...))))]))

  (meta define (set-location shader)
    (lambda (uniform value type count-tail)
      (with-syntax ([shader shader])
        (let ([counts (syntax->list count-tail)])
          (cond
           [(null? counts)
            (with-syntax ([uniform uniform]
                          [value value]
                          [type type])
              #'(let ([loc (get-shader-location shader uniform)])
                  (set-shader-value shader loc value type)))]
           [(null? (cdr counts))
            (with-syntax ([uniform uniform]
                          [value value]
                          [type type]
                          [count (car counts)])
              #'(let ([loc (get-shader-location shader uniform)])
                  (set-shader-value-v shader loc value type count)))]
           [else (syntax-error count-tail "expected zero or one uniform count")])))))

  (define-syntax (shader-init! stx)
    (lambda (lookup)
      (syntax-case stx ()
        [(_ shader (uniform-values* ...))
         (with-syntax ([vertex-file
                        (datum->syntax #'shader
                                       (lookup #'shader #'vertex-shader))]
                       [fragment-file
                        (datum->syntax #'shader
                                       (lookup #'shader #'fragment-shader))]
                       [(set* ...) (map (set-location #'shader)
                                        (lookup #'shader #'uniforms)
                                        #'(uniform-values* ...)
                                        (lookup #'shader #'uniform-types)
                                        (lookup #'shader #'uniform-counts))])
           #'(begin
               (set! shader
                     (if-env-release
                      (load-shader vertex-file fragment-file)
                      (begin (format #t "TODO: Release mode\n") #f)))
               set* ...))])))

  (define-syntax (shader-reload? stx)
    (lambda (lookup)
      (syntax-case stx ()
        [(_ shader)
         (with-syntax ([vertex-file
                        (datum->syntax #'shader (lookup #'shader #'vertex-shader))]
                       [fragment-file
                        (datum->syntax #'shader (lookup #'shader #'fragment-shader))]
                       [shader-mod-time (syntax-append #'shader "-modified")])
           #'(let ([curr-shader-mod-time
                    (max (get-file-mod-time vertex-file)
                         (get-file-mod-time fragment-file))])
               (not (fx= curr-shader-mod-time shader-mod-time))))])))

  (define-syntax (shader-reload! stx)
    (lambda (lookup)
      (syntax-case stx ()
        [(_ shader (uniform-values* ...) (refs* ...))
         (with-syntax
             ([updated-shader (datum->syntax #'here (gensym))]
              [vertex-file
               (datum->syntax #'shader (lookup #'shader #'vertex-shader))]
              [fragment-file
               (datum->syntax #'shader (lookup #'shader #'fragment-shader))]
              [shader-mod-time (syntax-append #'shader "-modified")]
              [(set-uniforms* ...) (map (set-location #'shader)
                                        (lookup #'shader #'uniforms)
                                        #'(uniform-values* ...)
                                        (lookup #'shader #'uniform-types)
                                        (lookup #'shader #'uniform-counts))])
           (with-syntax
               ([(set-refs* ...)
                 (map (lambda (ref)
                        (with-syntax ([ref ref])
                          #'(when (and ref (fx= (ftype-ref Shader (id) shader)
                                                (ftype-ref Shader (id) ref)))
                              (set! ref updated-shader))))
                      #'(refs* ...))])
             #'(let ([curr-shader-mod-time
                      (max (get-file-mod-time vertex-file)
                           (get-file-mod-time fragment-file))])
                 (unless (fx= curr-shader-mod-time shader-mod-time)
                   (let ([updated-shader (load-shader vertex-file fragment-file)])
                     (format #t "Shader modified, reloading ~a and ~a...\n"
                             vertex-file fragment-file)
                     set-refs* ...
                     (unload-shader shader)
                     (set! shader updated-shader)
                     set-uniforms* ...
                     (set! shader-mod-time curr-shader-mod-time))))))]))))

;; Local Variables:
;; eval: (put 'meta 'scheme-indent-function 2)
;; End:
