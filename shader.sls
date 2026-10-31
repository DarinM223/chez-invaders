(library (shader)
  (export define-shader shader-init! shader-reload!)
  (import (chezscheme) (utils) (raylib raylib (0 3)))
  
  (meta define (syntax-append stx str)
    (datum->syntax
     stx
     (string->symbol
      (string-append (symbol->string (syntax->datum stx)) str))))

  ;; Private identifiers to be used as keys into define-property
  (meta define vertex-shader)
  (meta define fragment-shader)
  (meta define uniforms)
  (meta define uniform-types)

  (define-syntax (define-shader stx)
    (syntax-case stx ()
      [(_ shader vertex-file fragment-file ((uniforms* types*) ...))
       (with-syntax ([shader-mod-time (syntax-append #'shader "-modified")])
         #'(begin
             (define-toplevel shader #f)
             (define shader-mod-time (max (get-file-mod-time vertex-file)
                                          (get-file-mod-time fragment-file)))
             (define-property shader vertex-shader vertex-file)
             (define-property shader fragment-shader fragment-file)
             (define-property shader uniforms #'(uniforms* ...))
             (define-property shader uniform-types #'(types* ...))))]))

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
                       [(set* ...) (map
                                    (lambda (uniform value type)
                                      (with-syntax ([uniform uniform]
                                                    [value value]
                                                    [type type])
                                        #'(let ([loc (get-shader-location shader uniform)])
                                            (set-shader-value shader loc value type))))
                                    (lookup #'shader #'uniforms)
                                    #'(uniform-values* ...)
                                    (lookup #'shader #'uniform-types))])
           #'(begin
               (set! shader (load-shader vertex-file fragment-file))
               set* ...))])))

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
              [(set-uniforms* ...) (map
                                    (lambda (uniform value type)
                                      (with-syntax ([uniform uniform]
                                                    [value value]
                                                    [type type])
                                        #'(let ([loc (get-shader-location shader uniform)])
                                            (set-shader-value shader loc value type))))
                                    (lookup #'shader #'uniforms)
                                    #'(uniform-values* ...)
                                    (lookup #'shader #'uniform-types))])
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
