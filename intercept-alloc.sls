(library (intercept-alloc)
  (export foreign-alloc with-auto-free with-custom-alloc)
  (import (prefix (chezscheme) cs:)
          (except (chezscheme) foreign-alloc foreign-free))
  (define current-allocate (make-parameter cs:foreign-alloc))
  (define current-free (make-parameter cs:foreign-free))
  (define current-track-allocation (make-parameter (lambda (ptr) ptr)))
  (define (allocate . args)
    (let ([ptr (apply (current-allocate) args)])
      ((current-track-allocation) ptr)))
  (define-syntax foreign-alloc
    (syntax-rules ()
      [(_ args ...) (allocate args ...)]))
  (define-syntax foreign-free
    (syntax-rules ()
      [(_ args ...) ((current-free) args ...)]))
  (define-syntax with-auto-free
    (syntax-rules ()
      [(_ body ...)
       (let ([alloc-list '()])
         (dynamic-wind
           (lambda () #f)
           (lambda ()
             (parameterize ([current-track-allocation 
                             (lambda (ptr)
                               (set! alloc-list (cons ptr alloc-list))
                               ptr)])
               body ...))
           (lambda () (for-each (lambda (ptr) (foreign-free ptr)) alloc-list))))]))
  (define-syntax with-custom-alloc
    (syntax-rules ()
      [(_ alloc free body ...)
       (parameterize ([current-allocate alloc]
                      [current-free free])
         body ...)])))
