(library (raylib intercept-alloc)
  (export foreign-alloc with-auto-free)
  (import (prefix (chezscheme) cs:)
          (except (chezscheme) foreign-alloc))
  (define current-allocator (make-parameter cs:foreign-alloc))
  (define-syntax foreign-alloc
    (syntax-rules ()
      [(_ args ...) ((current-allocator) args ...)]))
  (define-syntax with-auto-free
    (syntax-rules ()
      [(_ body ...)
       (let ([alloc-list '()]
             [original-alloc (current-allocator)])
         (dynamic-wind
           (lambda () #f)
           (lambda ()
             (parameterize ([current-allocator 
                             (lambda (size)
                               (let ([ptr (original-alloc size)])
                                 (set! alloc-list (cons ptr alloc-list))
                                 ptr))])
               body ...))
           (lambda () (for-each foreign-free alloc-list))))])))
