(library (arena)
  (export make-arena arena-try-alloc arena-alloc arena-reset arena-free)
  (import (chezscheme))

  (define-record arena-allocator
    ([unsigned-64 start]
     [unsigned-64 current]
     [unsigned-64 end]))

  (define (make-arena total-bytes)
    (let ([ptr (foreign-alloc total-bytes)])
      (make-arena-allocator ptr ptr (fx+ ptr total-bytes))))
  (define (arena-try-alloc allocator size align count)
    (let* ([current (arena-allocator-current allocator)]
           [end (arena-allocator-end allocator)]
           [mask (fx- align 1)]
           [pad (fxand (fx- current) mask)]
           [available (fx- end current pad)])
      (and (fx> size 0)
           (fx> align 0)
           (fx<= count (fx/ available size))
           (let ([ptr (fx+ current pad)])
             (set-arena-allocator-current! allocator
                                           (fx+ ptr (fx* count size)))
             ptr))))
  ;; Assumes align is a power of 2.
  (define arena-alloc
    (rec arena-alloc
      (case-lambda
        [(allocator size align) (arena-alloc allocator size align 1)]
        [(allocator size align count)
         (let ([ptr (arena-try-alloc allocator size align count)])
           (if ptr ptr (raise "can't allocate")))])))
  (define (arena-reset allocator)
    (set-arena-allocator-current! allocator
                                  (arena-allocator-start allocator)))
  (define (arena-free allocator)
    (foreign-free (arena-allocator-start allocator))))
