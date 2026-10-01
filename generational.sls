(library (generational)
  (export define-generational)
  (import (chezscheme))

  (meta define (syntax-prepend str stx)
    (datum->syntax
     stx
     (string->symbol
      (string-append str (symbol->string (syntax->datum stx))))))

  (meta define (syntax-append stx str)
    (datum->syntax
     stx
     (string->symbol
      (string-append (symbol->string (syntax->datum stx)) str))))

  (define-syntax (define-generational stx)
    (syntax-case stx ()
      [(_ actor-type capacity)
       (with-syntax ([actor-index-type (syntax-append #'actor-type "-index")]
                     [actor-slot-type (syntax-append #'actor-type "-slot")]
                     [actor-manager-type
                      (syntax-append #'actor-type "-manager")])
         (with-syntax ([make-actor-index
                        (syntax-prepend "make-" #'actor-index-type)]
                       [actor-index-index
                        (syntax-append #'actor-index-type "-index")]
                       [actor-index-generation
                        (syntax-append #'actor-index-type "-generation")]
                       [actor-manager-init
                        (syntax-append #'actor-manager-type "-init!")]
                       [actor-manager-add
                        (syntax-append #'actor-manager-type "-add!")]
                       [actor-manager-remove
                        (syntax-append #'actor-manager-type "-remove!")]
                       [actor-manager-index-at
                        (syntax-append #'actor-manager-type "-index-at")]
                       [actor-manager-for-each
                        (syntax-append #'actor-manager-type "-for-each")])
           #'(module
                 (export actor-manager-type
                         actor-manager-init
                         actor-manager-add
                         actor-manager-remove
                         actor-manager-index-at
                         actor-manager-for-each)
               (import (chezscheme) (ftypes))

               (define-record actor-index-type
                 ([unsigned-32 index]
                  [unsigned-16 generation]))
               (define-ftype actor-slot-type
                 (struct
                   [full? boolean]
                   [data (union
                           [free (bits
                                   [exists? unsigned 1]
                                   [next unsigned 31])]
                           [full (struct
                                   [data actor-type]
                                   [generation unsigned-16])])]))
               (define-ftype actor-manager-type
                 (struct
                   [slots (array capacity actor-slot-type)]
                   [free-top (bits
                               [exists? unsigned 1]
                               [next unsigned 31])]
                   [generation unsigned-16]
                   [size unsigned-32]))
               (define (actor-manager-init manager)
                 (with-ftypes
                  ([actor-manager-type manager])
                  ;; initialize each index to be free
                  ;; and point to the next index.
                  (for-each
                   (lambda (i)
                     (:= manager.slots.i.full? #f)
                     (if (fx< (fx+ i 1) capacity)
                         (begin
                           (:= manager.slots.i.data.free.exists? 1)
                           (:= manager.slots.i.data.free.next (fx+ i 1)))
                         (:= manager.slots.i.data.free.exists? 0)))
                   (iota capacity))
                  (:= manager.free-top.exists? 1)
                  (:= manager.free-top.next 0)
                  (:= manager.generation 0)
                  (:= manager.size 0)))
               (define (actor-manager-add manager actor)
                 (with-ftypes
                  ([actor-manager-type manager]
                   [actor-type actor])
                  (if manager.free-top.exists?
                      (let* ([slot manager.free-top.next]
                             [index
                              (make-actor-index slot manager.generation)])
                        (:= manager.free-top.exists?
                            manager.slots.slot.data.free.exists?)
                        (:= manager.free-top.next
                            manager.slots.slot.data.free.next)
                        (:= manager.slots.slot.full? #t)
                        (::= &manager.slots.slot.data.full.data actor)
                        (:= manager.slots.slot.data.full.generation
                            manager.generation)
                        (:= manager.size (fx+ manager.size 1))
                        index)
                      (raise "no free top"))))
               (define (actor-manager-index-at manager i)
                 (with-ftypes
                  ([actor-manager-type manager])
                  (if (and manager.slots.i.full?
                           (fx= manager.slots.i.data.full.generation
                                manager.generation))
                      (make-actor-index
                       i manager.slots.i.data.full.generation)
                      (raise "empty or invalid generation slot at index"))))
               (define (actor-manager-remove manager index)
                 (with-ftypes
                  ([actor-manager-type manager])
                  (let ([index (actor-index-index index)]
                        [generation (actor-index-generation index)])
                    (if manager.slots.index.full?
                        (if (fx= manager.slots.index.data.full.generation
                                 generation)
                            (begin
                              (:= manager.slots.index.full? #f)
                              (:= manager.slots.index.data.free.exists?
                                  manager.free-top.exists?)
                              (:= manager.slots.index.data.free.next
                                  manager.free-top.next)
                              (:= manager.free-top.exists? 1)
                              (:= manager.free-top.next index)
                              (:= manager.generation
                                  (fx+ manager.generation 1))
                              (:= manager.size (fx- manager.size 1)))
                            (raise "different generation"))
                        (raise "slot is free")))))
               (define (actor-manager-for-each f manager)
                 (with-ftypes
                  ([actor-manager-type manager])
                  (do ([i 0 (fx+ i 1)])
                      ((fx>= i manager.size))
                    (when (and manager.slots.i.full?
                               (fx= manager.slots.i.data.full.generation
                                    manager.generation))
                      (f i &manager.slots.i.data.full.data))))))))])))
