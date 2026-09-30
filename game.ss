(import (chezscheme)
        (raylib raymath)
        (raylib intercept-alloc)
        (raylib raylib (0 3))
        (ftypes))

(define-syntax define-toplevel
  (lambda (stx)
    (syntax-case stx ()
      [(_ id value) (identifier? #'id)
       #'(unless (top-level-bound? (quote id))
           (define-top-level-value (quote id) value))])))

(define (safe-call thunk)
  (guard (condition
          [else
           (display "[Scheme Error]: ")
           (display-condition condition)
           (newline)])
    (thunk)))

(define-ftype ShipEnemy
  (struct
    [x double]
    [y double]
    [angle double]))

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
    [(_ actor-type)
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
                 [slots (array 100 actor-slot-type)]
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
                   (if (fx< (fx+ i 1) 100)
                       (begin
                         (:= manager.slots.i.data.free.exists? 1)
                         (:= manager.slots.i.data.free.next (fx+ i 1)))
                       (:= manager.slots.i.data.free.exists? 0)))
                 (iota 100))
                (:= manager.free-top.exists? 1)
                (:= manager.free-top.next 0)
                (:= manager.generation 0)
                (:= manager.size 0)))
             (define (actor-manager-add manager actor)
               (with-ftypes
                ([actor-manager-type manager]
                 [actor-type actor])
                (if manager.free-top.exists?
                    (begin
                      (let ([slot manager.free-top.next])
                        (:= manager.free-top.exists?
                            manager.slots.slot.data.free.exists?)
                        (:= manager.free-top.next
                            manager.slots.slot.data.free.next)
                        (:= manager.slots.slot.full? #t)
                        (::= &manager.slots.slot.data.full.data actor)
                        (:= manager.slots.slot.data.full.generation
                            manager.generation)
                        (:= manager.size (fx+ manager.size 1))))
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
                    (f i &manager.slots.i.data.full.data))))))))]))

(define-generational ShipEnemy)

(define-syntax print-ftype-fields
  (lambda (x)
    (syntax-case x ()
      [(_ ftype-id)
       #'(let* ([dummy-ptr (make-ftype-pointer ftype-id 0)]
                [struct-sexpr (ftype-pointer->sexpr dummy-ptr)])
           (map car (cdr struct-sexpr)))])))

(define-toplevel rectangles
  (list
   (make-rectangle 50.0 50.0 100.0 100.0)
   (make-rectangle 100.0 100.0 100.0 100.0)))
(define-toplevel ship-x 100.0)
(define-toplevel ship-y 60.0)
(define-toplevel ship-angle 0.)
(define-toplevel ship-velocity 0.2)
(define-toplevel enemies (foreign-alloc (ftype-sizeof ShipEnemy-manager)))

(define (get-enemies) (make-ftype-pointer ShipEnemy-manager enemies))

(define last-time (get-time))

(define (vector-2-cross-product v1 v2)
  (with-ftypes [(Vector-2 v1 v2)]
               (fl- (fl* v1.x v2.y) (fl* v1.y v2.x))))

(define (vector-2-subtract v1 v2)
  (with-ftypes [(Vector-2 v1 v2)]
               (make-vector-2 (fl- v1.x v2.x) (fl- v1.y v2.y))))

(define (drop-rectangle rect)
  (with-ftypes [(Rectangle rect)]
               (when (fl<= (fl+ rect.height rect.y)
                           (fixnum->flonum (get-screen-height)))
                 (:= rect.y (fl+ 1. rect.y)))))

(define (draw-triangle-fixed v1 v2 v3 color)
  (let* ([edge1 (vector-2-subtract v2 v1)]
         [edge2 (vector-2-subtract v3 v1)]
         [cross-z (vector-2-cross-product edge1 edge2)])
    (if (fl> cross-z 0.0)
        (draw-triangle v1 v3 v2 color)
        (draw-triangle v1 v2 v3 color))))

(define (get-rotated-vertex cx cy local-x local-y rad)
  (let* ([cos-r (flcos rad)]
         [sin-r (flsin rad)]
         [rot-x (fl- (fl* local-x cos-r) (fl* local-y sin-r))]
         [rot-y (fl+ (fl* local-x sin-r) (fl* local-y cos-r))])
    (make-vector-2 (fl+ cx rot-x) (fl+ cy rot-y))))

(define (spaceship-draw x y scale color rad)
  (let* ([v-top (get-rotated-vertex x y 0.0 (fl* scale -10.0) rad)]
         [v-left (get-rotated-vertex x y (fl* scale -6.0) (fl* scale 5.0) rad)]
         [v-mid (get-rotated-vertex x y 0.0 (fl* scale 0.0) rad)]
         [v-right (get-rotated-vertex x y (fl* scale 6.0) (fl* scale 5.0) rad)])
    (draw-triangle-fixed v-top v-left v-mid color)
    (draw-triangle-fixed v-top v-mid v-right color)))

(define (handle-keys dt)
  (when (is-key-down KEY_UP)
    (set! ship-y (fl+ ship-y (fl* ship-velocity dt))))
  (when (is-key-down KEY_DOWN)
    (set! ship-y (fl- ship-y (fl* ship-velocity dt))))
  (when (is-key-down KEY_LEFT)
    (set! ship-x (fl+ ship-x (fl* ship-velocity dt))))
  (when (is-key-down KEY_RIGHT)
    (set! ship-x (fl- ship-x (fl* ship-velocity dt)))))

(define (keep-in-bounds x y)
  (let ([height (fixnum->flonum (get-screen-height))]
        [width (fixnum->flonum (get-screen-width))])
    (when (fl>= ship-x width) (set! ship-x width))
    (when (fl<= ship-x 0.) (set! ship-x 0.))
    (when (fl<= ship-y 0.) (set! ship-y 0.))
    (when (fl>= ship-y height) (set! ship-y height))))

(define (user-game-init)
  (define enemies (get-enemies))
  (display "Game logic initialized!\n")
  (set! ship-x (fl/ (exact->inexact (get-screen-width)) 2.))
  (set! ship-y (fl- (fl+ (exact->inexact (get-screen-height))) 20.))
  (ShipEnemy-manager-init! (get-enemies))
  (with-auto-free
   (do ([x 60. (fl+ x 40.)])
       ((fl>= x (exact->inexact (get-screen-width))))
     (let ([enemy (make-ftype-pointer ShipEnemy (foreign-alloc (ftype-sizeof ShipEnemy)))])
       (with-ftypes ([ShipEnemy enemy])
                    (:= enemy.x x)
                    (:= enemy.y 60.)
                    (:= enemy.angle PI)
                    (ShipEnemy-manager-add! enemies enemy))))))

(define (user-game-update)
  (eval-when (compile) (optimize-level 2))
  (define curr-time (get-time))
  (define dt (fl* 1000. (fl- last-time curr-time)))
  (set! last-time curr-time)
  (for-each drop-rectangle rectangles)
  (handle-keys dt)
  (keep-in-bounds ship-x ship-y))

(define (user-game-draw)
  (eval-when (compile) (optimize-level 2))
  ;; Note: inside any loop we have to explicitly cast the pointer address
  ;; into the latest ftype pointer so that during a reload it always uses
  ;; the most recent definition of ftype.
  (define enemies (get-enemies))
  (with-ftypes
   [(ShipEnemyManager enemies)]
   (with-auto-free
    (clear-background BLACK)
    (for-each (lambda (rect) (draw-rectangle-rec rect BLUE)) rectangles)
    (spaceship-draw ship-x ship-y 2. WHITE ship-angle)
    (ShipEnemy-manager-for-each
     (lambda (enemy-idx enemy)
       (define enemy-gen-idx (ShipEnemy-manager-index-at enemies enemy-idx))
       (with-ftypes ([ShipEnemy enemy])
                    (spaceship-draw enemy.x enemy.y 2. WHITE enemy.angle)))
     enemies))))

(define (game-init) (safe-call user-game-init))
(define (game-update) (safe-call user-game-update))
(define (game-draw) (safe-call user-game-draw))
