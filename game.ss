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
(define-ftype ShipEnemies (array 100 ShipEnemy))

(define-ftype ShipEnemySlot
  (struct
    [full boolean]
    [data (union
            [free (bits
                    [exists? unsigned 1]
                    [next unsigned 31])]
            [full (struct
                    [data ShipEnemy]
                    [generation unsigned-16])])]))

(define-ftype ShipEnemyManager
  (struct
    [slots (array 100 ShipEnemySlot)]
    [free-top (bits
                [exists? unsigned 1]
                [index unsigned 31])]
    [generation unsigned-16]
    [size unsigned-32]))

(define-record ShipEnemy-index
  [(unsigned-32 index)]
  [(unsigned-16 generation)])

(define-syntax print-ftype-fields
  (lambda (x)
    (syntax-case x ()
      [(_ ftype-id)
       #'(let* ([dummy-ptr (make-ftype-pointer ftype-id 0)]
                [struct-sexpr (ftype-pointer->sexpr dummy-ptr)])
           (map car (cdr struct-sexpr)))])))

(define (add-ship-enemy manager actor)
  (with-ftypes
   ([ShipEnemyManager manager]
    [ShipEnemy actor])
   ;; todo:
   ;; 1. Get free top from manager
   ;; 2. If no open slot, throw exception
   ;; 3. Set manager's free top to the slot's free top
   ;; 4. Set slot data at index (with generation = manager's generation)
   ;; 5. Increment manager's size
   (if manager.free-top.exists?
       (begin
         (let ([slot manager.free-top.index])
           (:= manager.free-top.exists? manager.slots.slot.data.free.exists?)
           (:= manager.free-top.index manager.slots.slot.data.free.next)
           (::= &manager.slots.slot.data.full.data actor)
           (:= manager.slots.slot.data.full.generation manager.generation)
           (:= manager.size (fx+ manager.size 1))))
       (raise "no free top"))))

(define (remove-ship-enemy manager index)
  (with-ftypes
   ([ShipEnemyManager manager])
   ;; todo:
   ;; 1. from the manager get the slot at the index's index field
   ;; 2. if the slot is either free or full with different generation,
   ;; throw exception
   ;; 3. set the slot to free with the next being the manager's free top.
   ;; 4. set the manager's free top to the slot
   ;; 5. increment the generation and decrement the size
   '()))

(define (get-ship-enemy manager index)
  (with-ftypes
   ([ShipEnemyManager manager])
   ;; todo:
   ;; get the slot at the index's index if the
   ;; generation of the slot == generation of the index
   '()))

(define-toplevel rectangles
  (list
   (make-rectangle 50.0 50.0 100.0 100.0)
   (make-rectangle 100.0 100.0 100.0 100.0)))
(define-toplevel ship-x 100.0)
(define-toplevel ship-y 60.0)
(define-toplevel ship-angle 0.)
(define-toplevel ship-velocity 0.2)
(define-toplevel enemies (foreign-alloc (ftype-sizeof ShipEnemies)))
(define-toplevel enemies-size 0)

(define (get-enemies) (make-ftype-pointer ShipEnemies enemies))

(define (make-enemy x y angle)
  (define size enemies-size)
  (define enemies (get-enemies))
  (with-ftypes
   [(ShipEnemies enemies)]
   (:= enemies.size.x x)
   (:= enemies.size.y y)
   (:= enemies.size.angle angle)
   (set! enemies-size (fx+ enemies-size 1))))

(define last-time (get-time))

(define (vector-2-cross-product v1 v2)
  (with-ftypes [(Vector-2 v1 v2)]
               (fl- (fl* v1.x v2.y) (fl* v1.y v2.x))))

(define (vector-2-subtract v1 v2)
  (with-ftypes [(Vector-2 v1 v2)]
               (make-vector-2 (fl- v1.x v2.x) (fl- v1.y v2.y))))

(define (drop-rectangle rect)
  (with-ftypes [(Rectangle rect)]
               (when (<= (fl+ rect.height rect.y) (get-screen-height))
                 (:= rect.y (fl+ 1. rect.y)))))

(define (draw-triangle-fixed v1 v2 v3 color)
  (let* ([edge1 (vector-2-subtract v2 v1)]
         [edge2 (vector-2-subtract v3 v1)]
         [cross-z (vector-2-cross-product edge1 edge2)])
    (if (> cross-z 0.0)
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
  (display "Game logic initialized!\n")
  (set! ship-x (fl/ (exact->inexact (get-screen-width)) 2.))
  (set! ship-y (fl- (fl+ (exact->inexact (get-screen-height))) 20.)))

(define (user-game-update)
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
   [(ShipEnemies enemies)]
   (with-auto-free
    (clear-background BLACK)
    (for-each (lambda (rect) (draw-rectangle-rec rect BLUE)) rectangles)
    (spaceship-draw ship-x ship-y 3. WHITE ship-angle)
    (do ([i 0 (fx+ i 1)])
        ((fx>= i enemies-size))
      (spaceship-draw enemies.i.x enemies.i.y 3. WHITE enemies.i.angle)))))

(define (game-init) (safe-call user-game-init))
(define (game-update) (safe-call user-game-update))
(define (game-draw) (safe-call user-game-draw))
