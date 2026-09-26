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

(define-toplevel rectangles
  (list
   (make-rectangle 50.0 50.0 100.0 100.0)
   (make-rectangle 100.0 100.0 100.0 100.0)))

(define (drop-rectangle rect)
  (with-ftypes [(Rectangle rect)]
               (when (<= (fl+ rect.height rect.y) (get-screen-height))
                 (:= rect.y (fl+ 1. rect.y)))))

(define-toplevel ship-x 60.)
(define-toplevel ship-y 100.)
(define-toplevel ship-angle 0.)
(define-toplevel ship-velocity-x 5.)
(define-toplevel ship-velocity-y 5.)

(define (user-game-init)
  (display "Game logic initialized!\n"))

(define (user-game-update)
  (for-each drop-rectangle rectangles)
  (set! ship-angle (fl+ ship-angle 0.02))
  (set! ship-x (fl+ ship-x ship-velocity-x))
  (set! ship-y (fl+ ship-y ship-velocity-y)))

(define (vector-2-cross-product v1 v2)
  (with-ftypes [(Vector-2 v1 v2)]
               (fl- (fl* v1.x v2.y) (fl* v1.y v2.x))))

(define (vector-2-subtract v1 v2)
  (with-ftypes [(Vector-2 v1 v2)]
               (make-vector-2 (fl- v1.x v2.x) (fl- v1.y v2.y))))

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

(define (user-game-draw)
  (eval-when (compile) (optimize-level 2))
  (with-auto-free
   (clear-background BLACK)
   (for-each (lambda (rect) (draw-rectangle-rec rect BLUE)) rectangles)
   (let ([height (fixnum->flonum (get-screen-height))]
         [width (fixnum->flonum (get-screen-width))])
     (cond
      [(fl>= ship-x width)
       (set! ship-x width)
       (set! ship-velocity-x (fl- ship-velocity-x))]
      [(fl<= ship-x 0.)
       (set! ship-x 0.)
       (set! ship-velocity-x (fl- ship-velocity-x))]
      [(fl<= ship-y 0.)
       (set! ship-y 0.)
       (set! ship-velocity-y (fl- ship-velocity-y))]
      [(fl>= ship-y height)
       (set! ship-y height)
       (set! ship-velocity-y (fl- ship-velocity-y))]))
   (spaceship-draw ship-x ship-y 3. WHITE ship-angle)))

(define (game-init) (safe-call user-game-init))
(define (game-update) (safe-call user-game-update))
(define (game-draw) (safe-call user-game-draw))
