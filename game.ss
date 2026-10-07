(import (chezscheme)
        (raylib raymath)
        (raylib intercept-alloc)
        (raylib raylib (0 3))
        (utils)
        (ftypes)
        (arena)
        (generational))

(define-ftype ShipEnemy
  (struct
    [position Vector-2]
    [angle double]))

(define-ftype Bullet
  (struct
    [position Vector-2]
    [velocity Vector-2]))

(import (prefix (coroutine) co:))

(define (wait secs)
  (define start-time (get-time))
  (let loop ([curr-time (get-time)])
    (unless (fl>= (fl- curr-time start-time) secs)
      (co:yield)
      (loop (get-time)))))

;; use barycentric coordinates (1-t,t) to
;; interpolate over a line:
;; x(t) = a_x + t * (b_x - a_x)
;; y(t) = a_y + t * (b_y - a_y)
(define (lerp start end t)
  (fl+ (fl* start (fl- 1. t)) (fl* end t)))

(define (move-to start-x start-y end-x end-y speed
                 interpolate-x interpolate-y setter)
  (define start-time (fl* (get-time) 1000.))
  ;; dx/dt = speed -> dt = dx / speed
  (define distance
    (sqrt (fl+ (flexpt (fl- end-x start-x) 2.)
               (flexpt (fl- end-y start-y) 2.))))
  (define end-time (fl+ start-time (fl/ distance speed)))
  (let loop ([curr-time (fl* (get-time) 1000.)])
    (let ([t (flmin 1. (fl/ (fl- curr-time start-time) end-time))])
      (unless (fl< (flabs (fl- t 1.)) 0.001)
        (let ([x (interpolate-x start-x end-x t)]
              [y (interpolate-y start-y end-y t)])
          (setter x y)
          (co:yield)
          (loop (fl* (get-time) 1000.)))))))

(define (line-to start-x start-y end-x end-y speed setter)
  (move-to start-x start-y end-x end-y speed lerp lerp setter))

(define (quad-bezier-to start-x start-y end-x end-y
                        control-x control-y speed setter)
  (define (bezier-x start end t)
    (lerp (lerp start control-x t) (lerp control-x end t) t))
  (define (bezier-y start end t)
    (lerp (lerp start control-y t) (lerp control-y end t) t))
  (move-to start-x start-y end-x end-y speed bezier-x bezier-y setter))

;; De Casteljau's algorithm for obtaining bezier curve through
;; repeated linear interpolations.
(define (cubic-bezier-to start-x start-y end-x end-y
                         control1-x control1-y control2-x control2-y
                         speed setter)
  (define (bezier-x start end t)
    (let* ([q0 (lerp start control1-x t)]
           [q1 (lerp control1-x control2-x t)]
           [q2 (lerp control2-x end t)]
           [r0 (lerp q0 q1 t)]
           [r1 (lerp q1 q2 t)])
      (lerp r0 r1 t)))
  (define (bezier-y start end t)
    (let* ([q0 (lerp start control1-y t)]
           [q1 (lerp control1-y control2-y t)]
           [q2 (lerp control2-y end t)]
           [r0 (lerp q0 q1 t)]
           [r1 (lerp q1 q2 t)])
      (lerp r0 r1 t)))
  (move-to start-x start-y end-x end-y speed bezier-x bezier-y setter))

(define-with-ftypes (move-rectangle [Rectangle rect] end-x end-y speed)
  (line-to rect.x rect.y end-x end-y speed
           (lambda (x y) (:= rect.x x) (:= rect.y y))))

(define-with-ftypes (curve-rectangle [Rectangle rect] end-x end-y
                                     control-x control-y speed)
  (quad-bezier-to rect.x rect.y end-x end-y control-x control-y speed
                  (lambda (x y) (:= rect.x x) (:= rect.y y))))

(define-with-ftypes (curve2-rectangle [Rectangle rect] end-x end-y
                                     control1-x control1-y
                                     control2-x control2-y
                                     speed)
  (cubic-bezier-to rect.x rect.y end-x end-y
                   control1-x control1-y control2-x control2-y speed
                   (lambda (x y) (:= rect.x x) (:= rect.y y))))

(define-generational ShipEnemy 100)
(define-generational Bullet 100)

(define-toplevel arena (make-arena 1000))
(define-toplevel rect1 (make-rectangle 50.0 50.0 100.0 100.0))
(define-toplevel rect2 (make-rectangle 100.0 100.0 100.0 100.0))
(define-toplevel task1
  (co:create-task! "move-rect1"
   (move-rectangle rect1 300. 300. .5)
   (wait 1.)
   (move-rectangle rect1 300. 0. .5)
   (curve2-rectangle rect1 0. 0. 200. 400. 100. 200. .5)))
(define-toplevel task2
  (co:create-task! "move-rect2"
   (move-rectangle rect2 500. 300. .63)
   (wait 1.)
   (move-rectangle rect2 500. 0. .5)
   (curve-rectangle rect2 0. 0. 300. 300. .5)))
(define-toplevel tasks (list task1 task2))
(define-toplevel rectangles (list rect1 rect2))
(define-toplevel ship-x 100.0)
(define-toplevel ship-y 60.0)
(define-toplevel ship-angle 0.)
(define-toplevel ship-velocity 0.2)
(define-toplevel ship-scale 2.)
(define-toplevel game-over? #f)
(define-toplevel enemies (foreign-alloc (ftype-sizeof ShipEnemy-manager)))
(define-toplevel bullets (foreign-alloc (ftype-sizeof Bullet-manager)))

(define (get-enemies) (make-ftype-pointer ShipEnemy-manager enemies))
(define (get-bullets) (make-ftype-pointer Bullet-manager bullets))

(define-with-ftypes (vector-2-cross-product [Vector-2 v1 v2])
  (fl- (fl* v1.x v2.y) (fl* v1.y v2.x)))

(define-with-ftypes (vector-2-subtract [Vector-2 v1 v2])
  (make-vector-2 (fl- v1.x v2.x) (fl- v1.y v2.y)))

(define-with-ftypes (drop-rectangle [Rectangle rect])
  (when (fl<= (fl+ rect.height rect.y)
              (fixnum->flonum (get-screen-height)))
    (:= rect.y (fl+ 1. rect.y))))

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

(define (spaceship-triangles-cont x y scale rad k)
  (let* ([v-top (get-rotated-vertex x y 0.0 (fl* scale -10.0) rad)]
         [v-left (get-rotated-vertex x y (fl* scale -6.0) (fl* scale 5.0) rad)]
         [v-mid (get-rotated-vertex x y 0.0 (fl* scale 0.0) rad)]
         [v-right (get-rotated-vertex x y (fl* scale 6.0) (fl* scale 5.0) rad)])
    (k v-top v-left v-mid)
    (k v-top v-mid v-right)))

(define (spaceship-draw x y scale rad color)
  (spaceship-triangles-cont x y scale rad
   (lambda (a b c) (draw-triangle-fixed a b c color))))

(define (spaceship-collides bullet-x bullet-y
                            ship-x ship-y ship-scale ship-rad)
  (let ([bullet (make-vector-2 bullet-x bullet-y)]
        [collides #f])
    (spaceship-triangles-cont ship-x ship-y ship-scale ship-rad
     (lambda (a b c)
       (set! collides
             (or collides (check-collision-point-triangle bullet a b c)))))
    collides))

(define-with-ftypes (bullet-update manager bullet-idx [Bullet bullet] dt)
  (:= bullet.position.x (fl+ bullet.position.x (fl* bullet.velocity.x dt)))
  (:= bullet.position.y (fl+ bullet.position.y (fl* bullet.velocity.y dt)))
  (when (or (fl> bullet.position.y (exact->inexact (get-screen-height)))
            (fl< bullet.position.y 0.))
    (let ([bullet-idx (Bullet-manager-index-at manager bullet-idx)])
      (Bullet-manager-remove! manager bullet-idx))))

(define (handle-keys dt)
  (when (is-key-down KEY_UP)
    (set! ship-y (fl- ship-y (fl* ship-velocity dt))))
  (when (is-key-down KEY_DOWN)
    (set! ship-y (fl+ ship-y (fl* ship-velocity dt))))
  (when (is-key-down KEY_LEFT)
    (set! ship-x (fl- ship-x (fl* ship-velocity dt))))
  (when (is-key-down KEY_RIGHT)
    (set! ship-x (fl+ ship-x (fl* ship-velocity dt)))))

(define (keep-in-bounds x y)
  (let ([height (fixnum->flonum (get-screen-height))]
        [width (fixnum->flonum (get-screen-width))])
    (when (fl>= ship-x width) (set! ship-x width))
    (when (fl<= ship-x 0.) (set! ship-x 0.))
    (when (fl<= ship-y 0.) (set! ship-y 0.))
    (when (fl>= ship-y height) (set! ship-y height))))

(define (populate-enemies enemies rows cols)
  (define ship-width (fl* ship-scale 12.))
  (define ship-width-plus-pad (fl+ ship-width 12.))
  (define col-size (fl* ship-width-plus-pad (exact->inexact cols)))
  (define start-x (fl- (fl/ (exact->inexact (get-screen-width)) 2.) (fl/ col-size 2.)))
  (with-auto-free
   (let loop ([x start-x] [y 60.] [row 0] [col 0])
     (unless (fx>= row rows)
       (if (fx>= col cols)
           (loop start-x (fl+ y 40.) (fx1+ row) 0)
           (let ([enemy (alloc-struct ShipEnemy)])
             (with-ftypes ([ShipEnemy enemy])
               (:= enemy.position.x x)
               (:= enemy.position.y y)
               (:= enemy.angle PI)
               (ShipEnemy-manager-add! enemies enemy)
               (loop (fl+ x ship-width-plus-pad) y row (fx1+ col)))))))))

(define (user-game-init)
  (define enemies (get-enemies))
  (define bullets (get-bullets))
  (display "Game logic initialized!\n")
  (set! ship-x (fl/ (exact->inexact (get-screen-width)) 2.))
  (set! ship-y (fl- (fl+ (exact->inexact (get-screen-height))) 20.))
  (ShipEnemy-manager-init! enemies)
  (Bullet-manager-init! bullets)
  (populate-enemies enemies 3 5))

(define last-fire-time (get-time))

(define (user-game-update)
  (eval-when (compile) (optimize-level 2))
  (define curr-time (get-time))
  (define dt (fl* (get-frame-time) 1000.))
  (define enemies (get-enemies))
  (define bullets (get-bullets))
  (arena-reset arena)
  (with-auto-free
   (set! tasks (co:step tasks))
   ;; (for-each drop-rectangle rectangles)
   (when (and (is-key-down KEY_SPACE)
              (fl> (fl- curr-time last-fire-time) .3))
     (set! last-fire-time curr-time)
     (let ([bullet (alloc-struct Bullet)])
       (with-ftypes ([Bullet bullet])
         (:= bullet.position.x ship-x)
         (:= bullet.position.y (fl- ship-y 40.))
         (:= bullet.velocity.x 0.)
         (:= bullet.velocity.y (fl- 0.5))
         (Bullet-manager-add! bullets bullet))))
   (handle-keys dt)
   (keep-in-bounds ship-x ship-y)
   (Bullet-manager-for-each
    (lambda-ftypes (bullet-idx [Bullet bullet])
      (define collided? #f)
      (when (spaceship-collides bullet.position.x bullet.position.y
                                ship-x ship-y ship-scale ship-angle)
        (set! game-over? #t))
      (ShipEnemy-manager-for-each
       (lambda-ftypes (enemy-idx [ShipEnemy enemy])
         (when (spaceship-collides bullet.position.x bullet.position.y
                                   enemy.position.x enemy.position.y
                                   ship-scale enemy.angle)
           (set! collided? #t)
           (ShipEnemy-manager-remove! enemies
                                      (ShipEnemy-manager-index-at enemies enemy-idx))))
       enemies)
      (if collided?
        (Bullet-manager-remove! bullets (Bullet-manager-index-at bullets bullet-idx))
        (bullet-update bullets bullet-idx bullet dt)))
    bullets)))

(define (user-game-draw)
  (eval-when (compile) (optimize-level 2))
  ;; Note: inside any loop we have to explicitly cast the pointer address
  ;; into the latest ftype pointer so that during a reload it always uses
  ;; the most recent definition of ftype.
  (define enemies (get-enemies))
  (define bullets (get-bullets))
  (with-ftypes [(ShipEnemy-manager enemies)
                (Bullet-manager bullets)]
    (with-auto-free
     (clear-background BLACK)
     (for-each (lambda (rect) (draw-rectangle-rec rect BLUE)) rectangles)
     (if game-over?
         (draw-text "GAME OVER"
                    (fx- (fx/ (get-screen-width) 2) 120)
                    (fx- (fx/ (get-screen-height) 2) 20)
                    40 WHITE)
         (spaceship-draw ship-x ship-y ship-scale ship-angle WHITE))
     (ShipEnemy-manager-for-each
      (lambda-ftypes (enemy-idx [ShipEnemy enemy])
        (define enemy-gen-idx (ShipEnemy-manager-index-at enemies enemy-idx))
        (spaceship-draw enemy.position.x enemy.position.y
                        ship-scale enemy.angle WHITE))
      enemies)
     (Bullet-manager-for-each 
      (lambda-ftypes (bullet-idx [Bullet bullet])
        (draw-circle-v &bullet.position 1.0 WHITE))
      bullets))))

(define (game-init) (safe-call user-game-init))
(define (game-update) (safe-call user-game-update))
(define (game-draw) (safe-call user-game-draw))

;; Local Variables:
;; eval: (put 'with-ftypes 'scheme-indent-function 1)
;; eval: (put 'lambda-ftypes 'scheme-indent-function 1)
;; eval: (put 'define-with-ftypes 'scheme-indent-function 1)
;; End:
