(import (chezscheme)
        (raylib raymath)
        (raylib intercept-alloc)
        (raylib raylib (0 3))
        (utils)
        (ftypes)
        (arena)
        (generational))

(define game-rng (make-pseudo-random-generator))
(let* ([t (current-time)]
       [seed-val (+ (time-second t) (time-nanosecond t))])
  (pseudo-random-generator-seed! game-rng
                                 (+ 1 (modulo seed-val (- (expt 2 32) 1)))))

(define SHIP-ENEMY-NORMAL 0)
(define SHIP-ENEMY-STRAFING 1)

(define-ftype ShipEnemy
  (struct
    [position Vector-2]
    [angle double]
    [state integer-8]))

(define-ftype Bullet
  (struct
    [position Vector-2]
    [velocity Vector-2]))

(define-ftype Star
  (struct
    [position Vector-2]
    [depth integer-8]
    [color Color]))

(define-generational ShipEnemy 100)
(define-generational Bullet 100)
(define-generational Star 100)

(define-toplevel *shipenemy-tasks* (make-eqv-hashtable))
(define-toplevel *game-time* 0.)

(import (prefix (coroutine) co:))

(define (wait secs)
  (define start-time *game-time*)
  (let loop ([curr-time *game-time*])
    (unless (fl>= (fl- curr-time start-time) secs)
      (co:yield)
      (loop *game-time*))))

;; use barycentric coordinates (1-t,t) to
;; interpolate over a line:
;; x(t) = a_x + t * (b_x - a_x)
;; y(t) = a_y + t * (b_y - a_y)
(define (lerp start end t)
  (fl+ (fl* start (fl- 1. t)) (fl* end t)))

(define (move-to start-x start-y end-x end-y speed
                 interpolate-x interpolate-y setter)
  (define start-time (fl* *game-time* 1000.))
  ;; dx/dt = speed -> dt = dx / speed
  (define distance
    (sqrt (fl+ (flexpt (fl- end-x start-x) 2.)
               (flexpt (fl- end-y start-y) 2.))))
  (define duration (fl/ distance speed))
  (let loop ([curr-time (fl* *game-time* 1000.)])
    (let ([t (flmin 1. (fl/ (fl- curr-time start-time) duration))])
      (unless (fl< (flabs (fl- t 1.)) 0.001)
        (let ([x (interpolate-x start-x end-x t)]
              [y (interpolate-y start-y end-y t)])
          (setter x y)
          (co:yield)
          (loop (fl* *game-time* 1000.)))))))

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

(define-with-ftypes (ShipEnemy-move [ShipEnemy enemy] end-x end-y speed)
  (line-to enemy.position.x enemy.position.y end-x end-y speed
           (lambda (x y) (:= enemy.position.x x) (:= enemy.position.y y))))

(define-with-ftypes (ShipEnemy-curve [ShipEnemy enemy]
                                     end-x end-y control-x control-y speed)
  (quad-bezier-to
   enemy.position.x enemy.position.y
   end-x end-y control-x control-y speed
   (lambda (x y) (:= enemy.position.x x) (:= enemy.position.y y))))

(define-with-ftypes (ShipEnemy-curve2 [ShipEnemy enemy] end-x end-y
                                      control1-x control1-y
                                      control2-x control2-y
                                      speed)
  (cubic-bezier-to
   enemy.position.x enemy.position.y end-x end-y
   control1-x control1-y control2-x control2-y speed
   (lambda (x y) (:= enemy.position.x x) (:= enemy.position.y y))))

(define-toplevel tasks '())
(define-syntax (push-task! stx)
  (syntax-case stx ()
    [(_ name body* ...) (identifier? #'name)
     (with-syntax ([name-str
                    (datum->syntax #'name
                                   (symbol->string (syntax->datum #'name)))])
       #'(set! tasks (cons (co:create-task! name-str body* ...) tasks)))]
    [(_ body* ...)
     (with-syntax ([task
                    (datum->syntax
                     #'here
                     (string-append "task-" (symbol->string (gensym))))])
       #'(set! tasks (cons (co:create-task! task body* ...) tasks)))]))

(define-toplevel arena (make-arena 1000))
(define-toplevel ship-color WHITE)
(define-toplevel ship-x 100.0)
(define-toplevel ship-y 60.0)
(define-toplevel ship-angle 0.)
(define-toplevel ship-velocity 0.2)
(define-toplevel ship-scale 2.)
(define-toplevel game-over? #f)
(define-toplevel enemies (foreign-alloc (ftype-sizeof ShipEnemy-manager)))
(define-toplevel bullets (foreign-alloc (ftype-sizeof Bullet-manager)))
(define-toplevel stars (foreign-alloc (ftype-sizeof Star-manager)))
(define-toplevel game-state 'start)
(define-toplevel game-level 1)

(define (get-enemies) (make-ftype-pointer ShipEnemy-manager enemies))
(define (get-bullets) (make-ftype-pointer Bullet-manager bullets))
(define (get-stars) (make-ftype-pointer Star-manager stars))

(define-with-ftypes (vector-2-cross-product [Vector-2 v1 v2])
  (fl- (fl* v1.x v2.y) (fl* v1.y v2.x)))

(define-with-ftypes (vector-2-subtract [Vector-2 v1 v2])
  (make-vector-2 (fl- v1.x v2.x) (fl- v1.y v2.y)))

(define-with-ftypes (drop-star [Star star])
  (if (fl<= star.position.y (fixnum->flonum (get-screen-height)))
      (:= star.position.y (fl+ star.position.y
                               (fl/ (exact->inexact (fx1+ star.depth)) 2.)))
      (begin
        (:= star.position.y 0.)
        (:= star.position.x
            (exact->inexact
             (pseudo-random-generator-next! game-rng (get-screen-width)))))))

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
         [v-left
          (get-rotated-vertex x y (fl* scale -6.0) (fl* scale 5.0) rad)]
         [v-mid (get-rotated-vertex x y 0.0 (fl* scale 0.0) rad)]
         [v-right
          (get-rotated-vertex x y (fl* scale 6.0) (fl* scale 5.0) rad)])
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

(define (populate-stars stars)
  (with-auto-free
   (do ([i 0 (fx1+ i)])
       ((>= i 100))
     (let* ([x (pseudo-random-generator-next! game-rng (get-screen-width))]
            [y (pseudo-random-generator-next! game-rng (get-screen-height))]
            [depth (fx1+ (pseudo-random-generator-next! game-rng 3))]
            [star (alloc-struct Star)]
            [color (case depth
                     [3 (make-color 40 55 90 100)]
                     [2 (make-color 120 160 230 180)]
                     [1 (make-color 255 255 255 255)])])
       (with-ftypes ([Star star] [Color color])
         (:= star.position.x (exact->inexact x))
         (:= star.position.y (exact->inexact y))
         (:= star.depth depth)
         (::= &star.color color)
         (Star-manager-add! stars star))))))

(define (update-stars stars)
  (Star-manager-for-each (lambda (star-idx star) (drop-star star)) stars))

(define (populate-enemies enemies rows cols)
  (define ship-width (fl* ship-scale 12.))
  (define ship-width-plus-pad (fl+ ship-width 12.))
  (define col-size (fl* ship-width-plus-pad (exact->inexact cols)))
  (define start-x
    (fl- (fl/ (exact->inexact (get-screen-width)) 2.) (fl/ col-size 2.)))
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
               (:= enemy.state SHIP-ENEMY-NORMAL)
               (ShipEnemy-manager-add! enemies enemy)
               (loop (fl+ x ship-width-plus-pad) y row (fx1+ col)))))))))

(define-with-ftypes (ShipEnemy-normal-x [ShipEnemy enemy] curr-time)
  (if (fx= enemy.state SHIP-ENEMY-NORMAL)
      (fl+ enemy.position.x (fl* (flsin curr-time) 10.))
      enemy.position.x))

(define-with-ftypes (shipenemy-task [ShipEnemy enemy] time)
  (let* ([saved-position-x enemy.position.x]
         [saved-position-y enemy.position.y]
         [curr-position-x (ShipEnemy-normal-x enemy time)]
         [control-x (if (fl< saved-position-x
                             (fl/ (exact->inexact (get-screen-width)) 2.))
                        0.
                        (exact->inexact (get-screen-width)))]
         [control-y (fl/ (exact->inexact (get-screen-height)) 3.)])
    (:= enemy.state SHIP-ENEMY-STRAFING)
    (ShipEnemy-curve enemy
                     curr-position-x
                     (fl+ (exact->inexact (get-screen-height)) 60.)
                     control-x control-y
                     .1)
    (ShipEnemy-curve enemy
                     saved-position-x saved-position-y
                     (if (fl= control-x 0.)
                         (exact->inexact (get-screen-width))
                         0.)
                     control-y
                     .1)
    (:= enemy.angle PI)
    (:= enemy.state SHIP-ENEMY-NORMAL)))

(define (load-level level)
  (define enemies (get-enemies))
  (define bullets (get-bullets))
  (display "Game logic initialized!\n")
  (set! ship-x (fl/ (exact->inexact (get-screen-width)) 2.))
  (set! ship-y (fl- (fl+ (exact->inexact (get-screen-height))) 20.))
  (set! game-over? #f)
  (ShipEnemy-manager-init! enemies)
  (Bullet-manager-init! bullets)
  (populate-enemies enemies 3 5))

(define (user-game-init)
  (define stars (get-stars))
  (ShipEnemy-manager-init! (get-enemies))
  (Bullet-manager-init! (get-bullets))
  (Star-manager-init! stars)
  (populate-stars stars)
  (set! ship-x (fl/ (exact->inexact (get-screen-width)) 2.))
  (set! ship-y (fl- (fl+ (exact->inexact (get-screen-height))) 20.)))

(define last-fire-time *game-time*)

(define (game-state-update)
  (eval-when (compile) (optimize-level 2))
  (define dt (fl* (get-frame-time) 1000.))
  (define enemies (get-enemies))
  (define bullets (get-bullets))
  (define stars (get-stars))
  (set! *game-time* (fl+ *game-time* (get-frame-time)))
  (arena-reset arena)
  (set! tasks (co:step tasks))
  (unless game-over?
    (when (and (is-key-down KEY_SPACE)
               (fl> (fl- *game-time* last-fire-time) .3))
      (set! last-fire-time *game-time*)
      (with-auto-free
       (let ([bullet (alloc-struct Bullet)])
         (with-ftypes ([Bullet bullet])
           (:= bullet.position.x ship-x)
           (:= bullet.position.y (fl- ship-y 40.))
           (:= bullet.velocity.x 0.)
           (:= bullet.velocity.y (fl- 0.5))
           (Bullet-manager-add! bullets bullet)))))
    (handle-keys dt)
    (keep-in-bounds ship-x ship-y))
  (ShipEnemy-manager-for-each
   (lambda-ftypes (enemy-idx [ShipEnemy enemy])
     (exclusive-cond
       [(fx= enemy.state SHIP-ENEMY-NORMAL)
        ;; (level - 1) in 3000 chance to be promoted to strafing.
        (if (fx< (pseudo-random-generator-next! game-rng 3000) (fx1- game-level))
            (let* ([task-name (string-append "task-"
                                             (symbol->string (gensym)))]
                   [task (co:create-task! task-name
                                          (shipenemy-task enemy *game-time*))])
              (hashtable-set! *shipenemy-tasks* enemy-idx (list task))
              (:= enemy.state SHIP-ENEMY-STRAFING))
            (when (and (fl> enemy.position.y (fl* 40. 3.))
                       (fx< (pseudo-random-generator-next! game-rng 1000)
                            (fx* 5 game-level)))
              (with-auto-free
               (let ([bullet (alloc-struct Bullet)])
                 (with-ftypes ([Bullet bullet])
                   (:= bullet.position.x (ShipEnemy-normal-x enemy *game-time*))
                   (:= bullet.position.y (fl+ enemy.position.y 40.))
                   (:= bullet.velocity.x 0.)
                   (:= bullet.velocity.y 0.5)
                   (Bullet-manager-add! bullets bullet))))))]
       [(fx= enemy.state SHIP-ENEMY-STRAFING)
        (let* ([dx (fl- ship-x enemy.position.x)]
               [dy (fl- ship-y enemy.position.y)]
               [angle (atan dy dx)])
          (:= enemy.angle (fl+ angle (fl/ PI 2.)))
          ;; Advance strafing coroutine.
          (let ([tasks (hashtable-ref *shipenemy-tasks* enemy-idx #f)])
            (when tasks
              (hashtable-set! *shipenemy-tasks* enemy-idx (co:step tasks))))
          (when (fx< (pseudo-random-generator-next! game-rng 1000) (fx* 5 game-level))
            (with-auto-free
             (let ([bullet (alloc-struct Bullet)])
               (with-ftypes ([Bullet bullet])
                 (:= bullet.position.x
                     (fl+ enemy.position.x (fl* (flcos angle) 40.)))
                 (:= bullet.position.y
                     (fl+ enemy.position.y (fl* (flsin angle) 40.)))
                 (:= bullet.velocity.x (fl* (flcos angle) .5))
                 (:= bullet.velocity.y (fl* (flsin angle) .5))
                 (Bullet-manager-add! bullets bullet))))))]))
   enemies)
  (Star-manager-for-each (lambda (star-idx star) (drop-star star)) stars)
  (Bullet-manager-for-each
   (lambda-ftypes (bullet-idx [Bullet bullet])
     (define collided? #f)
     (when (spaceship-collides bullet.position.x bullet.position.y
                               ship-x ship-y ship-scale ship-angle)
       (set! game-over? #t))
     (ShipEnemy-manager-for-each
      (lambda-ftypes (enemy-idx [ShipEnemy enemy])
        (when (spaceship-collides bullet.position.x bullet.position.y
                                  (ShipEnemy-normal-x enemy *game-time*)
                                  enemy.position.y
                                  ship-scale enemy.angle)
          (set! collided? #t)
          (ShipEnemy-manager-remove!
           enemies (ShipEnemy-manager-index-at enemies enemy-idx))
          (when (fx= (ShipEnemy-manager-size enemies) 0)
            (set! game-state 'level-complete))))
      enemies)
     (if collided?
         (Bullet-manager-remove!
          bullets (Bullet-manager-index-at bullets bullet-idx))
         (bullet-update bullets bullet-idx bullet dt)))
   bullets)
  (when (and (is-key-pressed KEY_ENTER) game-over?)
    (set! game-level 1)
    (load-level game-level)))

(define (user-game-update)
  (eval-when (compile) (optimize-level 2))
  (define dt (fl* (get-frame-time) 1000.))
  (case game-state
    [start
     (update-stars (get-stars))
     (when (is-key-pressed KEY_ENTER)
       (load-level game-level)
       (set! game-state 'running))]
    [running (game-state-update)]
    [level-complete
     (update-stars (get-stars))
     (handle-keys dt)
     (when (is-key-pressed KEY_ENTER)
       (set! game-level (fx1+ game-level))
       (load-level game-level)
       (set! game-state 'running))]))

(define draw-text-middle
  (rec draw-text-middle
    (case-lambda
      [(text size color) (draw-text-middle text size color 0 0)]
      [(text size color offset-x offset-y)
       (define middle-x (fx/ (get-screen-width) 2))
       (define middle-y (fx/ (get-screen-height) 2))
       (define text-size-x (measure-text text size))
       ;; text-size + 2x = width
       ;; start-x = (width - text-size) / 2
       (define start-x (fx/ (fx- (get-screen-width) text-size-x) 2))
       ;; size + 2y = height
       ;; start-y = (height - size) / 2
       (define start-y (fx/ (fx- (get-screen-height) size) 2))
       (draw-text text (fx+ start-x offset-x) (fx+ start-y offset-y) size color)])))

(define (user-game-draw)
  (eval-when (compile) (optimize-level 2))
  ;; Note: inside any loop we have to explicitly cast the pointer address
  ;; into the latest ftype pointer so that during a reload it always uses
  ;; the most recent definition of ftype.
  (define enemies (get-enemies))
  (define bullets (get-bullets))
  (define stars (get-stars))
  (with-auto-free
   (clear-background BLACK)
   (Star-manager-for-each
    (lambda-ftypes (star-idx [Star star])
      (case star.depth
        [1 (draw-circle-v &star.position 1.5 &star.color)]
        [2 (draw-circle-v &star.position 1.2 &star.color)]
        [3 (draw-pixel-v &star.position &star.color)]))
    stars)
   (case game-state
     [start
      (draw-text-middle "CHEZ INVADERS" 40 WHITE)
      (draw-text-middle "Press enter to start" 20 WHITE 0 100)]
     [running
      (if game-over?
          (begin
            (draw-text-middle "GAME OVER" 40 WHITE)
            (draw-text-middle "Press enter to restart" 20 WHITE 0 100))
          (spaceship-draw ship-x ship-y ship-scale ship-angle ship-color))
      (ShipEnemy-manager-for-each
       (lambda-ftypes (enemy-idx [ShipEnemy enemy])
         (spaceship-draw (ShipEnemy-normal-x enemy *game-time*)
                         enemy.position.y
                         ship-scale enemy.angle WHITE))
       enemies)
      (Bullet-manager-for-each 
       (lambda-ftypes (bullet-idx [Bullet bullet])
         (draw-circle-v &bullet.position 1.0 WHITE))
       bullets)]
     [level-complete
      (spaceship-draw ship-x ship-y ship-scale ship-angle ship-color)
      (draw-text-middle "LEVEL COMPLETE" 40 WHITE)
      (draw-text-middle
       (format #f "Press enter to start level ~a" (fx1+ game-level)) 20 WHITE 0 100)])))

(define (game-init) (safe-call user-game-init))
(define (game-update) (safe-call user-game-update))
(define (game-draw) (safe-call user-game-draw))

;; Local Variables:
;; eval: (put 'with-ftypes 'scheme-indent-function 1)
;; eval: (put 'lambda-ftypes 'scheme-indent-function 1)
;; eval: (put 'define-with-ftypes 'scheme-indent-function 1)
;; End:
