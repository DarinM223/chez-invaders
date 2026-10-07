(library (coroutine)
  (export yield create-task create-task! fork-task step)
  (import (chezscheme) (utils))

  (define yield-to-scheduler0
    (lambda (resume-task)
      (error 'yield-to-scheduler "yield-to-scheduler not defined")))
  (define yield-to-scheduler yield-to-scheduler0)

  (define-record-type task
    (fields
     name
     (mutable k)
     (mutable dead?)))

  (define (yield)
    (define resume-scheduler
      (call/1cc (lambda (resume-task) (yield-to-scheduler resume-task))))
    (set! yield-to-scheduler resume-scheduler))

  (define (create-task name thunk)
    (make-task name
               (lambda (resume-scheduler)
                 (set! yield-to-scheduler resume-scheduler)
                 (thunk)
                 (yield-to-scheduler #f))
               #f))

  (define-syntax (create-task! stx)
    (syntax-case stx ()
      [(_ name body* ...)
       #'(create-task name (lambda () (safe-call (lambda () body* ...))))]))

  (define (fork-task name thunk tasks)
    (set! tasks (cons (create-task name thunk) tasks)))

  (define (step tasks)
    (define (handle-task task)
      (unless (task-dead? task)
        (let ([resume-task
               (call/1cc (lambda (resume-scheduler)
                           ((task-k task) resume-scheduler)))])
          (if (procedure? resume-task)
              (task-k-set! task resume-task)
              (task-dead?-set! task #t)))))
    (for-each handle-task tasks)
    (filter (lambda (task) (not (task-dead? task))) tasks)))

;; (module test ()
;;   (import (prefix (coroutine) co:))

;;   (define (sample name start end)
;;     (do ([i start (fx1+ i)])
;;         ((>= i end))
;;       (format #t "~a: ~a\n" name i)
;;       (co:yield)))

;;   (define my-tasks
;;     (list (co:create-task "1-3" (lambda () (sample "i" 1 3)))
;;           (co:create-task "2-5" (lambda () (sample "j" 2 5)))))

;;   (set! my-tasks (co:step my-tasks))
;;   (set! my-tasks (co:step my-tasks))
;;   (set! my-tasks (co:step my-tasks)))
