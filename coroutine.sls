(library (coroutine)
  (export yield
          create-task
          create-task!
          fork-task
          step
          create-wait-group
          wait-group-incr
          wait-group-wait
          wait-group-done)
  (import (chezscheme) (only (utils) safe-call) (match))

  (define yield-to-scheduler0
    (lambda (resume-task)
      (error 'yield-to-scheduler "yield-to-scheduler not defined")))
  (define yield-to-scheduler yield-to-scheduler0)

  (define-record-type task
    (fields
     name
     (mutable k)
     (mutable state)))

  (define-record-type wait-group
    (fields
     (mutable count)
     (mutable waiters)))

  (define create-wait-group
    (case-lambda
      [() (make-wait-group 0 '())]
      [(count) (make-wait-group count '())]))

  (define (wait-group-incr group)
    (wait-group-count-set! group (fx1+ (wait-group-count group))))

  (define (wait-group-wait group)
    (define resume-scheduler
      (call/1cc (lambda (resume-task)
                  (yield-to-scheduler `(wait ,group ,resume-task)))))
    (set! yield-to-scheduler resume-scheduler))

  (define (wait-group-done group)
    (wait-group-count-set! group (fx1- (wait-group-count group)))
    (when (fx= (wait-group-count group) 0)
      ;; Wake up waiters when the count is finished.
      (for-each
       (lambda (task) (task-state-set! task 'running))
       (wait-group-waiters group))
      (wait-group-waiters-set! group '())))

  (define (yield)
    (define resume-scheduler
      (call/1cc (lambda (resume-task)
                  (yield-to-scheduler `(yield ,resume-task)))))
    (set! yield-to-scheduler resume-scheduler))

  (define (create-task name thunk)
    (make-task name
               (lambda (resume-scheduler)
                 (set! yield-to-scheduler resume-scheduler)
                 (thunk)
                 (yield-to-scheduler #f))
               'running))

  (define-syntax (create-task! stx)
    (syntax-case stx ()
      [(_ name body* ...)
       #'(create-task name (lambda () (safe-call (lambda () body* ...))))]))

  (define (fork-task name thunk tasks)
    (set! tasks (cons (create-task name thunk) tasks)))

  (define (step tasks)
    (define (handle-task task)
      (when (eq? (task-state task) 'running)
        (let ([resume-task
               (call/1cc (lambda (resume-scheduler)
                           ((task-k task) resume-scheduler)))])
          (match resume-task
            [(yield ,k) (task-k-set! task k)]
            [(wait ,group ,k)
             (wait-group-waiters-set! group
                                      (cons task (wait-group-waiters group)))
             (task-k-set! task k)
             (task-state-set! task 'blocked)]
            [#f (task-state-set! task 'dead)]))))
    (for-each handle-task tasks)
    (filter (lambda (task) (not (eq? (task-state task) 'dead))) tasks)))

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
