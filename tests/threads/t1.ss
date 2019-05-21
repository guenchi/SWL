(import swl:threads)
;(install-thread-system)

(define-syntax assert ; not present in 7.4
  (syntax-rules ()
    [(_ expr)
     (unless expr
       (syntax-error #'expr "assertion failed:"))]))

(define (t1)
  (define verdict #f)
  (thread-fork
    (lambda ()
      (let ([q (thread-make-msg-queue 'cready)] [results '()])
        (thread-fork
          (lambda ()
            (printf "sleeping\n")
            (thread-sleep 500)
            (printf "waking\n")
            (thread-send-msg q 2)))
        (critical-section
          (set! results (cons 1 results))
          (set! results (cons (thread-receive-msg q) results)))
        (assert (equal? '(2 1) results))
        (set! verdict #t))))
  (let f () (or verdict (begin (thread-yield) (f)))))

(define (t2 n)
  (define verdict #f)
  (thread-fork
    (lambda ()
      (let ([q (thread-make-msg-queue 'cready)] [results '()])
        (thread-fork
          (lambda ()
            (let f ([n n])
              (when (> n 0)
                (thread-sleep 100)
                (f (- n 1))))
            (thread-send-msg q 2)))
        (critical-section
          (set! results (cons 1 results))
          (set! results (cons (thread-receive-msg q) results)))
        (assert (equal? '(2 1) results))
        (set! verdict #t))))
  (let f () (or verdict (begin (thread-yield) (f)))))

