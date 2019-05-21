;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


;; For these tests both Scheme++ and this system were
;; compiled at optimize-level 3 and the test files were
;; also compiled at optimize-level 3.
;;
;; These tests were done with a send macro that did error
;; checking but which was not as badly written (ie. abstracted)
;; as it is now.  Compiler optimization should be able to
;; repair the damage (removing redundant tests, etc.).
;;
;; Test 1   pretty average case
;;   us
;;     (define z (make four 1))
;;     (define thunk (lambda () (send z show)))
;;     (bench 10000)
;;     ALPHA     48 ms     320008 bytes allocated
;;     486/33   280 ms     320192 bytes allocated
;;   them
;;     (define z (four 1))
;;     (define thunk (lambda () ($show z)))
;;     (bench 10000)
;;     ALPHA   6039 ms   1200504 bytes allocated
;;
;; Test 2   add allocation to a pretty average case
;;   us
;;     (define thunk (lambda () (send (make four 1) show)))
;;     (bench 10000)
;;     ALPHA   114 ms     1200352 bytes allocated
;;     486/33  380 ms     1200960 bytes allocated
;;   them
;;     (define thunk (lambda () ($show (four 1))))
;;     (bench 10000)
;;     ALPHA   8549 ms    2560808 bytes allocated
;;
;; Test 3   really bad case for us at present (chained dispatch)
;;   us
;;     (define z (make test10 10))
;;     (define thunk (lambda () (send z show)))
;;     (bench 10000)
;;     ALPHA   235 ms     1200512 bytes allocated
;;     486/33 1110 ms     1201016 bytes allocated
;;   them
;;     (define z (test10 10))
;;     (define thunk (lambda () ($show z)))
;;     (bench 10000)
;;     ALPHA   3270 ms    1200512 bytes allocated
;;
;; Test 4   add allocation to our bad case example
;;   us
;;     (define thunk (lambda () (send (make test10 10) show)))
;;     (bench 10000)
;;     ALPHA   240 ms     1521128 bytes allocated
;;     486/33 1160 ms     1521808 bytes allocated
;;   them
;;     (define thunk (lambda () ($show (test10 10))))
;;     (bench 10000)
;;     ALPHA   8930 ms    4081992 bytes allocated
;;
;; Test 5  just for fun, how do we do on an easy case?
;;   us
'      (define thunk
         (let ((z (make pr 2 3)))
           (lambda ()
             (send z set-car! (send z car))
             (send z set-cdr! (send z cdr)))))
;;     (bench 10000)
;;     ALPHA   18 ms       8 bytes allocated
;;     486/33  80 ms       8 bytes allocated
;;   them
'      (define thunk
         (let ((z (pr 2 3)))
           (lambda ()
             ($set-car! z ($car z))
             ($set-cdr! z ($cdr z)))))
;;     (bench 10000)
;;     ALPHA   1882 ms     320008 bytes allocated

(define thunk void)
(define bench
  (lambda (n)
    (time
      (let loop ((n n))
        (if (fxzero? n)
            #f
            (begin (thunk) (loop (fx- n 1))))))))

;; first test initialization and send-base

(define-class (one x) (<base>)
  (ivars (a (cons 'one x)))
  (inherited)
  (inheritable)
  (private
    [eye (x) x])
  (protected)
  (public
    [show () (list (send self geta))]
    [geta () (eye a)] ; send syntax not permitted for private
    [seta (x) (set! a x)]))

(define-class (two x) (one (cons 'two x))
  (ivars (b (cons 'two x)))
  (inherited)
  (inheritable)
  (private
    [eye (x) x])
  (protected)
  (public
    [show () (cons (send self getb) (send-base self show))]
    [getb () (eye b)] ; send syntax not permitted for private
    [setb (x) (set! b x)]))

(define-class (three x) (two (cons 'three x))
  (ivars (c (cons 'three x)))
  (inherited)
  (inheritable)
  (private
    [eye (x) x])
  (protected)
  (public
    [show () (cons (send self getc) (send-base self show))]
    [getc () (eye c)] ; send syntax not permitted for private
    [setc (x) (set! c x)]))

(define-class (four x) (three (cons 'four x))
  (ivars (d (cons 'four x)))
  (inherited)
  (inheritable)
  (private
    [eye (x) x])
  (protected)
  (public
    [show () (cons (send self getd) (send-base self show))]
    [getd () (eye d)] ; send syntax not permitted for private
    [setd (x) (set! d x)]))

;; more testing of send-base

(define-class (pr car cdr) (<base>)
  (ivars (car car) (cdr cdr))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [cdr () cdr]
    [car () car]
    [set-car! (v) (set! car v)]
    [set-cdr! (v) (set! cdr v)]))

(define-class (pr1 car cdr) (pr car cdr)
  (ivars (cnt 0))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [car () (set! cnt (+ cnt 1)) (send-base self car)]
    [cnt () cnt]))

'                   ;; commented out
(define thunk
  (lambda ()
    (let ((z (make pr1 4 5)))
      (send z set-car! (send z cdr))
      (send z set-cdr! (send z car))
      (send z cnt))))

;; test protected methods

;; pad these suckers
(define-class (test x) (<base>)
  (ivars (x x) (y 100) (pad1 1) (pad2 2) (pad3 3))
  (inherited)
  (inheritable x y)
  (private
    [sum () (list 'test-sum (+ x y))])
  (protected
    [mpad1 () 1]
    [mpad2 () 1]
    [prod () (list 'test-prod (* x y))]
    [diff () (list 'test-diff (- x y))])
  (public
    [setx (v) (set! x v)]
    [sety (v) (set! y v)]
    [show () (list (prod) (sum) (diff))]))

(define-class (test2 x) (test x)
  (ivars)
  (inherited x y)
  (inheritable x y)
  (private
    [sum () (printf "this won't be seen~n")])
  (protected
    [cons () (list 'test2-cons (#%cons x y))]
    [diff () (list 'test2-diff (- (* x x) (* y y)) (cons))])
  (public))

(define-class (test3 x) (test2 x)
  (ivars)
  (inherited x y)
  (inheritable x y)
  (private
    [diff () (printf "test3's diff will not be seen here~n")])
  (protected
    [sum () (printf "this won't be seen by inherited show~n")]
    [cons () (list 'test3-cons (#%cons x y))])
  (public))

(define-class (test4 x) (test3 x)
  (ivars)
  (inherited x y)
  (inheritable x y)
  (private)
  (protected
    [diff () (list 'test4-diff (send-base self diff))])  ; BROKEN? no send-base for protecteds now?
  (public))

(define-class (test5 x) (test4 x)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))
(define-class (test6 x) (test5 x)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))
(define-class (test7 x) (test6 x)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))
(define-class (test8 x) (test7 x)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))
(define-class (test9 x) (test8 x)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))
(define-class (test10 x) (test9 x)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))

'      ;; COMMENTED OUT
(define thunk
  (let ((z (make test4 123)))
    (lambda ()
      (send z show))))
'
(define thunk
  (lambda ()
    (let ((z (make test4 12)))
      (send z show))))

;; Times for (begin (collect) (bench 10000))
'
(define thunk (lambda () (send (make test 1) show)))
'
(define thunk (lambda () (send (make test2 1) show)))
'
(define thunk (lambda () (send (make test3 1) show)))
'
(define thunk (lambda () (send (make test4 1) show)))
'
(define thunk (lambda () (send (make test5 1) show)))
'
(define thunk (lambda () (send (make test6 1) show)))
'
(define thunk (lambda () (send (make test7 1) show)))
'
(define thunk (lambda () (send (make test8 1) show)))
'
(define thunk (lambda () (send (make test9 1) show)))
'
(define thunk (lambda () (send (make test10 1) show)))

;; test visibility of methods within the various kinds of methods 

; (let ()
(define-class (mtest x) (<base>)
  (ivars (x (* x x)))
  (inherited)
  (inheritable x)
  (private
    [priv-f (z) (printf "(priv-f ~s [~s])~n" z x)]
    [go-priv ()
      (printf "GO private~n")
      (priv-f x)
      (prot-f x)
      (send self pub-f x)])
  (protected
    [prot-f (z) (printf "(prot-f ~s [~s])~n" z x)]
    [go-prot ()
      (printf "GO protected~n")
      (priv-f x)
      (prot-f x)
      (send self pub-f x)])
  (public
    [pub-f (z) (printf "(pub-f ~s [~s])~n" z x)]
    [go-pub ()
      (printf "GO public~n")
      (priv-f x)
      (prot-f x)
      (send self pub-f x)
      (go-prot)
      (go-priv)]))

; (send (make mtest 5) go-pub)
; )

;; at optimize-level 3 set waste to 1250 to duplicate Scheme++ "performance"
;; at optimize-level 0 set waste to 120

(define-class (prot-test x y z) (<base>)
  (ivars (a x) (b y) (c z) (waste 120))
  (inherited)
  (inheritable)
  (private
    [waste-time (n)
     (if (fxzero? n)
         #f
         (waste-time (fx- n 1)))])
  (protected
    [f (x) (waste-time waste) (g b)]
    [g (y) (waste-time waste) (h c)]
    [h (z) (waste-time waste) (values a b c)])
  (public
    [go () (waste-time waste) (f a)]))


;; make sure we can call publics for other objects that have the
;; same name as our private and protected methods

(define-class (pub-test) (<base>)
  (ivars)
  (inherited)
  (inheritable)
  (private
    [f () "pub-test's private f method"])
  (protected
    [g () "pub-test's protected g method"])
  (public
    [go (x) (list (f) (g) (send x f) (send x g))]))

(define-class (aux) (<base>)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [f () "aux's public f method"]
    [g () "aux's public g method"]))


;; See if method names shadow formals
; broken now because priv/protected are not identifier macros?

(define-class (shadow) (<base>)
  (ivars (x 'ivar-1) (z 'ivar-2))
  (inherited)
  (inheritable x)
  (private
    [m1 (z) (list z x m1 m2 m3 m4)]
    [m2 (m1) (list z x m1 m2 m3 m4)])
  (protected
    [m3 (z) (list z x m1 m2 m3 m4)]
    [m4 (m1) (list z x m1 m2 m3 m4)])
  (public
    [go ()
     (list (m1 'm1) (m2 'm2) (m3 'm3) (m4 'm4))]))
 

(define-class (foobar) (<base>)
  (ivars [x 3] [y 99])
  (inherited)
  (inheritable)
  (private
    [f (x) (list x y)]
    [g (ls) (map f ls)])
  (protected)
  (public
    [sety (v) (set! y v)]
    [go (ls)
     (pretty-print (g ls))
     f]))
 

; Test protected method override and inheritance

(module (foo)
(define-class (foo) (<base>)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected
    [prop-set (v) (printf "prop-set ~s\n" v)]
    [prop-ref () (printf "prop-get\n")]
    [blop () (printf "original blop\n")])
  (public
    [go ()
     (printf "original go\n")
     (prop-set 'me)
     (prop-ref)
     (blop)]))
)

(define-class (bar) (foo)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected
    [blop () (printf "new improved blop\n")])
  (public
    [go ()
     (send-base self go)
     (printf "new improved go\n")
     (prop-set 'me)
     (prop-ref)
     (blop)]))

(define-class (baz) (bar)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))

