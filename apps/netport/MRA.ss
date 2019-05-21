;; Copyright (c) 1996 Carl Bruggeman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;; MRA-list "Most Recently Accessed list" abstraction
;
(define-swl-class (<MRA-list> size) (<base>)
  (ivars [size (+ size 1)] [first 0] [last 0]
         [keys (make-vector (+ size 1) #f)]
         [vals (make-vector (+ size 1) #f)])
  (inherited)
  (inheritable)
  (private
    [lookup (key)
     (let loop ([i first])
       (cond
         [(= i last) #f]
         [(equal? key (vector-ref keys i))
          (vector-ref vals i)]
         [else 
          (loop (next-index i))]))]
    [next-index (idx) (let ([i (+ idx 1)]) (if (>= i size) 0 i))])
  (protected)
  (public
    [init (size) (void)]
    [add (key val)
     (unless (lookup key)
       (vector-set! keys last key)
       (vector-set! vals last val)
       (set! last (next-index last))
       (when (= first last)
         (set! first (next-index first))))]
    [list ()
     (let loop ([i first] [ls '()])
       (cond
         [(= i last) ls]
         [else 
          (loop (next-index i)
                (cons (cons (vector-ref keys i) (vector-ref vals i))
                      ls))]))]
    
  )
)

