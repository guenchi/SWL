;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

; Confusing?  thunks invoked by the select method.

(define-swl-class (<selectbox> parent) (<listbox> parent)
  (ivars (thunks (vector #f)) (count 0) (action (lambda x (void))))
  (inherited)
  (inheritable)
  (private
    [vector-shuffle! (vec to from)
     (let ([len (vector-length vec)])
       (let loop ([i to] [j from])
         (unless (fx= i from)
           (vector-set! vec i (and (fx< j len) (vector-ref vec j)))
           (loop (fx+ i 1) (fx+ j 1)))))]
    [index->thunk (i) (vector-ref thunks i)])
  (protected)
  (public
    [mouse-press (x y mods)
     (send-base self mouse-press x y mods)
     (event-case ((modifier= mods))
       (([double left-button]) (action self)))]
    [add-choice (label proc)
     (let ((len (vector-length thunks)))
       (when (= count len)
         (let ((newvec (make-vector (* 2 len)))
               (oldvec thunks))
           (let loop ((n 0))
             (unless (= n len)
               (vector-set! newvec n (vector-ref oldvec n))
               (loop (+ n 1))))
           (set! thunks newvec)))
        (send self insert count label)
        (vector-set! thunks count proc)
        (set! count (+ count 1)))]
    [delete-all () (send self delete 0 'end)]
    [delete (from to)
     (let ([from (if (eq? from 'end) (item-count self) from)]
           [to (if (eq? to 'end) (item-count self) to)])
       (let ([min (min from to)] [max (max from to)])
         (send-base self delete min max)
         (vector-shuffle! thunks min max)
         (set! count (item-count self))))]
    [delete (i)
     (send-base self delete i)
     (vector-shuffle! thunks i (fx+ i 1))]
    [init (parent)
     (send-base self init parent)
     (set-select-mode! self 'single)
     (set-export-selection! self #f)]
    [select (index)
     (unless (negative? index) 
       (clear-selection self 0 'end)
       (send-base self select index)
       (let ((f (index->thunk index)))
         (when f (f))))]
    [set-action! (proc)
     (unless (procedure? proc)
       (assertion-violationf 'set-action! "~s is not a procedure" proc))
     (set! action proc)]
    [get-action () action]))

