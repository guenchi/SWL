;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; this is a quick hack to get a group of mutually-exclusive choices 
;; represented as radiobuttons, that operate on a common object when
;; pressed.

(define-swl-class (<option-box> parent options setter getter) (<frame> parent)
  (ivars (title #f)
         (options options)
         (setter setter)
         (getter getter)
         (receiver #f))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [init (parent opts setter getter)
     (set! title (create <label> self))
     (show title)
     ;; hackery dacquiri dock
     (set! options
       (let loop ([opts opts])
         (if (null? opts)
             '()
             (let ([x (car opts)])
               (let ([label (car x)] [val (cdr x)])
                 (let ([rb (create <radiobutton> self
                             with
                              (title: label)
                              (action:
                                (lambda (self)
                                  (when receiver (setter receiver val)))))])
                   (pack rb (side: 'top) (anchor: 'w))
                   (cons (cons val rb) (loop (cdr opts)))))))))]
    [set-title! (str) (send title set-title! str)]
    [get-title () (send title get-title)]
    [operate-on (obj)
     (set! receiver obj)
     (let loop ((opts options) (val (getter obj)))
       (cond
         ((null? opts) (printf "rats - no options match~n"))
         ((equal? (caar opts) val) (select (cdar opts)))
         (else (loop (cdr opts) val))))]))

