;; Copyright (c) 1996 R. Kent Dybvig
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(module ()
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)

(define label-printf)
(define (go*)
  (let* ([top (create <toplevel> with (title: "Welcome to Animal!"))]
         [font (create <font> 'times 18 '(roman normal))]
         [label (create <label> top with
                  (width/char: 50)
                  (font: font)
                  (wrap-length: 300))]
         [frame1 (create <frame> top)]
         [frame2 (create <frame> top)]
         [ybutton (create <button> frame1 with
                    (title: "yes")
                    (font: font)
                    (width/char: 4)
                    (action:
                      (lambda (self)
                        (record-case animal-state
                          [(y/n) (k) (k #t)]))))]
         [nbutton (create <button> frame1 with
                    (title: "no")
                    (font: font)
                    (width/char: 4)
                    (action:
                      (lambda (self)
                        (record-case animal-state
                          [(y/n) (k) (k #f)]))))]
         [entry (create <entry> top with (width/char: 50)
                  (font: font)
                  (action:
                    (lambda (self)
                      (record-case animal-state
                        [(text) (k)
                         (let ((s (get-string self)))
                           (delete-all self)
                           (set-focus top)
                           (set-enabled! self #f)
                           (k s))]))))]
         [scroll1 (create <scrollbar> top with (orientation: 'horizontal))]
         [quit-button
          (create <button> frame2 with
            (title: "quit")
            (font: font)
            (width/char: 4)
            (action: (lambda (self) (destroy top))))]
         [okay-button
          (create <button> frame2 with
            (title: "ok")
            (font: font)
            (width/char: 4)
            (action:
              (lambda (self)
                (record-case animal-state
                  [(start) (k) (k)]
                  [(text) (k)
                   (let ((s (get-string entry)))
                     (delete-all entry)
                     (set-focus top)
                     (set-enabled! entry #f)
                     (k s))]))))])
    (set-option! entry
      (hscroll-notify:
        (lambda (left right) (set-view! scroll1 left right))))
    (set-option! scroll1
      (action: (lambda (self n q) (hscroll entry n q))))
    (pack ybutton (side: 'top))
    (pack nbutton (side: 'bottom))
    (pack frame1 (side: 'left))
    (pack okay-button (side: 'top))
    (pack quit-button (side: 'bottom))
    (pack frame2 (side: 'right))
    (pack label)
    (pack entry (expand: #t) (fill: 'x))
    (pack scroll1 (expand: #t) (fill: 'x))
    (set! read-text
      (lambda (k)
        (set-enabled! entry #t)
        (set-focus entry)
        (set! animal-state
          (list 'text k))))
    (set! label-printf
      (lambda args
        (set-title! label (apply format args))))))

;(define-structure (qrec question yes no))
(define make-qrec list)
(define qrec-question car)
(define qrec-yes cadr)
(define qrec-no caddr)
(define qrec? pair?)

(define data-file
  (format "~a/~a"
    (or (getenv "TEMP")
        (getenv "TMP")
        (if (memv (machine-type) '(i3nt ppcnt)) "c:/temp" "/tmp"))
    "animal.dat"))

;;; animal-states are
;;;   (start k): k is waiting signal to start
;;;   (y/n k): k is waiting for yes or no answer
;;;   (text k): k is waiting for text input
(define animal-state)

(define go
  (lambda ()
    (go*)
    (let ((t (and (file-exists? data-file)
                  (guard (c [#t #f])
                    (let* ((ip (open-input-file data-file))
                           (t (guard (c [#t #f])
                                (read ip))))
                      (close-input-port ip)
                      (if (let ok? ((t t))
                            (or (string? t)
                                (and (qrec? t)
                                     (string? (qrec-question t))
                                     (ok? (qrec-yes t))
                                     (ok? (qrec-no t)))))
                          t
                          (begin
                            (label-printf "corrupt database; starting over")
                            #f)))))))
      (let loop ((t (or t "dog")))
        (label-printf "Think of an animal and press ok.")
        (set! animal-state
          (list 'start
            (lambda ()
              (animal t
                (lambda (t)
                  (let ((op (open-output-file data-file 'truncate)))
                    (pretty-print t op)
                    (close-output-port op))
                  (loop t))))))))))

(define animal
  (lambda (t k)
    (if (qrec? t)
        (let ((q (qrec-question t)) (yes (qrec-yes t)) (no (qrec-no t)))
          (label-printf "~a" q)
          (set! animal-state
            (list 'y/n
              (lambda (yes?)
                (if yes?
                    (animal yes (lambda (t) (k (make-qrec q t no))))
                    (animal no (lambda (t) (k (make-qrec q yes t)))))))))
        (begin
          (label-printf "Is your animal '~a'?" t)
          (set! animal-state
            (list 'y/n
              (lambda (yes?)
                (if yes?
                    (k t)
                    (begin
                      (label-printf "Enter the name of your animal.")
                      (read-text
                        (lambda (a)
                          (label-printf "Enter a question that distinguishes '~a' from '~a'." a t)
                          (read-text
                            (lambda (q)
                              (let ((q (massage-q q)))
                                (label-printf "~a" (subst-q q a))
                                (set! animal-state
                                  (list 'y/n
                                    (lambda (yes?)
                                      (if yes?
                                          (k (make-qrec q a t))
                                          (k (make-qrec q t a))))))))))))))))))))
 
(define read-text
  (lambda (k)
    (set! animal-state
      (list 'text
        (lambda (s)
          (if (null? s)
              (read-text k)
              (k s)))))))

(define massage-q
  (lambda (q)
    (let ((q (if (char=? (string-ref q (- (string-length q) 1)) #\?)
                 q
                 (format "~a?" q))))
      (when (char-lower-case? (string-ref q 0))
        (string-set! q 0 (char-upcase (string-ref q 0))))
      q)))

(define subst-q
  ; extremely naive
  (lambda (q a)
    (define subst-string
     ; not the fastest algorithm...
      (lambda (old new s)
        (let ((sn (string-length s)) (oldn (string-length old)))
          (let lp1 ((i 0) (j oldn))
            (if (> j sn)
                #f
                (if (string=? (substring s i j) old)
                    (string-append (substring s 0 i) new
                      (let ((s (substring s j sn)))
                        (or (subst-string old new s) s)))
                    (lp1 (+ i 1) (+ j 1))))))))
    (let ((b (format " '~a' " a)))
      (or (subst-string " your animal " b q)
          (subst-string " it " b q)
          (subst-string " the animal " b q)
          (format "what is the answer for '~a'?" a)))))

(go))
