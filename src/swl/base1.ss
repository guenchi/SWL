;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Base classes for unnamed Scheme Graphics Library
;; $Id: base1.ss 1.1.1.1 Thu, 20 Jun 2002 13:45:19 -0500 owaddell $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tk-Object
;;
;;   Let's us ask if something isa? tk-object, and sanitizes widget destroy.

;; Note:
;;   destroy method currently blitzes only the public dispatch table
;;   presumably it is an error if user code calls destroy and then continues
;;   to call protected or private methods on the object.  ie. destroy is
;;   the last thing that should be done.  since this bangs on internal
;;   representation, I was tempted to put this in ../oop/class.ss, instead
;;   we might generalize this

(module (<tk-object>)

(define-syntax pass-it-on
  (syntax-rules ()
    [(_ self method arg ...)
     (let ([parent (send self get-parent)])
       (if parent
           (send parent method arg ...)
           (assertion-violationf #f "ancestor widget must call load-prefs before ~s can invoke ~s"
             (send self class-name)
             'method)))]))

(define-swl-class (<tk-object>) (<base>)
  ;* Documentation for this class will not be printed
  ;* using the current document compiler.
  (ivars (misc '()))
  (inherited)
  (inheritable)
  (private)
  (protected
    [prop-set! (key what)
     (let ((hit (assq key misc)))
       (if hit
           (set-cdr! hit what)
           (set! misc (cons (cons key what) misc))))]
    [prop-ref (key)
     (let ((hit (assq key misc)))
       (and hit (cdr hit)))])
  (public
    [print-rep ()
     ;* \ret{string}     
     ;* Builds a printable representation for the instance.
     (send self print-rep "")]
    [print-rep (str)
     ;* \ret{string}     
     ;* Builds a printable representation for the instance.
     (string-append "#i[" (symbol->string (class-name self))  " " str "]" )]
    [print ()
     ;* \ret{unspecified}
     ;* Displays a printable representation of the instance to the
     ;* current output port.
     (send self print (current-output-port))]
    [print (op)
     ;* \ret{unspecified}
     ;* Displays a printable representation of the instance to the
     ;* output port \var{op}.
     (fprintf op "~a" (send self print-rep))]

    [get-parent ()
     ;* \ret{see below}
     ;* Returns the parent of the widget or \scheme{#f} if the widget has no parent.
     #f]
    [load-prefs (tag)
     ;* \ret{unspecified}
     ;* \scheme{load-prefs} fetches the global preferences stored under this tag
     ;* and installs the \scheme{'prefs} property which indicates that this
     ;* widget is the root of the preference tree.  Children of this
     ;* widget delegate to this widget to manage their preferences.
     ; (tempted to make it a set-prefs-tag! method so that it would
     ; jive better with our automatic (prefs-tag: ...) syntax in the
     ; create macro and thereby appear more functional.
     (prop-set! 'prefs (or (swl:load-preferences tag) '()))]
    [save-prefs! (tag)
     ;* \ret{unspecified}
     ;* Saves the preferences associated with this tag.
     ; perhaps pointless or disagreeable to have the added flexibilty (or liability)
     ; of saving prefs under a different tag from that under which we loaded them
     (let ([als (prop-ref 'prefs)])
       (if als
           (swl:save-preferences! tag als)
           (pass-it-on self save-prefs! tag)))]
    [set-pref! (key val)
     ;* \ret{unspecified}
     ;* Associates the value \var{val} with preference \var{key}.
     (let ([als (prop-ref 'prefs)])
       (if als
           (let ([a (assq key als)])
             (if a
                 (set-cdr! a val)
                 (prop-set! 'prefs (cons (cons key val) als))))
           (pass-it-on self set-pref! key val)))]
    [get-pref (key default)
     ;* \ret{preference value associated with \var{key}, or \var{default} if none}
     ;* A warning is signaled in the latter case.
     (on-error
       (with-message msg
         (begin
           (warningf #f (format "Failed to get ~s preference for ~s.\n\n~a" key tag msg))
           default))
       (let ([als (prop-ref 'prefs)])
         (if als
             (let ([a (assq key als)]) (if a (cdr a) default))
             (pass-it-on self get-pref key default))))]

    [destroy ()
     ;* \ret{unspecified}
     ;* Destroys the object.

     ;; catching protected method calls here would entail banging in a
     ;; protected vector filled w/ error procs.  and we'd want to share
     ;; these (have one that was large enough for the biggest protected
     ;; table known)
     ;;
     ;; catching private method calls here is nigh-well impossible, thus
     ;; we only catch public calls
     (critical-section
     (vector-set! self 0
       (lambda (msg instance . args)
         (unless
           (case msg
             ((destroy) (null? args))
             ((disown) (and (not (null? args)) (null? (cdr args))))
             (else #f))
           (assertion-violationf msg "instance has been destroyed"))))
     ;; experimental:  explicitly blitz all ivars (but not class prot table)
     ;; (nuking class prot table would make instance? unhappy)
     (let loop ((i (fx- (vector-length self) 1)))
       (unless (fx<= i 1)
         (vector-set! self i #f)
         (loop (fx- i 1))))
     )]))

)

