;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

; procedure  swl:clear-selection   clears the specified selection
; procedure  swl:get-selection     returns the specified selection or #f if none
; method     claim-selection       claims the specified selection (see base2.ss)

; procedure  swl:append-clipboard  tacks the given string onto the clipboard
; procedure  swl:clear-clipboard   clear the clipboard
; procedure  swl:get-clipboard     return the contents of the clipboard or #f

(module (swl:clear-selection
         swl:get-selection
         swl:clear-clipboard
         swl:append-clipboard
         swl:get-clipboard)

; Still need to abstract get-selection and get-clipboard ...
; Also need to introduce parameter to toggle 7bit filtering or 8bit clean mode.

(define clean
  (lambda (c)
    (integer->char (fxlogand #x7F (char->integer c)))))

; this is to deal w/ browsers that treat &nbsp; as #xA0
(define filter
  (lambda (s)
    (and (string? s)
         (let ([end (string-length s)])
           (do ([i 0 (+ i 1)]) ((= i end))
             (string-set! s i (clean (string-ref s i))))
           s))))

(swl:api-procedure define swl:clear-selection
 ;* \formdef{swl:clear-selection}{procedure}{(swl:clear-selection)}
 ;* \formdef{swl:clear-selection}{procedure}{(swl:clear-selection \var{selection})}
 ;* \ret{unspecified}
 ;* This clears the specified selection (\scheme{primary} or \scheme{clipboard})
 ;* if it exists.  The default is to clear the \scheme{primary} selection.
  (case-lambda
    [() (swl:clear-selection 'primary)]
    [(which)
     (swl:tcl-eval 'selection 'clear '-selection
       (swl:resolve-selection 'swl:clear-selection which))
     (void)]))

(swl:api-procedure define swl:get-selection
 ;* \formdef{swl:get-selection}{procedure}{(swl:get-selection)}
 ;* \formdef{swl:get-selection}{procedure}{(swl:get-selection \var{selection})}
 ;* \ret{see below}
 ;* This returns the specified selection (\scheme{primary} or \scheme{clipboard})
 ;* as a string if it exists, \scheme{#f} otherwise.  The default is to return the
 ;* \scheme{primary} selection.
  (case-lambda
    [() (swl:get-selection 'primary)]
    [(which)
     ;; Tk doesn't give us a way to ask whether the selection exists.
     ;; Therefore we use Tcl's catch construct, and therefore we're
     ;; in a critical section.
     ;; can abstract this with swl:get-clipboard once we have a module system.
     (critical-section
       (and (string=? "0"
              (swl:tcl-eval 'catch
                 #\{ 'selection 'get '-selection
                       (swl:resolve-selection 'swl:get-selection which)
                 #\}
                 'swlgetselection))
            (filter (swl:tcl-eval 'set 'swlgetselection))))]))

(swl:api-procedure define swl:clear-clipboard
  ;* \formdef{swl:clear-clipboard}{procedure}{(swl:clear-clipboard)}
  ;* \ret{unspecified}
  ;* This clears and claims ownership of the clipboard.
  (lambda ()
    (swl:tcl-eval 'clipboard 'clear)
    (void)))

(swl:api-procedure define swl:append-clipboard
  ;* \formdef{swl:append-clipboard}{procedure}{(swl:append-clipboard \var{string})}
  ;* \ret{unspecified}
  ;* This appends the given string to the clipboard where it may be
  ;* retrieved via \scheme{(swl:get-selection 'clipboard)}.
  ;* \scheme{swl:clear-clipboard} must be called before calling \scheme{swl:append-clipboard},
  ;* and multiple appends should be carried out in a critical section to
  ;* prevent another process from retrieving an incomplete clipboard or
  ;* usurping ownership of the clipboard.
  ;* The append mechanism makes no
  ;* attempt to prevent multiple threads from writing to the clipboard.
  ;*
  ;* In practice it appears that each (operating-system) process has its own
  ;* clipboard and \scheme{swl:append-clipboard} writes to that clipboard.
  ;\scheme{(swl:get-selection 'clipboard)}
  ;* seems to read from the clipboard of whichever process wrote to its
  ;* clipboard last.  \scheme{(swl:clear-selection 'clipboard)} seems to
  ;* clear the clipboards of all applications.
  ;* (Still searching for adequate documentation of clipboards in Tcl/Tk.)
  (lambda (string)
    (unless (string? string)
      (assertion-violationf 'swl:append-clipboard "supports string data only"))
    (swl:tcl-eval 'clipboard 'append '-- string)
    (void)))

(swl:api-procedure define swl:get-clipboard
  ;* \formdef{swl:get-clipboard}{procedure}{(swl:get-clipboard)}
  ;* \ret{see below}
  ;* This returns the contents of the clipboard if any, else \scheme{#f}.
  (lambda ()
    ;; can abstract this with swl:get-selection once we have a module system.
    (critical-section
       (and (string=? "0"
              (swl:tcl-eval 'catch #\{
                  'selection 'get '-selection '|CLIPBOARD|
                #\}
                'swlgetclipboard))
            (filter (swl:tcl-eval 'set 'swlgetclipboard))))))

(define swl:resolve-selection
  (lambda (who sel)
    (case sel
      [(primary) '|PRIMARY|]
      [(secondary) '|SECONDARY|]
      [else (assertion-violationf who "invalid selection ~s" sel)])))

)
