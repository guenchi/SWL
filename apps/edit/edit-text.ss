;; Copyright (c) 1996 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(require "../common/scheme-text.ss" 'once)

(define-swl-class (<edit-text> parent) (<scheme-text> parent)
  (ivars
    (new-menu-item #f)
    (open-menu-item #f)
    (insert-file-menu-item #f)
    (execute-menu-item #f)
    (save-menu-item #f)
    (save-as-menu-item #f)
    (directory-menu-item #f)
    (quit-menu-item #f)
    (undo-menu-item #t)
    (redo-menu-item #t)
    (copy-menu-item #t)
    (cut-menu-item #t)
    (paste-menu-item #t)
    (buffer-modified-menu-item #f)
    (outline-mode-menu-item #f)
    (need-to-save-buffer? #f)
    (paste-str #f)
    (posted? #f)
    (requests '()) ; who cares about edits to this widget?
    )
  (inherited mini-buffer box-markup paren-markup highlight-markup
    key-prefix begin-exp end-exp end-mk handle)

  (inheritable mini-buffer box-markup paren-markup highlight-markup
    key-prefix begin-exp end-exp end-mk handle)

  (private
    [activate (mi)
      (send self turn-search-off)
      (when (send mi get-enabled) ((send mi get-action) mi))]
    [editor-reserved? (cancel-res)
     (or posted?
         (and (not (null? requests))
              (fluid-let ([posted? #t])
                ; make sure the <toplevel> window is visible
                ; and on top so we know which editor we're
                ; about to blow away.
                (let ([top (do ([w self (send w get-parent)]) ((isa? w <toplevel>) w))])
                  (send top show)
                  (send top raise)
                  (swl:sync-display)
                  (let ([ans
                         (warning-dialog top
                           (apply string-append "This editor is being used"
                             (let f ([ls (map request-reason requests)] [n 0])
                               (if (null? (cdr ls))
                                   (list* (case n [(0) " "] [(1) " and "] [else ", and "]) (car ls) '("."))
                                   (cons (if (= n 0) " " ", ") (cons (car ls) (f (cdr ls) (+ n 1)))))))
                           (cons cancel-res '(#21=|leave it unchanged|)))])
                    (or (eq? ans '#21#)
                        (begin
                          (for-each (lambda (req) ((request-denied req))) requests)
                          (set! requests '())
                          #f)))))))]

    )
  (protected)
  (public
    [get-string () (send self get-string '(0 . 0) end-mk)]

    [reserved? (cancel-res) (editor-reserved? cancel-res)]

    [register-request (request)
     (set! requests (cons request requests))]
    [unregister-request (request)
     (set! requests (remq request requests))]
    [get-requests () requests]

    [buffer-modified? () need-to-save-buffer?]

    [set-buffer-to-safe! ()
     (set-title! buffer-modified-menu-item "")
     (send self mark-undo-state)
     (set! need-to-save-buffer? #f)]

    [set-buffer-to-modified! ()
     (unless need-to-save-buffer?    ; avoid flickering on slow displays (vnc)
       (set-title! buffer-modified-menu-item "(Modified)"))
     (send self show-cursor-pos)
     (set! need-to-save-buffer? #t)]

    [undo ()
(unless (editor-reserved? '|modify anyway|)
     (send-base self undo)
      ; could be more clever and include save among the undo actions so
      ; that we can reset buffer modified flag appropriately
)
    ]

    [redo ()
(unless (editor-reserved? '|modify anyway|)
     (send-base self redo)
      ; could be more clever and include save among the undo actions so
      ; that we can reset buffer modified flag appropriately
)
    ]

    [delete-char (disp)
(unless (editor-reserved? '|modify anyway|)
     (send-base self delete-char disp)
)
    ]
    [delete-eol ()
(unless (editor-reserved? '|modify anyway|)
     (send-base self delete-eol)
     (send self set-buffer-to-modified!)
)
     ]

    [delete (idx)
(unless (editor-reserved? '|modify anyway|)
     (send-base self delete idx)
     (send self set-buffer-to-modified!)
)
      ]

    [delete (idx1 idx2)
(unless (editor-reserved? '|modify anyway|)
      ;; this method is used for several things besides cutting the selection. 
      ; so we can yank it back
     (send-base self delete idx1 idx2)
     (send self set-buffer-to-modified!)
)
      ]

    [insert-at (idx what)
(unless (editor-reserved? '|modify anyway|)
     (send-base self insert-at idx what)
     (send self set-buffer-to-modified!)
)
      ]

    [set-cursor-pos! (idx)
     (send-base self set-cursor-pos! idx)
     (send self show-cursor-pos)
    ]

    [show-cursor-pos ()
     (let ([here (get-cursor-pos self)])
       (send self display-mini (format "line ~s, char ~s" (+ 1 (cdr here)) (+ 1 (car here)))))]

    [key-press-no-prefix (key mods)
      (if (number? key) ;;; kludge test for modifier key-only
	  (send-base self key-press-no-prefix key mods)
	  (begin 
	    (event-case ((key= key) (modifier= mods))
	      [([alt #\n]) (activate new-menu-item)]
	      [([alt #\o]) (activate open-menu-item)]
	      [([alt #\i]) (activate insert-file-menu-item)]
	      [([alt #\g]) (activate execute-menu-item)]
	      [([alt #\s]) (activate save-menu-item)]
	      [([alt #\a]) (activate save-as-menu-item)]
	      [([alt #\d]) (activate directory-menu-item)]
	      [([alt #\q]) (activate quit-menu-item)]
	      [([alt #\c]) (activate copy-menu-item)]
	      [([alt #\x]) (activate cut-menu-item)]
	      [([alt #\v]) (activate paste-menu-item)]
	      [([control #\d])
	       (send self action-copy) ;;; buffer any selection
	       ;; let the base-class do the actual delete
	       (send-base self key-press-no-prefix key mods)
	       ]
	      [else
	       (send-base self key-press-no-prefix key mods)
	       ])))]

    [set-menu-items! (unsaved-m new-m open-m insert-file-m execute-m save-m save-as-m directory-m quit-m
		       undo-m redo-m copy-m cut-m paste-m outline-mode-m)
      (set! buffer-modified-menu-item unsaved-m)
      (set! new-menu-item new-m)
      (set! open-menu-item open-m)
      (set! insert-file-menu-item insert-file-m)
      (set! execute-menu-item execute-m)
      (set! save-menu-item save-m)
      (set! save-as-menu-item save-as-m)
      (set! directory-menu-item directory-m)
      (set! quit-menu-item quit-m)
      (set! undo-menu-item undo-m)
      (set! redo-menu-item redo-m)
      (set! copy-menu-item copy-m)
      (set! cut-menu-item cut-m)
      (set! paste-menu-item paste-m)
      (set! outline-mode-menu-item outline-mode-m)
      (send self notify-undo-state #f #f #f)
      ]

    [notify-undo-state (undo? redo? modified?)
     ; base method is NOP, but including this in case we change base class w/o
     ; scrutinizing each and every method
     (send-base self notify-undo-state undo? redo? modified?)
     (set-enabled! undo-menu-item undo?)
     (set-enabled! redo-menu-item redo?)
     (if modified?
         (send self set-buffer-to-modified!)
         (send self set-buffer-to-safe!))]

    [destroy () (when quit-menu-item (activate quit-menu-item))]
  ))

