(module ()
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(swl:tutorial swl:file-dialog
;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

  (illustrates swl:file-dialog <toplevel> <text>
    make-menu destroy pack <scrollframe>
    delete-all set-enabled! set-action! action: insert)
  (follows <menu>)
  ;* The following program implements a simple viewer for the tutorials
  ;* in this section (located in the \scheme{tutorial} directory of this release).
  ;* We first define a simple procedure that loads a file into a \scheme{<text>}
  ;* widget using block I/O operations.  Next we include the definition
  ;* of a \scheme{<scrollframe>} class.  We use a \scheme{<scrollframe>} as the
  ;* parent for the \scheme{<text>} widget so that scrollbars will be displayed
  ;* automatically when scrolling is possible.  The same could be done
  ;* for a \scheme{<listbox>} or \scheme{<canvas>} widget.
  ;*
  ;* The ``Open'' item on the ``File'' menu uses \scheme{swl:file-dialog} to
  ;* present the user with a choice of filenames via the file
  ;* selection dialog of the native window system. 
  ;* The \scheme{'open} flag requires that the user specify an existing file.
  ;* The optional \scheme{file-types:} parameter restricts the display to
  ;* files with an extension matching \scheme{"*.ss"}.
  ;* If the user does not cancel out of the dialog, a string corresponding
  ;* to the chosen filename is returned.  In this case we update the
  ;* title of the \scheme{<toplevel>} window accordingly, delete all text
  ;* from the \scheme{<text>} widget, insert the contents of the file,
  ;* and record the name of the selected file so that the file can be loaded
  ;* when the ``Run'' menu item is selected.
(let ()
  (define read-file
    (lambda (filename txt)
      (let ([buf (make-string 2048)])
        (let loop ([ip (open-input-file filename)])
          (let ([x (block-read ip buf 2048)])
            (unless (eof-object? x)
              (insert txt (if (< x 2048) (substring buf 0 x) buf))
              (loop ip)))))))
  (let* ([top (create <toplevel> with (title: "Tutorials"))]
         [sf (create <scrollframe> top)]
         [txt (create <text> sf)]
         [tutorial #f])
    (send txt set-font! (create <font> 'courier 12 '()))
    (set-menu! top
      (make-menu
        ("_File"
          (make-menu
            ("_Open"
              (lambda (item)
                (let ([filename
                       (swl:file-dialog "Select a tutorial" 'open
                         (file-types: '(("Scheme source" ("*.ss"))))
                         (parent: top))])
                  (when filename
                    (set-title! top (format "Tutorial: ~s" filename))
                    (delete-all txt)
                    (set! tutorial filename)
                    (read-file filename txt)))))
            ("_Run"
              (lambda (item)
                (when tutorial (load tutorial))))
            ("_Quit" (lambda (item) (destroy top)))))))
    (pack sf (expand: #t) (fill: 'both))))

) ; end tutorial
)

