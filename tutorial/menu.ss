(module ()
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(swl:tutorial <menu>
;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

  (illustrates
    <listbox> <menu> <toplevel> get-menu-items get-title
    insert make-menu set-enabled! show title:)
  (follows <entry>)
  ;* \label{menu-tutorial}  %% I should have a macro for ref'ing tutorials... (since I'm generating a label in stex)
  ;* Simple menus can be created using the \scheme{make-menu} macro which constructs
  ;* an instance of the \scheme{<menu>} class.  More complicated menus can be
  ;* created by constructing an instance of the \scheme{<menu>} class and supplying
  ;* a list of ``menu item'' objects such as command, check, radio, separator,
  ;* or cascade items.  A menu must be attached to a
  ;* toplevel (or to another menu) before it can be displayed.
  ;*
  ;* In the following example we create a toplevel window containing a listbox.
  ;* We use the \scheme{make-menu} macro to construct a menu with two top
  ;* level entries, \scheme{"_File"} and \scheme{"_Edit"}.
  ;* The subforms \scheme{make-menu} are lists associating a string label
  ;* with a procedure or a submenu.
  ;* For example, the \scheme{"_Open"} label is associated with
  ;* a procedure, returned by \scheme{say}, that
  ;* inserts a string in the listbox \scheme{lb}, and the \scheme{"_Disable"}
  ;* item is associated with a submenu constructed using \scheme{make-menu}.
  ;* The underscore characters (`\scheme{_}') in the menu titles may be used
  ;* to post a menu by pressing \scheme{Alt} and the letter following
  ;* the underscore in the title of that menu.
  ;* For example, pressing \scheme{Alt}+\scheme{f} posts the ``File'' menu.
  ;*
  ;* Note how the entire
  ;* ``Edit'' menu is enabled and disabled using
  ;* \scheme{set-enabled!}.  Individual menu items are enabled and disabled
  ;* similarly.  Below we use the \scheme{find} procedure to retrieve these
  ;* items, but they could also be bound explicitly if the menu were
  ;* constructed by hand.
  ;*
  ;* The menu is installed on the \scheme{<toplevel>} window using
  ;* the \scheme{set-menu!} method, and then the listbox
  ;* \scheme{lb} is made visible under the frame via \scheme{show}.

(let* ([top (create <toplevel> with (title: "Menu Example"))]
       [lb (create <listbox> top)]
       [say (lambda (what) (lambda (self) (insert lb 0 what)))]
       [find
        (let ()
          (define massoc
            (lambda (title menu)
              (let loop ([ls (send menu get-menu-items)])
                (cond
                  [(null? ls) #f]
                  [(string=? title (get-title (car ls))) (car ls)]
                  [else (loop (cdr ls))]))))
          (define find
            (lambda (full-path)
              (let search ([path full-path] [found (send top get-menu)])
                (if (not found)
                    (assertion-violationf 'find "bad menu path ~s" full-path)
                (if (null? path)
                    found
                    (search (cdr path) (massoc (car path) found)))))))
          find)]
       [menu
        (make-menu
          ("_File"
            (make-menu
              ("_Open" (say "Open"))
              ("_Save" (say "Save"))
              ("_Disable"
                (make-menu
                  ("Edit" (lambda (self) (set-enabled! (find '("_Edit")) #f)))
                  ("Edit:Undo"
                    (lambda (self) (set-enabled! (find '("_Edit" "_Undo")) #f)))
                  ("Edit:Zap"
                    (lambda (self) (set-enabled! (find '("_Edit" "_Zap")) #f)))))
              ("_Enable"
                (make-menu
                  ("Edit" (lambda (self) (set-enabled! (find '("_Edit")) #t)))
                  ("Edit:Undo"
                    (lambda (self) (set-enabled! (find '("_Edit" "_Undo")) #t)))
                  ("Edit:Zap"
                    (lambda (self) (set-enabled! (find '("_Edit" "_Zap")) #t)))))
              ("_Print"
                (make-menu
                  ("Draft" (say "Print Draft"))
                  ("2-up" (say "Print 2-up"))))))
          ("_Edit"
            (make-menu
              ("_Undo" (say "Undo"))
              ("_Process"
                (make-menu
                  ("_Fold" (say "Fold"))
                  ("_Spindle" (say "Spindle"))
                  ("_Mutilate" (say "Mutilate"))))
              ("_Zap"
                (make-menu
                  ("Blaster"
                    (make-menu
                      ("Death _Ray" (say "Death Ray Blaster"))
                      ("Ion _Pulse" (say "Ion Pulse Blaster"))))
                  ("Phaser"
                    (make-menu
                      ("_Stun" (say "Phaser = Stun"))
                      ("_Maim" (say "Phaser = Maim"))
                      ("_Frappe" (say "Phaser = Frappe")))))))))])
  (set-menu! top menu)
  (show lb))

) ; end tutorial
)

