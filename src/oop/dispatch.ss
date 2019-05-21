;; Copyright (c) 1996 R. Kent Dybvig, Carl Bruggeman, and Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


;;; swl:dispatch-table implementation based on object-lambda
;; for internal use only
;;; quick-hack mode on for now

;;; (swl:dispatch-table delegate [msg args body] ...)
;;;    delegate   is a binding for another swl:dispatch-table (gives inheritance)
;;; a variant of object-lambda

;;; (object-lambda self [msg args body] ...)
;;;    self   is optional, and if present must be an identifier.
;;;           If supplied, it is a name by which the object may
;;;           refer to itself.
;;;    msg    is any Scheme object and serves as a message
;;;           specifier distinguished using eqv?.
;;;    args   is a lambda list specifying the message arguments.
;;;    body   is a lambda body (zero or more definitions followed
;;;           by at least one expression.

;;; An object-lambda expression evaluates to a procedure.  When a
;;; procedure created by object-lambda is invoked, at most one clause
;;; is selected.  A clause is selected by comparing the incoming message
;;; (first argument) and arity (count of arguments excluding the message)
;;; against the messages and accepted arities of each clause, in
;;; left-to-right order.  The first matching clause is selected.  If no
;;; clause is selected, an error is signaled.  If a clause is selected,
;;; its formal parameters (args) are bound to the actual paramters
;;; (exclusive of the message argument) within body.

;;; The same message specifier may appear in more than one object-lambda
;;; clauses, provided that the clauses have different arities.  If two
;;; clauses have the same message specifier and arity, the first always
;;; takes precedence, and the second is therefore unreachable.  Similarly,
;;; if two clauses have the same message specifier, and the first has
;;; a more general interface (necessarily a "dot" interface), the first
;;; again always takes precedence and the second is unreachable.

;;; An object-lambda expression expands into a case-lambda expression,
;;; each clause of which contains a case expression.  Each clause of
;;; the object-lambda expression is represented by one case within
;;; each of one or more case-lambda clauses.  Minimal reordering of
;;; clauses is done to preserve the user-specified ordering to allow
;;; programmers to base ordering on predicted message frequency for
;;; efficiency.  Reordering is done only to allow correct left-to-right
;;; ordering of messages.

;;; The translation algorithm works as follows:
;;;   * The set of required case-lambda clauses is determined from
;;;     the set of object-lambda clauses.  A clause accepting zero or
;;;     more arguments is always created to serve as an "else"
;;;     to catch invalid invocations.  The clauses are reordered only
;;;     as necessary to place more specific interfaces in front of
;;;     less specific (but comparable) interfaces.
;;;   * Cases are inserted into case-lambda clauses by a left-to-right
;;;     traversal of the object-lambda clauses.  An object-lambda
;;;     clause with a "dot" interface may be represented by a case
;;;     in more than one case-lambda clause.  Warnings are issued
;;;     for unreachable object-lambda clauses, which are otherwise
;;;     discarded.
;;;   * The output case-lambda expression is constructed, wrapped in a
;;;     let expression containing temporary bindings for duplicated
;;;     object-lambda clauses.  Each clause of the case-lambda
;;;     expression consists of a case expression.  An else clause
;;;     is included for each case expression to catch and signal
;;;     invocation errors.

;; not intended to be exposed to user like object-lambda
(define-syntax swl:dispatch-table
  (let ()
    (import scheme)

    (define arity
      ;; (arity '(x1 ... xn)), n>=0 => n
      ;; (arity 'r) => -1
      ;; (arity '(x1 ... xn . r)), n>=1 => -(n+1)
      (lambda (args)
        (let f ([args args] [n 0])
          (if (pair? args)
              (f (cdr args) (+ n 1))
              (if (null? args)
                  n
                  (- -1 n))))))
    
    (define flatten-args
      ;; (flatten-args '(x1 ... xn)), n>=0 => (x1 ... xn)
      ;; (flatten-args 'r) => (r)
      ;; (flatten-args '(x1 ... xn . r)), n>=1 => (x1 ... xn r)
      (lambda (args)
        (let f ([x args])
          (syntax-case x ()
            [(x . y) (with-syntax ([rest (f (syntax y))])
                       (syntax (x . rest)))]
            [()      (syntax ())]
            [x        (syntax (x))]))))
    
    (define build-args
      ;; (build-args n), n>=0 => (g1 ... gn)
      ;; (build-args -(n+1)), n>=1 => (g1 ... gn . g)
      ;; (build-args -1) => g
      (lambda (a)
        (if (< a 0)
            (let f ((a a))
              (if (= a -1)
                  (gentmp)
                  (cons (gentmp) (f (+ a 1)))))
            (generate-temporaries (make-list a #f)))))
    
    (define gentmp
      (lambda ()
        (car (generate-temporaries '(#f)))))
    
    (define-structure (objcase msg arity proc source)
      ([clauses '()]
       [temp #f]))
    
    (define-structure (clause arity args)
      ([cases '()]))
    
    (define build-clauses
      ;; create list of clauses needed by the cases
      ;; reorder only if necessary to place a more specific case in
      ;; front of a less specific case (assumes sort performs a
      ;; stable sort)
      (lambda (ocases)
        (sort (lambda (x y)
                ; returns #t only if x must be placed before y
                (let ([ax (clause-arity x)] [ay (clause-arity y)])
                  (and (< ay 0)
                       (or (>= ax 0)
                           (< ax ay)))))
              (let f ([ocases ocases] [arities '()])
                (if (null? ocases)
                    (if (memv -1 arities)
                        '()
                        (cons (make-clause -1 (gentmp)) '()))
                    (let ([a (objcase-arity (car ocases))])
                      (if (memv a arities)
                          (f (cdr ocases) arities)
                          (cons (make-clause a (build-args a))
                                (f (cdr ocases) (cons a arities))))))))))
   (define mem
     (lambda (pred? ls)
       (unless (procedure? pred?)
         (assertion-violationf 'mem "~s is not a procedure" pred?))
       (let f ([l ls])
         (cond
           [(pair? l) (if (pred? (car l)) l (f (cdr l)))]
           [(null? l) #f]
           [else (assertion-violationf 'mem "~s is not a proper list" ls)]))))
 
    (define memid
      ;; like memq, but for identifiers
      (lambda (id ls)
        (mem (lambda (x) (free-identifier=? x id))
             ls)))
    
    (define insert-case
      ;; insert ocase into clause if ocase arity matches clause arity
      ;; and ocase message not already represented
      (lambda (ocase clause)
        (let ([a (objcase-arity ocase)] [b (clause-arity clause)])
          (when (and (if (>= a 0) (= a b) (if (>= b 0) (<= (- -1 a) b) (>= a b)))
                     (not (memid (objcase-msg ocase)
                                 (map objcase-msg (clause-cases clause)))))
            (set-objcase-clauses! ocase (cons clause (objcase-clauses ocase)))
            ; need to reverse cases later to preserve programmer order
            (set-clause-cases! clause (cons ocase (clause-cases clause)))))))
    
    (define build-output-bindings
      ;; create bindings from temp to lambda expression for all cases
      ;; that need them (those with nonfalse temp)
      (lambda (ocases)
        (if (null? ocases)
            '()
            (let ([temp (objcase-temp (car ocases))])
              (if temp
                  (cons `(,temp ,(objcase-proc (car ocases)))
                        (build-output-bindings (cdr ocases)))
                  (build-output-bindings (cdr ocases)))))))
    
    (define build-output-args
      ;; create the arguments for a call of arity a from arguments args.
      (lambda (a args)
        (if (>= a 0)
            args
            (let f ([a a] [args args])
              (if (= a -1)
                  (if (list? args) ; proper list?
                      (with-syntax ([(arg ...) args])
                        (syntax ((list arg ...))))
                      (with-syntax ([(arg ...) (flatten-args args)])
                        (syntax ((list* arg ...)))))
                  (cons (car args) (f (+ a 1) (cdr args))))))))
    
    (define build-ocase-clause
      (lambda (ocase clause)
        (with-syntax ([obj-msg (objcase-msg ocase)]
                      [obj-tmp (or (objcase-temp ocase) (objcase-proc ocase))]
                      [(out-arg ...) (build-output-args (objcase-arity ocase)
                                                        (clause-args clause))])
          (syntax
           [(obj-msg)
            (obj-tmp self out-arg ...)]))))
    
    (define build-output-clause
      ;; generate one case-lambda clause
      (lambda (delegate-id clause)
        (with-syntax ([delegate delegate-id]
                      [args (clause-args clause)]
                      [(cl ...) (map (lambda (ocase)
                                       (build-ocase-clause ocase clause))
                                     (reverse (clause-cases clause)))])
          (if (negative? (clause-arity clause))
              (with-syntax ((delegate-args (flatten-args (clause-args clause))))
                (syntax
                 ((msg inst . args)
                  (fluid-let-syntax ((self (identifier-syntax inst)))
                    (case msg
                     cl ...
                     (else (apply delegate msg inst . delegate-args)))))))
              (syntax
               ((msg inst . args)
                (fluid-let-syntax ((self (identifier-syntax inst)))
                  (case msg
                   cl ...
                   (else (delegate msg inst . args))))))))))
    
    (define mk-ocase
      (lambda (self-id)      ; make sure we fluid-let-syntax the right self
        (lambda (s)          ; in case interaction-environment has changed
          (syntax-case s ()
            [(msg args e ...)
             (with-syntax ([(flat-arg ...) (flatten-args #'args)])
               (make-objcase
                 #'msg
                 (arity (syntax-object->datum #'args))
                 (with-syntax ([self self-id])
                   #'(lambda (init flat-arg ...)
                       (fluid-let-syntax ([self
                                           (identifier-syntax init)])
                         e
                         ...)))
                 #'(msg args e ...)))]))))
    
    (define build-dispatch-table
      (lambda (self-id delegate-id srcs)
        (let ([ocases (map (mk-ocase self-id) srcs)])
          (let ([clauses (build-clauses ocases)])
            (for-each (lambda (ocase)
                        (for-each (lambda (clause) (insert-case ocase clause))
                                  clauses))
                      ocases)
            (for-each (lambda (ocase)
                        (if (null? (objcase-clauses ocase))
                            (warningf 'swl:dispatch-table
                                     "unreachable case~n~s"
                                     (syntax-object->datum (objcase-source ocase)))
                            (unless (null? (cdr (objcase-clauses ocase)))
                              (set-objcase-temp! ocase (gentmp)))))
                      ocases)
            (with-syntax ([(binding ...) (build-output-bindings ocases)]
                          [(clause ...) (map (lambda (clause)
                                               (build-output-clause
                                                  delegate-id clause))
                                             clauses)])
              (syntax (let (binding ...)
                        (case-lambda clause ...))))))))

     (lambda (form)
      (syntax-case form ()
        [(_ self delegate (msg args e1 e2 ...) ...)
         (with-syntax ([exp (build-dispatch-table (syntax self)
                              (syntax delegate)
                              (syntax ((msg args e1 e2 ...) ...)))])
           (syntax exp))]))))

