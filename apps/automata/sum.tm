;; This machine computes the sum of two numbers represented in unary
;; and separated on the tape by a single blank.

(lambda (machine)

(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(import turing-machine)

(define q5 (create <state> machine 'q5 238 297))
(define q4 (create <state> machine 'q4 358 254))
(define q3 (create <state> machine 'q3 352 151))
(define q2 (create <state> machine 'q2 272 209))
(define q1 (create <state> machine 'q1 159 196))
(define q0 (create <state> machine 'q0 54 160))
(let ([x (create <transition> machine q4 'blank 'right q5)])
  (send x suggest-points '())
  (send x show-label 302 274))
(let ([x (create <transition> machine q4 '1 'left q4)])
  (send x
        suggest-points
        '(372.5489263833234
           247.34201673983506
           417
           227
           418
           281
           372.5907440828017
           260.56583483726075))
  (send x show-label 418 252))
(let ([x (create <transition> machine q3 'blank 'left q4)])
  (send x suggest-points '())
  (send x show-label 353 202))
(let ([x (create <transition> machine q2 '1 'blank q3)])
  (send x suggest-points '())
  (send x show-label 302 185))
(let ([x (create <transition> machine q1 'blank 'left q2)])
  (send x suggest-points '())
  (send x show-label 209 202))
(let ([x (create <transition> machine q1 '1 'right q1)])
  (send x
        suggest-points
        '(154.5529426623224
           142.95574640069995
           139
           100
           183
           100
           165.89801152452998
           143.12675354683748))
  (send x show-label 151 138))
(let ([x (create <transition> machine q0 'blank '1 q1)])
  (send x suggest-points '())
  (send x show-label 97 176))
(let ([x (create <transition> machine q0 '1 'right q0)])
  (send x
        suggest-points
        '(48.53211099061273
           144.963305224185
           34
           105
           79
           105
           60.62084710881894
           145.43413636059833))
  (send x show-label 50 105))
)
