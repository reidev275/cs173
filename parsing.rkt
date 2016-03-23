#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define (desugar [a : ArithS]) : ArithC
  (type-case ArithS a
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

(define negFive (uminusS (numS 5)))

(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define seven (parse '(+ (* 1 2) (+ 2 3))))
(test seven (plusS (multS (numS 1) (numS 2)) (plusS (numS 2) (numS 3))))

(define three (parse '(- 5 2)))
(test three (bminusS (numS 5) (numS 2)))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))
(test (interp (desugar seven)) 7)
(test (interp (desugar three)) 3)
(test (interp (desugar negFive)) -5)




