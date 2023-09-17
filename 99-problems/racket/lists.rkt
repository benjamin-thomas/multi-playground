#lang racket

(require rackunit)

#|

Sources:

    - http://community.schemewiki.org/?ninety-nine-scheme-problems
    - https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html

---

Run the tetst:

    rg --files | entr -c raco test /_
    echo ./lists.rkt | entr -c bash -c 'racket ./lists.rkt && echo OK'

---

Load a REPL:

    racket --repl --eval '(enter! (file "./lists.rkt"))'
|#

(define (add a b)
  (+ a b))
(check-equal? 3 (add 1 2))

; P01 (*) Find the last box of a list.
(define (my-last lst)
  (if (null? (cdr lst)) (car lst) (my-last (cdr lst))))

(define (my-last2 lst)
  (foldl (lambda (x _acc) x) null lst))

(check-equal? (my-last '(a b c d)) 'd)
(check-equal? (my-last2 '(a b c d)) 'd)
(check-equal? (my-last2 '()) '())
