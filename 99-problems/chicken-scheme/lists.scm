#|

Chicken Scheme (compiles to C)

  REPL:
    csi ./lists.scm

  COMPILE
    - csi -s ./lists.scm
    - csc -o lists./lists.scm && ./lists

  TDD:
    echo ./lists.scm | entr -c csi -s /_

---

Install the testing framework: http://wiki.call-cc.org/eggref/5/srfi-78

  chicken-install srfi-78, as root (?)
  See: https://wiki.call-cc.org/man/4/Extensions#changing-repository-location

---

Sources:

    * http://community.schemewiki.org/?ninety-nine-scheme-problems
    * https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html

|#

(import srfi-78)
(check-set-mode! 'report-failed)


; ===================================
; P01 (*) Find the last box of a list
; ===================================
(define (last lst)
  (foldl (lambda (acc x) x)
	 '()
	 lst))

(check (last '(1 2 3)) => 3)

; OMG ;)
(define (last2 lst)
  (eval (cons 'and lst)))

(check (last2 '(1 2 3 4)) => 4)

; ============================================
; P02 (*) Find the last but one box of a list.
; ============================================
(define (penultimate lst)
  (cadr (foldl (lambda (acc x) (cons x acc))
	       '()
	       lst)))

(check (penultimate '(1 2 3 4 5)) => 4)

(define (penultimate2 lst)
  (when (not (null? lst)) ; guard against 0 element lists (would crash otherwise) --> is this appropriate?
    (when (not (null? (cdr lst))) ; guard against 1 element lists (would crash otherwise)
      (if (null? (cddr lst)) ; if list has 2 elements
	  (car lst)
	  (penultimate2 (cdr lst))))))

(define (pen3 lst)
  (when (not (null? lst))
    (penultimate2 lst))
  )

(check (penultimate2 '(1 2 3 4 5)) => 4)


;(check-report)
