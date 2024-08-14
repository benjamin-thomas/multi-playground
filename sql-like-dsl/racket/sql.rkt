#lang racket

#|
Run with:
    racket ./sql.rkt

NOTE:
The table and column names are checked at compile time (try disabling print)
 |#

(define-syntax-rule (where table condition)
  (filter
   (lambda (row) (condition row))
   table))

(struct post
  (id title author published) #:transparent)

(struct user
  (id name age is-subscribed) #:transparent)

(define users
  (list (user 1 "David" 20 #t)
        (user 2 "Jen"   30 #t)
        (user 3 "Paul"  50 #f)))

(define posts
  (list (post 1 "Racket"  "David" #t)
        (post 2 "Clojure" "Jen"   #t)
        (post 3 "Lisp"    "Paul"  #f)
        (post 4 "Python"  "David" #t)
        (post 5 "Haskell" "Jen"   #t)
        (post 6 "OCaml"   "Paul"  #f)))

(define (find-subscribed-users)
  (where users (lambda (row) (user-is-subscribed row))))

(define (find-older-users)
  (where users (lambda (row) (>= 30 (user-age row)))))

(define (find-latest-posts)
  (where posts (lambda (row) (>= 3 (post-id row)))))

(define *do-print* #f)
(set! *do-print* #t)

(when *do-print*
  (displayln "Subscribed users")
  (displayln "----------------")
  (println (find-subscribed-users))

  (newline)
  (displayln "Older users")
  (displayln "------------")
  (println (find-older-users))

  (newline)
  (displayln "Latest posts:")
  (displayln "-------------")
  (println (find-latest-posts))
  )
