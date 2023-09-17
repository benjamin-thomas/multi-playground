(ns problems.core)

;; barf backwards : ctrl+alt-s
;; barf forwards : alt+shift+t

;; slurp backwards : ctrl+alt+t
;; slurp forwards : alt+shift+s

;; send form te REPL: ctrl+è
;; jump to REPL : ctrl+à

;; switch repl ns to current file: alt+shift+r
;; sync files with REPL: alt+shift+m

; Write a function last : 'a list -> 'a option that returns the last element of a list
(defn my-last [lst]
  (if (empty? (rest lst))
    (first lst)
    (my-last (rest lst))))

(defn my-last2 [lst]
  (cond
    (empty? lst) nil
    (empty? (rest lst)) (first lst)
    :else (recur (rest lst))))

(defn my-last3 [lst]
  (let [rest' (rest lst)]
    (cond
      (empty? lst) nil
      (empty? rest') (first lst)
      :else (recur rest'))))

(defn my-last4 [lst]
  (reduce (fn [_acc x] x) nil lst))
