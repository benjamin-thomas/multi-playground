(ns aoc.core
  (:require [clojure.string :as str]))

;; IntelliJ shortcuts
;; ==================
;; barf backwards : ctrl+alt-s
;; barf forwards : alt+shift+t

;; slurp backwards : ctrl+alt+t
;; slurp forwards : alt+shift+s

;; send form to the REPL: ctrl+è
;; jump to REPL : ctrl+à

;; switch repl ns to current file: alt+shift+r
;; sync files with REPL: alt+shift+m

;; VS Code shortcuts
;; ==================
;; barf backwards : ???
;; barf forwards : ???

;; slurp backwards : ???
;; slurp forwards : ???

;; send form to the REPL: alt+enter
;; jump to REPL : ???

;; The example block of text
(defn example []
  "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
   ")


(defn add
  "Addition"
  [a b]
  (+ a b))

(defn mul
  "Multiplication"
  [a b]
  (* a b))

(defn div
  "Division"
  [a b]
  (/ a b))

(comment
  (require '[clojure.string :as str])

  (add 1 2)
  (mul 3 4)
  (div 12 4)

  (str/split-lines (example))

  ;; same but use threading macros
  (-> (example)
      (str/trim)
      (str/split-lines)
      (char-array))

  :rfc)