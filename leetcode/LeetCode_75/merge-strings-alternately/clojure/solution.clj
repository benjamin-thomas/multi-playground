; echo ./solution.clj | entr -c clojure -M /_

;; Calva shortcuts

;; barf backwards : ctrl+shift+alt+right
;; barf forwards : ctrl+alt+,

;; slurp backwards : ctrl+shift+alt+left
;; slurp forwards : ctrl+alt+.

;; start a REPL session: ctrl+alt+c ctrl+alt+j
;; send form to the REPL (form inside): ctrl+shift+enter

(ns solution)

;; =======
;; TESTING
;; =======

(defn red [str]
   (format "\033[31m%s\033[0m" str))

(defn green [str]
   (format "\033[32m%s\033[0m" str))

(defn assert-equal
  [a b]
  (if (= a b)
    (println (green (format "🟢 PASS: %s" a)))
    (println (  red (format "🔴 FAIL: %s != %s" a b)))))

;; =======
;; UTILITY
;; =======

(defn interleave-all
  "Like interleave, but stops when the longest seq is done, instead of
      the shortest.

      URL: https://ask.clojure.org/index.php/12125/add-interleave-all-like-partition-all-is-to-partition
      "
  {:copyright "Rich Hickey, since this is a modified version of interleave"}

  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (cond
        (and s1 s2) ; there are elements left in both
        (cons (first s1) (cons (first s2)
                               (interleave-all (rest s1) (rest s2))))
        s1 ; s2 is done
        s1
        s2 ; s1 is done
        s2))))
  ([c1 c2 & colls]
   (lazy-seq
    (let [ss (filter identity (map seq (conj colls c2 c1)))]
      (concat (map first ss) (apply interleave-all (map rest ss)))))))

;; =======
;; PROGRAM
;; =======

(defn merge-str
  [s1 s2]
  (apply str (interleave-all (char-array s1) (char-array s2))))

(comment

  (defn interleave+ [& x]
    (take (* (count x) (apply max (map count x)))
          (apply interleave (map cycle x))))


  (merge-str "ABC" "abc")
  (merge-str "ABC" "abc_def")

  (println (merge-str "ABC" "abc"))

  (def a [:a :c :e :f :g])
  (def b [:b :d])
  (interleave a b)
  (cons (first a) (interleave b (rest a)))
  (butlast (interleave a (conj b nil)))

  (interleave+ a b)
  (interpose a b)
  (interleave-all a b)
  (interleave-all [0 2 4] [1 3 5 6 7 8])
  (interleave-all (chars (char-array "ABC")) (chars (char-array "abc_def")))

  :rcf)

;; =======
;; TESTING
;; =======

(assert-equal "AaBbCc" (merge-str "ABC" "abc"))
(assert-equal "AaBb_cde" (merge-str "AB" "ab_cde"))
(assert-equal "AaBb_CDE" (merge-str "AB_CDE" "ab"))
(assert-equal "ABC" (merge-str "ABC" ""))
(assert-equal "abc" (merge-str "" "abc"))
(assert-equal "" (merge-str "" ""))

(println "DONE!")
