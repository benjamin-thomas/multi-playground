;; lein run -m main 16 < <(echo -e "ABC\ndef\nXY2Z\nFF")

(ns main
  (:require [clojure.string :as str]))

(def red "\u001B[31m")
(def reset "\u001B[0m")

(defn to-base10 [c]
  (let [c (char c)]
    (cond
      (Character/isDigit c)
      (- (int c) (int \0))

      (and (>= (int c) (int \A))
           (<= (int c) (int \F)))
      (+ 10 (- (int c) (int \A)))

      (and (>= (int c) (int \a))
           (<= (int c) (int \f)))
      (+ 10 (- (int c) (int \a)))

      :else nil)))

(defn horner [base nums]
  (reduce (fn [acc n] (+ n (* base acc)))
          0
          nums))

(defn convert-to-base10 [line]
  (reduce (fn [[good bad] c]
            (let [n (to-base10 c)]
              (if n
                [(conj good n) bad]
                [good (conj bad c)])))
          [[] []]
          line))

(defn main-loop [base]
  (let [line (read-line)]
    (when line
      (let [[good bad] (convert-to-base10 line)]
        (println (str "[" (str/join "," (map str line)) "]"))
        (if (empty? bad)
          (do
            (println (reverse good)) ; Just for clarity
            (println (str " -> " (horner base good))))
          (println (str red " => Bad chars: " (apply str bad) reset)))
        (println "")
        (recur base)))))

(defn -main [& args]
  (if (= 1 (count args))
    (let [base (Integer/parseInt (first args))]
      (println (str "Base is: " base))
      (println "---")
      (main-loop base)
      (println "Done!"))
    (println "Usage: ./Main BASE")))
