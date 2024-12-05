(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(defn setup [file]
  (slurp file))

(defn solve-part-one [file]
  (let [contents (setup file)
        mults (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" contents)]
    (->> mults
         (map (fn [mult]
                (list (Integer/parseInt (second mult)) (Integer/parseInt (second (rest mult))))))
         (reduce (fn [total pair]
                   (+ total (* (first pair) (second pair)))) 0))))

(defn solve-part-two [file]
  (let [contents (setup file)
        commands (map (fn [match] (into [] match)) (re-seq #"(don't\(\)|do\(\)|mul\((\d{1,3}),(\d{1,3})\))" contents))]
    (loop [commands commands
           on true
           total 0]
      (if (empty? commands)
        total
        (let [command (first commands)]
          (cond
            (= (first command) "do()") (recur (rest commands) true total)
            (= (first command) "don't()") (recur (rest commands) false total)
            :else (recur (rest commands)
                         on
                         (if on
                           (+ total (* (Integer/parseInt (get command 2)) (Integer/parseInt (get command 3))))
                           total))))))))

(println (solve-part-two "input.txt"))
