(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(defn setup [file]
  (with-open [reader (io/reader file)]
    (mapv (fn [line]
            (->> (str/split line #" ")
                 (map (fn [level] (Integer/parseInt level)))
                 (into [])))
          (line-seq reader))))

(defn check-report [report]
  (loop [current (second report)
         previous-difference (- (first report) current)
         levels (rest report)]
    (if (empty? (rest levels))
      (and
       (<= (abs previous-difference) 3)
       (not= previous-difference 0))
      (if (or
           (> (abs previous-difference) 3)
           (= previous-difference 0)
           (< (bit-xor (- current (second levels)) previous-difference) 0))
        false
        (recur
         (second levels)
         (- current (second levels))
         (rest levels))))))

(defn solve-part-one [file]
  (reduce (fn [total report]
            (if (check-report report)
              (inc total)
              total))
          0
          (setup file)))

(defn solve-part-two [file]
  (reduce (fn [total report]
            (if (first (filter (fn [i]
                                 (check-report (concat (subvec report 0 i)
                                                       (subvec report (inc i)))))
                               (range (count report))))
              (inc total)
              total))
          0
          (setup file)))

(println (solve-part-one "input.txt"))
(println (solve-part-two "input.txt"))
