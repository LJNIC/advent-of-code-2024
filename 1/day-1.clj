(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(defn setup [file]
  (with-open [reader (io/reader file)]
    (loop [lines (line-seq reader)
           a []
           b []
           counts {}]
      (if (empty? lines)
        {:a a :b b :counts counts}
        (let [line-split (str/split (first lines) #"   ")
              b-int (Integer/parseInt (second line-split))]
          (recur
            (rest lines)
            (conj a (Integer/parseInt (first line-split)))
            (conj b b-int)
            (assoc counts b-int (inc (get counts b-int 0)))))))))

(defn solve-part-one [file] 
  (loop [data (setup file)
         a (sort (:a data))
         b (sort (:b data))
         distance 0]
    (if (empty? a)
      distance
      (recur
         data
         (rest a)
         (rest b)
         (+ distance (abs (- (first a) (first b))))))))

(defn solve-part-two [file]
  (let [{counts :counts
         a :a} (setup file)]
    (reduce (fn [distance el] (+ distance (* el (get counts el 0)))) 0 a)))

(println (solve-part-two "input.txt"))
