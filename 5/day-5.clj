(require '[clojure.string :as str])
(require '[clojure.java.io :as io])
(require '[clojure.set :as set])

(defn map-rule [rules line]
  (let [rule (mapv #(Integer/parseInt %) (str/split line #"\|"))
        left (first rule)
        right (second rule)
        right-before (get rules right #{})]
    (assoc rules right (conj right-before left right))))

(defn create-update [line]
  (apply vector (reverse (mapv #(Integer/parseInt %) (str/split line #",")))))

(defn setup [file]
  (with-open [reader (io/reader file)]
    (loop [lines (line-seq reader)
           rules {}
           updates '()]
      (let [line (first lines)]
        (cond
          (empty? lines) {:rules rules :updates updates}
          (= (count line) 5) (recur (rest lines) (map-rule rules line) updates)
          (> (count line) 5) (recur (rest lines) rules (conj updates (create-update line))))))))

(defn update-valid? [up-date rules]
  (let [the-rest (rest up-date)]
    (cond
      (empty? the-rest) true
      (set/superset? (get rules (first up-date)) the-rest) (update-valid? the-rest rules)
      :else false)))

(defn solve-part-one [file]
  (let [{updates :updates
         rules :rules} (setup file)]
    (->> updates
         (filter (fn [up-date] (update-valid? up-date rules)))
         (map (fn [up-date] (get up-date (int (/ (count up-date) 2)))))
         (reduce +))))

(defn fix-update [in-progress up-date rules]
  (if (= 1 (count up-date))
    (conj in-progress (first up-date))
    (let [next-up
          (first (filter (fn [page] (set/superset? (get rules page) up-date)) up-date))]
      (fix-update (conj in-progress next-up) (apply vector (remove #{next-up} up-date)) rules))))

(defn solve-part-two [file]
  (let [{updates :updates
         rules :rules} (setup file)]
    (->> updates
         (filter (fn [up-date] (not (update-valid? up-date rules))))
         (map (fn [up-date] (fix-update [] up-date rules)))
         (map (fn [up-date] (get up-date (int (/ (count up-date) 2)))))
         (reduce +))))

(println (solve-part-one "input.txt"))
(println (solve-part-two "input.txt"))
