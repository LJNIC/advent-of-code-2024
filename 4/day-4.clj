(require '[clojure.string :as str])

(defn setup [file]
  (str/split (slurp file) #"\n"))

(defn next-letter [current-letter]
  (case current-letter
    \X \M
    \M \A
    \A \S
    \S nil
    nil))

(def directions '([-1 -1] [0 -1] [1 -1]
                  [-1 0]         [1 0]
                  [-1 1]  [0 1]  [1 1]))

(defn get-char [grid position]
  (get (get grid (first position)) (second position)))

(defn find-xmas [grid position direction looking-for]
  (let [found (get-char grid position)
        matched (= looking-for found)
        looking-for-next (next-letter looking-for)]
    (cond
      (not matched) 0
      (nil? looking-for-next) (if matched 1 0)
      :default (find-xmas grid (mapv + position direction) direction looking-for-next))))

(defn solve-part-one [file]
  (let [grid (setup file)
        height (count grid)
        width (count (first grid))
        positions (for [x (range width)
                        y (range height)
                        :when (= \X (get-char grid [y x]))]
                    [y x])]
    (reduce + (for [position positions]
                (reduce +
                        (map (fn [direction]
                               (find-xmas grid (mapv + position direction) direction \M))
                             directions))))))

(defn find-x-mas [grid position]
  (let [bleft-to-right
        (str (get-char grid (mapv + position [1 -1]))
             (get-char grid (mapv + position [-1 1])))
        tleft-to-right
        (str (get-char grid (mapv + position [-1 -1]))
             (get-char grid (mapv + position [1 1])))]
    (if
     (and
      (case bleft-to-right
        "SM" true
        "MS" true
        false)
      (case tleft-to-right
        "SM" true
        "MS" true
        false))
      1
      0)))

(defn solve-part-two [file]
  (let [grid (setup file)
        height (count grid)
        width (count (get grid 0))
        positions (for [y (range height)
                        x (range height)
                        :when (= \A (get-char grid [y x]))]
                    [y x])]
    (reduce + (map (fn [position] (find-x-mas grid position)) positions))))

(println (solve-part-one "input.txt"))
(println (solve-part-two "input.txt"))
