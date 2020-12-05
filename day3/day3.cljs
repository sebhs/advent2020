(require '[clojure.string :as str])

(defn read-file [f]
  (-> (slurp f)
      (str/split-lines)))

(defn has-tree? [line index]
  (let [times (+ 1 (quot index (count line)))
        repeated-line (clojure.string/join (repeat times line))]
    (if (= (nth repeated-line index) \#)
      true
      false)))

(defn count-trees [right down]
  (let [data-v (read-file "day3.txt")
        length (count data-v)]
    (loop [trees 0
           index-x right
           index-y down]
      (if (>= index-y length)
        trees
        (recur (if (has-tree? (get data-v index-y) index-x)
                 (+ trees 1)
                 trees)
               (+ index-x right)
               (+ index-y down))))))

(defn solve []
  (let [part1 (count-trees 3 1)
        part2 (* (count-trees 1 1) (count-trees 3 1) (count-trees 5 1) (count-trees 7 1) (count-trees 1 2))]
    (println "Part 1: " part1)
    (println "Part 2: " part2)))

(solve)



