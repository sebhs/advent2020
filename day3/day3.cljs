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

(defn count-trees []
  (let [data-v (read-file "day3.txt")
        length (count data-v)]
    (loop [trees 0
           index-y 1
           index-x 3]
      (if (>= index-y length)
        trees
        (recur (if
                (has-tree? (get data-v index-y) index-x)
                 (+ trees 1)
                 trees)
               (+ index-y 1)
               (+ index-x 3))))))

(print (count-trees))



