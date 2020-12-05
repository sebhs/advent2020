(require '[clojure.string :as str])

(defn only-digits? [s]
  (every? #(Character/isDigit %) s))

(defn valid-year-between? [s min max]
  (and (only-digits? s) (= (count s) 4) (>= (Integer. s) min) (<= (Integer. s) max)))

(defn valid-height? [s min-in max-in min-cm max-cm]
  (let [unit (subs s (- (count s) 2) (count s))
        value (subs s 0  (- (count s) 2))]
    (if (and (only-digits? value) (or (= unit "cm") (= unit "in")))
      (if (= unit "cm")
        (and (>= (Integer. value) min-cm) (<= (Integer. value) max-cm))
        (and (>= (Integer. value) min-in) (<= (Integer. value) max-in)))
      false)))

(defn valid-color? [s]
  (if (and (= (first s) \#) (re-find #"^[a-fA-F0-9]+$" (apply str (rest s))))
    true
    false))

(def keys-validator-fn {"byr" #(valid-year-between? % 1920 2002)
                        "iyr" #(valid-year-between? % 2010 2020)
                        "eyr" #(valid-year-between? % 2020 2030)
                        "hgt" #(valid-height? % 59 76 150 193)
                        "hcl" #(valid-color? %)
                        "ecl" #(or (= % "amb") (= % "blu") (= % "brn") (= % "gry") (= % "grn") (= % "hzl") (= % "oth"))
                        "pid" #(and (only-digits? %) (= (count %) 9))
                        "cid" #(or true %)})

(defn read-file [f]
  (mapv #(str/split % #" |\n")
        (-> (slurp f)
            (str/split #"\n\n"))))

(defn contains-cid? [passport]
  ((complement nil?) (some #(= "cid" %) (mapv #(first (str/split % #":")) passport))))

(defn fields-valid? [passport]
  (let [m (into {} (mapv #(str/split % #":") passport))
        valid? (atom true)]
    (doseq [[k v] m]
      (when ((complement (keys-validator-fn k)) v)
        (reset! valid? false)))
    @valid?))


(defn valid-passport? [passport part1?]
  (let [passport-length (count passport)]
    (cond
      (= passport-length 8) (or part1? (fields-valid? passport))
      (= passport-length 7) (and ((complement contains-cid?) passport) (or part1? (fields-valid? passport)))
      (< passport-length 7) false
      :else false)))

(defn count-valid-passports [part1?]
  (let [passport-matrix (read-file "day4.txt")]
    (count (filter identity (map #(valid-passport? % part1?) passport-matrix)))))

(defn solve []
  (println "Part 1:" (count-valid-passports true))
  (println "Part 2:" (count-valid-passports false)))

(solve)

