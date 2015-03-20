(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [p] (< p n)))

(defn equal-to [n]
  (fn [p] (== p n)))

(defn set->predicate [a-set]
  (fn [i] (contains? a-set i)))

(defn pred-and [pred1 pred2]
  (fn [i] (and (pred1 i) (pred2 i))))

(defn pred-or [pred1 pred2]
  (fn [i] (or (pred1 i) (pred2 i))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (or (empty? string)
      (every? whitespace? string)))

(defn has-award? [book award]
  (boolean (award (:awards book))))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (let [book-has-award? (fn [award] (has-award? book award))]
    (every? book-has-award? awards)))

(defn my-some [pred a-seq]
  (let [pred-map (map pred a-seq)
        filtered-pred-map (filter (complement false?) pred-map)
        first-item (first filtered-pred-map)]
    (cond
      (= nil first-item) false
      (true? first-item) true
      :else first-item)))

(defn my-every? [pred a-seq]
  (let [pred-map (map pred a-seq)
        filtered-pred-map (filter (complement false?) pred-map)]
    (if (empty? a-seq) true
                       (= (count a-seq) (count filtered-pred-map)))))

(defn prime? [n]
  (let [pred (fn [i] (= (mod n i) 0))]
    (not (some pred (range 2 n)))))
;^^
