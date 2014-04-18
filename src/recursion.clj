(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll))))) ; so tempting (apply * coll)

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (< (count seq-2) (count seq-1))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (not (empty? a-seq)) (empty? b-seq)) false
    (and (empty? a-seq) (not (empty? b-seq))) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (take (count a-seq) (lazy-rotate a-seq))))

(defn rotate [a-seq]
  (concat (rest a-seq) [(first a-seq)]))

(defn n-rotations [a-seq, rotations]
  (if (< rotations 2)
    [a-seq]
    (concat [a-seq] (n-rotations (rotate a-seq) (- rotations 1)))))

(defn lazy-rotate [a-seq]
  (cons a-seq (lazy-seq (lazy-rotate (rotate a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (update-in freqs [(first a-seq)] (fnil inc 0)) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat (fn [[e n]] (repeat n e)) a-map))

(defn un-frequencies2 [a-map]
  (if (empty? a-map)
    []
    (let [current (apply repeat (reverse (first a-map)))]
      (concat current (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (zero? n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
    (split-at (int (/ (count a-seq) 2)) a-seq))

(defn halve2 [a-seq]
  (let [split-point (int (/ (count a-seq) 2))]
    [(take split-point a-seq) (drop split-point a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< a b) (cons a (seq-merge (rest a-seq) b-seq))
      :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [splitted (halve a-seq)]
      (seq-merge (merge-sort (first splitted)) (merge-sort (last splitted))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [longest-so-far (longest-monotone-seq (reverse (inits a-seq)))
          length-so-far (count longest-so-far)
          to-go (drop length-so-far a-seq)]
          (concat [longest-so-far] (split-into-monotonics to-go)))))

(defn longest-monotone-seq [seqs]
  (first (filter (fn [s] (or (apply >= s) (apply <= s))) seqs)))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

