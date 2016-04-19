(ns recursion)

(defn product [coll]
  (if (empty? coll)
  1
  (* (first coll)
     (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false (if (empty? (rest coll)) true false)))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (if (empty? a-seq)
    nil
    (max (first a-seq)
       (my-last (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
       (my-last (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [my-cons (fn [x y]
        (if (nil? x) y (cons x y)))]
  (if (empty? a-seq)
    a-seq
     (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq))) (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [new '()]
    (cond
      (empty? a-seq)
      new
      (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      :else
      new)))
      

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    '()
    (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    :else
    (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (let [new '()]
    (cond
      (or (empty? seq-1) (empty? seq-2)) new
        :else
        (let [test (f (first seq-1) (first seq-2))]
          (cons test (my-map f (rest seq-1) (rest seq-2)))))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n)
    0
    (== 1 n)
    1
    :else
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond 
    (<= how-many-times 0)
    '()
    :else
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== 0 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond 
    (empty? a-seq)
    '(())
    :else
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits-helper [a-seq]
  (cond 
    (empty? a-seq)
    '(())
    :else
    (cons (seq a-seq) (inits-helper (butlast a-seq)))))

(defn inits [a-seq]
  (let [initial (inits-helper a-seq)]
    (sort (fn [x y] (< (count x) (count y))) initial)))

(defn rotation [a-seq number]
  (let [a-seq (concat (rest a-seq) [(first a-seq)])]
  (cond
    (== number 0)
    '()
    :else
    (cons a-seq (rotation a-seq (dec number))))))

(defn rotations [a-seq]
  (rotation a-seq (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
  freqs
  (let [new-freqs (get freqs (first a-seq))]
    (let [freqs (if new-freqs (update-in freqs [(first a-seq)] inc) (assoc freqs (first a-seq) 1))]
  (my-frequencies-helper freqs (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [whole '()]
  (if (empty? a-map)
    '()
  (let [n (get (apply hash-map (first a-map)) (first (first a-map)))]
    (let [zeroth (first (first a-map))]
      (let [repeated (repeat n zeroth)]
        (let [whole (concat whole repeated)]
    (concat whole (un-frequencies (rest a-map))))))))))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
    '()
    (let [taken (first coll)]
      (conj (seq (my-take (dec n) (rest coll))) taken))))

(defn my-drop [n coll]
  (cond 
    (empty? coll)
    '()
    (== n 0)
    coll
    :else
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [first-half (int (/ (count a-seq) 2))]
    (let [zeroth (my-take first-half a-seq)]
      (let [firth (seq (my-drop first-half a-seq))]
      (vec (cons zeroth (cons firth '())))))))

(defn remove-max [a-seq]
  (let [butmax '()]
    (if (empty? a-seq)
      butmax
    (let [maximum (apply max a-seq)]
      (cond 
        (not (== maximum (first a-seq))) 
        (let [butmax (concat [(first a-seq)] butmax)]
        (concat butmax (remove-max (rest a-seq)))))))))

(defn comb-merge [combined]
  (let [result '()]
    (if (empty? combined)
      result
    (let [result (concat [(apply max combined)] result)]
      (let [result (concat (comb-merge (remove-max combined)) result)]
        result)))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
    (if (<= (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))
          

(defn merge-sort [a-seq]
  (cond
    (< (count a-seq) 2)
    (into '() a-seq)
    :else
    (let [zero (merge-sort (nth (halve a-seq) 0))]
      (let [one (merge-sort (nth (halve a-seq) 1))]
        (seq-merge zero one)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

