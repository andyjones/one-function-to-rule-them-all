(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(comment
  ;; nice use of lazy sequences
  (defn my-interpose [x a-seq]
    (drop 1 (interleave (repeat x) a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [cnt _] (inc cnt))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(concat [%2] %1) [] a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [[cur-min cur-max] el]
                 [(min cur-min el) (max cur-max el)])
        fst (first a-seq)]
    (reduce min-max [fst fst] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [head '()
         tail sorted-seq]
    (cond
      (empty? tail) (concat head [n])
      (<= n (first tail)) (concat head [n] tail)
      :else (recur (concat head [(first tail)]) (rest tail)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (minus 0 x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & ys] (reduce * x ys)))

(defn pred-and
  ([]             (fn [x] true))
  ([p]            (fn [x] p))
  ([p1 p2]        (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

;; Exercise 13: write the function `my-map` that takes one or more
;; sequences and a function `f` that takes as many parameters
;; as there are sequences
(defn- accumulate [f seqs]
  "call f(x) on each element in a sequence and return the result.
   equivalent to map over a single sequence"
  (reduce (fn [acc s] (conj acc (f s))) [] seqs))

(defn- my-map-helper [acc f seqs]
  (if (some empty? seqs)
    acc
    (let [firsts (accumulate first seqs)
          rests  (accumulate rest seqs)]
      (recur (conj acc (apply f firsts)) f rests))))

(defn my-map [f & seqs]
  (my-map-helper [] f seqs))
