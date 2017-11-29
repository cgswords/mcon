(ns mcon.bst
  (:require [mcon.core :refer :all]))

(defrecord BinTree [val left right])

(defn treedc
  "Dependent tree contract combinator"
  [sleaf cleaf sval cval cleft cright srec]
  (fn  [tree blame]
    (if (nil? tree)
        (mon sleaf cleaf tree)
        (let [v (:val tree)]
          (BinTree.
            (mon sval cval v blame)
            (mon srec (cleft v) (:left tree) blame)
            (mon srec (cright v) (:right tree) blame))))))

(defn bst-range
  [lo hi]
  (fn [v] (and (>= v lo) (<= v hi))))

(defn bstc
  "Binary search tree contract"
  [rec-strat]
  (letfn [(bstc [lo hi]
            (treedc
              eager anyc
              eager (predc (bst-range lo hi))
              rec-strat
              (fn [v] (bstc lo (extract v)))
              (fn [v] (bstc (extract v) hi))))]
    (bstc Integer/MIN_VALUE Integer/MAX_VALUE)))

(defn make-leaf [v] (struct bst v nil nil))

(defn bst-insert
  [tree n]
  (let [tree (if (future? tree) @tree tree)
        tree (if (delay? tree) (force tree) tree)]
    (if (nil? tree) 
        (make-leaf n)
        (let [v (:val tree)
              l (:left tree)
              r (:right tree)]
          (cond
            (< n v) (BinTree. v (bst-insert l n) r)
            (> n v) (BinTree. v l (bst-insert r n))
            :else tree))))) ;; don't reinsert if it's already there...

(defn bst-contains?
  [tree n]
  (let [tree (if (future? tree) @tree tree)
        tree (if (delay? tree) (force tree) tree)]
    (if (nil? tree)
        false 
        (let [v (:val tree)
              l (:left tree)
              r (:right tree)]
          (cond
            (= v n) true
            (< n v) (bst-contains? l n)
            (> n v) (bst-contains? r n)
            :else false))))) ;; this case shouldn't occur

(defn bst-insert-wrong
  [tree n]
  (let [tree (if (future? tree) @tree tree)
        tree (if (delay? tree) (force tree) tree)]
    (if (nil? tree) 
        (make-leaf n)
        (let [v (:val tree)
              l (:left tree)
              r (:right tree)]
          (cond
            (> n v) (BinTree. v (bst-insert l n) r)
            (< n v) (BinTree. v l (bst-insert r n))
            :else tree))))) ;; don't reinsert if it's already there...

(defn all-to-tree
  [input]
  (reduce (fn [tree n] (bst-insert tree n)) nil input))

(def ls [500 250 750 200 300 700 800 100 400 600 900])

(defn rand-list
  [n]
  (for [x (range n)] (rand-int 1000000)))

(defn exp [x n]
  (reduce * (repeat n x)))

(def trees
  (for [x (range 25)] (all-to-tree (rand-list (exp 2 x)))))

