(ns mcon.paper
  (:require [mcon.core :refer :all])
  (:require [clojure.test.check.generators :as gen :refer [nat]])
  (:require [clojure.core.async :as a :refer [go put! take!]]))

(defn ex1 [] (+ 5 (mon eager natc 5 blm)))
(defn ex2 [] (+ 5 (mon eager natc -1 blm)))

(defn ex3 [] (mon futur natc 5 blm))
(defn ex4 [] @(mon futur natc 5 blm))

(defn ex5 [] (mon semi natc -1 blm))
(defn ex6 [] @(mon futur natc -1 blm))

(defn ex7 [] (mon conc natc -1 blm))
(defn ex8 [] (mon conc natc -1 blm))

;;;;;;;;;;;;;;;;;;;;;;

(defn natpairc [strat] (pairc strat natc natc))

(defn ex9  []  (first (mon eager (natpairc eager)  (list 5 -1) blm)))
(defn ex10 [] @(first (mon eager (natpairc semi)   (list 5 -1) blm)))
(defn ex11 []  (first @(mon futur (natpairc eager) (list 5 -1) blm)))
(defn ex12 [] @(first @(mon futur (natpairc semi)  (list 5 -1) blm)))

(defn ex13 []
  (let [x (mon semi natc 5 blm) 
        y (mon eager natc 5 blm)]
       (+ (extract x) (extract y))))

;;;;;;;;;;;;;;;;;;;;;;

(defn fact [x]
  (let [n (extract x)]
    (if (zero? n) 1 (* n (fact (- n 1))))))

(defn natfunc
  [strat]
  (func strat natc eager natc))

(def fact-ee (mon eager (natfunc eager) fact blm))

(defn ex14 [] (fact-ee 5))

(def fact-se (mon eager (natfunc semi) fact blm))

(defn ex15 [] (fact-se 5))

;;;;;;;;;;;;;;;;;;;;;;

(def nat-nat-fec (natfunc eager))

(defn ex16 [] (mon (spot gen/nat) nat-nat-fec (fn [x] (+ x 1)) blm))
(defn ex17 [] (mon (spot gen/nat) nat-nat-fec (fn [x] (- x 10)) blm))

;;;;;;;;;;;;;;;;;;;;;;

(def add-with-print 
  (mon eager
       (func (with println eager) natc
             eager                natc)
       (fn [x] (+ x 10))
       blm))

(defn ex18 [] (add-with-print 10))

;;;;;;;;;;;;;;;;;;;;;;

(def args (ref (list)))

(defn teller
  [x]
    (dosync (ref-set args (cons x (deref args)))))

(def add-with-tell
  (mon eager
       (func (with teller eager) anyc eager anyc)
       (fn [x] (+ x 1))
       blm))

(defn ex19 [] (add-with-tell 10))
(defn ex20 [] (add-with-tell 5))
(defn ex21 [] (add-with-tell 0))
(defn ex22 [] (deref args))

;;;;;;;;;;;;;;;;;;;;;;;

(def times (ref (list)))

(defn cur-time [] (System/currentTimeMillis))

(defn tag-timer
  [tag]
  (dosync (ref-set times
                   (cons (list tag (cur-time))
                   (deref times)))))

(defn slow-fact
  [n]
  (cond
    (zero? n) 1
    :else (dosync (Thread/sleep (* n 10))
                  (* n (fact (- n 1))))))

(def slow-fact-timed
  (mon eager
       (func (with (fn [x] (tag-timer :pre)) eager) natc
             (with (fn [x] (tag-timer :post)) eager) natc)
       slow-fact
       blm))

(defn ex23 [] (slow-fact-timed 10))
(defn ex24 [] (deref times))

(defn ex25 [] (mon eager natc 5 blm))
(defn ex26 [] (+ 5 (mon eager natc -1 blm)))

(defn ex27 [] (mon semi natc 5 blm))
(defn ex28 [] 
  (let [x (mon semi natc -1 blm)]
    (+ (fact 5) @x)))

(defn ex29 [] (mon futur natc 5 blm))
(defn ex30 [] 
  (let [x (mon futur natc -1 blm)]
    (+ (fact 5) @x)))

(defn ex31 []
  (let [x (mon conc natc -1 blm)]
    (+ x x)))

(def fact-m (mon eager (func (memo eager) natc
                             (memo eager) natc)
                 fact blm))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord BinTree [val left right])

(defn treedc
  "Dependent tree contract combinator"
  [sleaf cleaf sval cval srec cleft cright]
  (fn  [tree blame]
    (if (nil? tree)
        (mon sleaf cleaf tree blame)
        (let [v (:val tree)]
          (BinTree.
            (mon sval cval v blame)
            (mon srec (cleft v)  (:left tree) blame)
            (mon srec (cright v) (:right tree) blame))))))

(defn bst-range
  [lo hi]
  (fn [v] (and (>= v lo) (<= v hi))))

(defn bstc
  "Binary search tree contract"
  [rec-strat value-strat]
  (letfn [(bstc [lo hi]
            (treedc
              eager anyc
              value-strat (predc (bst-range lo hi))
              rec-strat
              (fn [v] (bstc lo (extract v)))
              (fn [v] (bstc (extract v) hi))))]
    (bstc Integer/MIN_VALUE Integer/MAX_VALUE)))

(def tree0 (BinTree. 5
                    (BinTree. 4 nil nil)
                    (BinTree. 7 nil nil)))

(defn ex32 [] (mon eager (bstc eager eager) tree0 blm))
(defn ex33 [] (mon futur (bstc eager eager) tree0 blm))

(def tree 
  (BinTree. 5
    (BinTree. 6 nil nil)
    (BinTree. 7 nil nil)))

(defn ex34 [] @(:left @(mon semi (bstc semi eager) tree blm)))
(defn ex35 []  (:val @(:left @(mon semi (bstc semi semi) tree blm))))
(defn ex36 [] @(:val @(:left @(mon semi (bstc semi semi) tree blm))))

(defn bst-insert [] nil)

(def bst-ins
  (mon eager 
       (func eager natc eager anyc
             (random 0.1 eager) (bstc eager eager))
       bst-insert
       blm))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(def iter-state (make-contract-state :unknown))

(def iternext
  (mon eager
       (func eager anyc
             (transition iter-state :some :unknown eager) anyc)
       #(.next %)
       blm))

(defn hasNextTrans
  [con-result cur-state]
    (if (extract con-result) :some :none))

(def hasNext
  (mon eager
       (func eager anyc
             (transition-as iter-state hasNextTrans eager) anyc)
       #(.hasNext %)
       blm))

;; Example 1
(defn ex37 []
  (let [iter (.iterator
               (.keySet (java.lang.System/getProperties)))]
                   (while (hasNext iter)
                         (println (next iter))))
)

;; Example 2
(defn ex38 []
  (let [iter (.iterator
               (.keySet (java.lang.System/getProperties)))]
                   (println (next iter)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn timer-task
  [in-chan out-chan timer-info]
    (let [action (a/<!! in-chan)]
        (cond
          (= action :result)
            (let [res (filter #(= ( first %) :time) timer-info)]
              (a/>!! out-chan
                     (/ (reduce + (map second res))
                        (float (count res)))))
          (and (list? action) (= (first action) :pre))
            (timer-task
              in-chan out-chan
              (cons (list :pre (second action)) timer-info))
          (and (list? action) (= (first action) :post))
            (timer-task
              in-chan out-chan
              (cons (list :time
                          (- (second action)
                             (second (first timer-info))))
                    (rest timer-info)))
          :else
            (timer-task in-chan out-chan timer-info))))

(defn start-timer-task
  [in-chan out-chan]
  (a/go (timer-task in-chan out-chan (list))))

(defn timer-func
  [in-chan out-chan]
  (do 
    (start-timer-task in-chan out-chan)
    (func (comm in-chan
                (fn [_] (list :pre  (cur-time)))
                eager)
          anyc
          (comm in-chan
                (fn [_] (list :post (cur-time)))
                eager)
          anyc)))

(defn ex39 []
	(let [in (a/chan)
        out (a/chan)
        f (mon eager (timer-func in out) slow-fact blm)]
    (f 15)
    (f 10)
    (f 5)
    (a/put! in :result)
    (a/<!! out)))

