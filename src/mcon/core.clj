(ns mcon.core
  (:require [clojure.core.async :as a :refer [go put!]])
  (:require [clojure.test.check.generators :as gen :refer [sample]])
  (:require [clojure.main :as m]))

(def debug false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check Definition                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Strategy [sname impl])

(defrecord Metastrat [sname impl substrat])

(defn mon-flat
  [strat contract dval]
  (cond
    (instance? Strategy strat) 
      ((:impl strat) contract dval)
    :else (Exception. (str "Invalid strategy: " strat "\n"
                           "  contract: " contract "\n"
                           "  input:" dval "\n"))))

(defn mon-meta
  [strat contract dval blame]
  (if debug (println strat))
  (if debug (println blame))
  (cond
    (instance? Metastrat strat) 
      ((:impl strat) contract dval (:substrat strat))
    (instance? Strategy strat)  
      (mon-flat strat contract dval)
    :else (Exception. (str "Invalid strategy: " strat "\n"
                           "  contract: " contract "\n"
                           "  input:" dval "\n"))))
 
;; (defmacro make-blame
;;   [contract]
;;   `(cons (-> ~contract var meta :name str) :server :client)) 
 
(defmacro mon
  "Check a contract with a specific strategy"
  [strat contract value]
  `(mon-meta ~strat ~contract (delay ~value) (list :server :client :contract)))

(defn extract
  [exp] (if (or (delay? exp) (future? exp)) @exp exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategy Definitions                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Skip Verification
(defn skip-check
  [contract dval]
  @dval)

(def skip  (Strategy. "skip" skip-check))

; Eager Verification
(defn eager-check
  [contract dval]
  (contract @dval))

(def eager (Strategy. "eager" eager-check))

; Semi Verification
(defn semi-check
  [contract dval]
  (delay (contract @dval)))

(def semi  (Strategy. "semi-eager" semi-check))

; Prom Verification
(defn future-check
  [contract dval]
  (future (contract @dval)))

(def futur  (Strategy. "future" future-check))

; Conc Verification
(defn conc-check
  [contract dval]
  (do (a/go (contract @dval)) @dval))

(def conc (Strategy. "conc" conc-check))

; Spot-Checking Verification (functions only)
(defn spot-check
  [generator]
  (fn [contract dval]
    (let [f @dval]
      (if (not (fn? f)) 
          (throw (Exception. (str f " is not a function"))))
      (let [c (contract f)]
        (doall (map (fn [x] (c x)) (gen/sample generator 20)))
        f))))

(defn spot   
  "spot-checker strategy"
  [g]
  (Strategy. "spot" (spot-check g)))

;; (defn fconc-check
;;   [contract dval strat]
;;   ;; FIX ME 
;;   (do (a/go (contract @dval)) @dval))
;; 
;; (def fconc (Strategy. "fconc" fconc-check nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metastrategy Definitions                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; With-Operator Verification
(defn with-check
  [fun]
  (fn [contract dval sub-strat]
    (let [res (mon sub-strat contract @dval)]
      (do (fun res)
          res))))

(defn with
   "with meta-strategy, expects a function and a strategy"
   [fun strat]
   (Metastrat. "with" (with-check fun) strat))

; Random Verification
(defn random-check
  [rate]
  (fn [contract dval sub-strat]
      (if debug (println (str "rand with " sub-strat)))
      (if (< (rand) rate) 
          (mon sub-strat contract @dval) 
          @dval)))

(defn random 
  "randomizer meta-strategy, excepts a monitor rate and a strategy"
  [rate strat] 
  (Metastrat. "rand" (random-check rate) strat))

; Communicating Verification
(defn comm-check
  [channel fun]
  (fn [contract dval sub-strat]
    (if debug (println sub-strat))
    (let [res (mon sub-strat contract @dval)]
    (do (a/put! channel (fun res))
        res))))

(defn comm
   "communication meta-strategy, expects a channel and a strategy"
   [chan fun strat]
   (Metastrat. "comm" (comm-check chan fun) strat))

; Memoizing Verification
(defn memo-check
  [contract dval sub-strat]
    (if debug (println sub-strat))
    (mon sub-strat (memoize contract) @dval))

(defn memo 
  "memoizer meta-strategy, excepts a ref to a map and a strategy"
  [strat] 
  (Metastrat. "memo" memo-check strat))

; State Contracts
(defn make-contract-state
  [start-state]
  (ref (list start-state)))

(defn in? 
  [coll elm]  
  (some #(= elm %) coll))

(defn transition-check
  [state-ref from-state to-state]
  (fn [contract dval sub-strat]
    (let [res (mon sub-strat contract @dval)]
      (if (in? (deref state-ref) from-state)
          (dosync (ref-set state-ref (if (list? to-state) to-state (list to-state)))
                  res)
          (throw (Exception. (str "Program performed invalid state transition:\n"
                                  "  Current state: " (deref state-ref)
                                  "  Transition: " from-state " -> " to-state "\n")))))))

(defn transition
  [state-ref from-state to-state strat]
  (Metastrat. (str "transition" from-state to-state)
              (transition-check state-ref from-state to-state)
              strat))

(defn transition-as-check
  [state-ref transition-fn]
  (fn [contract dval sub-strat]
    (let [res (mon sub-strat contract @dval)
          to-state (transition-fn (deref state-ref) res)]
      (if (not (= to-state :error))
          (dosync (ref-set state-ref (if (list? to-state) to-state (list to-state)))
                  res)
          (throw (Exception. (str "Program performed invalid operation:\n"
                                  "  Current state: " (deref state-ref) "\n")))))))

(defn transition-as
  [state-ref transition-fn strat]
  (Metastrat. (str "transition-as")
              (transition-as-check state-ref transition-fn)
              strat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contract Definitions                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pretty-demunge
  [fn-object]
  (let [dem-fn (m/demunge (str fn-object))
        pretty (second (re-find #"(.*?\/.*?)[\-\-|@].*" dem-fn))]
    (if pretty pretty dem-fn)))

(defn predc
  "Build a predicate contract"
    [f]
      (fn [x] 
        (if (f x) 
            x
            (throw (Exception. 
                     (str "Contract violation: " x 
                          " violated " f))))))

(def anyc (predc (fn [x] true)))

(def natc (predc (fn [x] (>= x 0))))

(defn pairc
  "Build a predicate contract"
    [s c1 c2]
    (fn [pair]
        [(mon s c1 (first pair))
         (mon s c2 (second pair))]))

(defn con-ravel
  [args ins]
  (if (empty? ins) 
      (list)
      (cons (concat (take 2 args) (list (first ins)))
            (con-ravel (drop 2 args) (rest ins)))))

(defn func
  "Build a function contract"
  [& scs]
  (fn [f]
      (fn [& ins]
        (let [l  (* 2 (count ins))
              cl (count scs)]
          (if (not (= (+ 2 l) cl))
            (throw (Exception. "Invalid number of arguments for contracts")))
          (let [mon-sets (con-ravel scs ins)
                posts    (drop l scs)]
            (mon (first posts) 
                 (second posts) 
                 (apply f (map (fn [x] (mon (first x) (second x) (second (rest x))))
                               mon-sets))))))))

