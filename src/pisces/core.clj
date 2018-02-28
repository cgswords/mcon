(ns pisces.core
  (:require [clojure.core.async :as a :refer [go put!]])
  (:require [clojure.test.check.generators :as gen :refer [sample]])
  (:require [clojure.main :as m]))

(def debug false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check Definition                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Blame [server client contract])

(def blm (Blame. "server" "client" "contract"))

(defn string-blame
   [blame]
   (str "[" (:server blame) "," (:client blame) "," (:contract blame) "]"))

(defn invert-blame
  [blame]
  (Blame. (:client blame) (:server blame) (:contract blame)))

(defn indy-blame
  [blame]
  (Blame. (:client blame) (:contract blame) (:contract blame)))

(defrecord Strategy [sname impl])

(defrecord Metastrat [sname impl substrat])

(defn mon-flat
  [strat contract dval blame]
  (cond
    (instance? Strategy strat) 
      ((:impl strat) contract dval blame)
    :else (Exception. (str "Invalid strategy: " strat "\n"
                           "  contract: " contract "\n"
                           "  input:" dval "\n"
                           "Blame: " (string-blame blame)))))

(defn mon-meta
  [strat contract dval blame]
  (if debug (println strat))
  (if debug (println blame))
  (cond
    (instance? Metastrat strat) 
      ((:impl strat) contract dval (:substrat strat) blame)
    (instance? Strategy strat)  
      (mon-flat strat contract dval blame)
    :else (Exception. (str "Invalid strategy: " strat "\n"
                           "  contract: " contract "\n"
                           "  input:" dval "\n"
                           "Blame: " (string-blame blame)))))
 
(defmacro mon
  "Check a contract with a specific strategy"
  [strat contract value blame]
  `(mon-meta ~strat ~contract (delay ~value) ~blame))

(defn extract
  [exp] (if (or (delay? exp) (future? exp)) @exp exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategy Definitions                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Skip Verification
(defn skip-check
  [contract dval blame]
  @dval)

(def skip  (Strategy. "skip" skip-check))

; Eager Verification
(defn eager-check
  [contract dval blame]
  (contract @dval blame))

(def eager (Strategy. "eager" eager-check))

; Semi Verification
(defn semi-check
  [contract dval blame]
  (delay (contract @dval blame)))

(def semi  (Strategy. "semi-eager" semi-check))

; Prom Verification
(defn future-check
  [contract dval blame]
  (future (contract @dval blame)))

(def futur  (Strategy. "future" future-check))

; Conc Verification
(defn conc-check
  [contract dval blame]
  (do (a/go (contract @dval blame)) @dval))

(def conc (Strategy. "conc" conc-check))

; Spot-Checking Verification (functions only)
(defn spot-check
  [generator]
  (fn [contract dval blame]
    (let [f @dval]
      (if (not (fn? f)) 
          (throw (Exception. (str f " is not a function"))))
      (let [c (contract f blame)]
        (doall (map (fn [x] (c x)) (gen/sample generator 20)))
        f))))

(defn spot   
  "spot-checker strategy"
  [g]
  (Strategy. "spot" (spot-check g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metastrategy Definitions                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; With-Operator Verification
(defn with-check
  [fun]
  (fn [contract dval sub-strat blame]
    (let [val @dval
          res (mon sub-strat contract val blame)]
      (do (fun (mon sub-strat contract val (indy-blame blame)))
          res))))

(defn with
   "with meta-strategy, expects a function and a strategy"
   [fun strat]
   (Metastrat. "with" (with-check fun) strat))

; Random Verification
(defn random-check
  [rate]
  (fn [contract dval sub-strat blame]
      (if debug (println (str "rand with " sub-strat)))
      (if (< (rand) rate) 
          (mon sub-strat contract @dval blame) 
          @dval)))

(defn random 
  "randomizer meta-strategy, excepts a monitor rate and a strategy"
  [rate strat] 
  (Metastrat. "rand" (random-check rate) strat))

; Communicating Verification
(defn comm-check
  [channel fun]
  (fn [contract dval sub-strat blame]
    (if debug (println sub-strat))
    (let [val @dval
          res (mon sub-strat contract val blame)]
    (do (a/put! channel (fun (mon sub-strat contract val (indy-blame blame))))
        res))))

(defn comm
   "communication meta-strategy, expects a channel and a strategy"
   [chan fun strat]
   (Metastrat. "comm" (comm-check chan fun) strat))

; Memoizing Verification
(defn memo-check
  [contract dval sub-strat blame]
    (if debug (println sub-strat))
    (mon sub-strat (memoize contract) @dval blame))

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
  (fn [contract dval sub-strat blame]
    (let [res (mon sub-strat contract @dval blame)]
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
  (fn [contract dval sub-strat blame]
    (let [res (mon sub-strat contract @dval blame)
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
      (fn [x blame] 
        (if (f x) 
            x
            (throw (Exception. 
                     (str "Contract violation: " x 
                          " violated " f "\n"
                          "Blame: " (string-blame blame)))))))

(def anyc (predc (fn [x] true)))

(def natc (predc (fn [x] (>= x 0))))

(defn pairc
  "Build a predicate contract"
    [s c1 c2]
    (fn [pair blame]
        [(mon s c1 (first pair) blame)
         (mon s c2 (second pair) blame)]))

(defn con-ravel
  [args ins]
  (if (empty? ins) 
      (list)
      (cons (concat (take 2 args) (list (first ins)))
            (con-ravel (drop 2 args) (rest ins)))))

(defn func
  "Build a function contract"
  [& scs]
  (fn [f blame]
      (fn [& ins]
        (let [l  (* 2 (count ins))
              cl (count scs)]
          (if (not (= (+ 2 l) cl))
            (throw (Exception. "Invalid number of arguments for contracts")))
          (let [mon-sets (con-ravel scs ins)
                posts    (drop l scs)]
            (mon (first posts) 
                 (second posts) 
                 (apply f (map (fn [x] (mon (first x) (second x) (second (rest x)) (invert-blame blame)))
                               mon-sets))
                 blame))))))
