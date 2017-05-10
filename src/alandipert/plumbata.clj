(ns alandipert.plumbata
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn bf-seq [branch? children root]
  (let [walk (fn walk [queue]
               (when-let [node (peek queue)]
                 (lazy-seq
                  (cons node (walk (into (pop queue)
                                         (when (branch? node)
                                           (children node))))))))]
    (walk (conj clojure.lang.PersistentQueue/EMPTY root))))

(declare propagate)
(declare propagate!)

(defmacro ^:private with-let
  "Binds resource to binding and evaluates body.  Then, returns
  resource.  It's a cross between doto and with-open."
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~binding))

(def ^:dynamic *transaction* nil)

(defn- dobatch*
  [thunk]
  (if *transaction*
    (thunk)
    (binding [*transaction* (ref (priority-map))]
      (thunk)
      (let [tx @*transaction*]
        (binding [*transaction* nil]
          (dosync
           (propagate tx)))))))

(defmacro dobatch
  [& body]
  `(dobatch* (fn [] ~@body)))

;; TODO Transactions/`dobatch`
(defn- swap*
  [cell f & args]
  (dosync
   (ref-set (.-prev cell) @(.-state cell))
   (with-let [newv (apply alter (.-state cell) f args)]
     (propagate! cell))))

(deftype Cell [arg-sources sinks rank thunk prev state]
  clojure.lang.IRef
  (deref [this] @(.-state this))
  clojure.lang.IAtom
  (swap [this f] (swap* this f))
  (swap [this f arg] (swap* this f arg))
  (swap [this f arg1 arg2] (swap* this f arg1 arg2))
  (swap [this f x y args] (apply swap* this f x y args))
  ;; boolean compareAndSet(Object oldv, Object newv);
  (reset [this newval] (swap* this (constantly newval))))

(def rank (ref 0))

(defn cell
  [v]
  (Cell. (ref [])                          ;arg-sources (arguments to the formula function)
         (ref #{})                         ;sinks (dependencies)
         (ref (dosync (commute rank inc))) ;rank
         (ref nil)                         ;thunk (non-nil for formula cells)
         (ref v)                           ;prev (previous state)
         (ref v)))                         ;state

(defn cell?
  [c]
  (instance? Cell c))

(defn formula?
  [c]
  (and (cell? c)
       (boolean @(.-thunk c))))

(defn deref*
  [c]
  (if (cell? c) @c c))

(defn- bump-ranks
  [c]
  (dosync
   (doseq [sink (bf-seq identity #(-> % .-sinks deref) c)]
     (ref-set (.-rank sink) (commute rank inc)))))

(defn- propagate
  [pm]
  (when-let [next-cell (first (peek pm))]
    (let [popq  (pop pm)
          old   @(.-prev next-cell)
          new   (if (formula? next-cell) (@(.-thunk next-cell)) @(.-state next-cell))
          diff? (not= new old)]
      (when diff?
        (ref-set (.-prev next-cell) new))
      (recur (if-not diff?
               popq
               (reduce #(assoc %1 %2 @(.-rank %2)) popq @(.-sinks next-cell)))))))

(defn- make-thunk
  [cell fn-source]
  #(dosync
    (let [next-state (apply (deref* fn-source)
                            (map deref* @(.-arg-sources cell)))]
      (ref-set (.-state cell) next-state))))

(defn add-sync!
  [cell]
  (dosync
   (alter *transaction* assoc cell @(.-rank cell))))

(defn propagate!
  [cell]
  (if *transaction*
    (doto cell add-sync!)
    (propagate (priority-map cell @(.-rank cell)))))

(defn set-formula!
  [this fn-source arg-sources]
  {:pre [(cell? this)
         (fn? (deref* fn-source))]}
  (dosync
   (ref-set (.-arg-sources this) arg-sources)
   (doseq [source (conj @(.-arg-sources this) fn-source)
           :when (cell? source)]
     (alter (.-sinks source) conj this)
     (if (> @(.-rank source) @(.-rank this))
       (bump-ranks source)))
   (ref-set (.-thunk this) (make-thunk this fn-source))
   (propagate! this)))

(defn formula
  [f]
  (fn [& arg-sources]
    (doto (cell ::none)
      (set-formula! f arg-sources))))

(comment
  (do
    (def a (cell 1))
    (def b (cell 2))
    (def sum ((formula +) a b)))

  ;; Notes
  ;; Can't implement our own alter/commute, but we can implement IAtom and use
  ;; clojure.core swap!/reset! a al Javelin.

  )
