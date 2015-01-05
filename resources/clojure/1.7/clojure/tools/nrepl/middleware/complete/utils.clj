(ns clojure.tools.nrepl.middleware.complete.utils
  "Functions and utilities for source implementations."
  (:require clojure.main
            [clojure.string :as string])
  (:import System.IO.FileInfo))

(defn split
  "Like clojure.string/split, but adds an empty string at the end if
  `s` ends with `re` and `append-empty?` is true."
  ([^String s, re]
     (split s re false))
  ([^String s, re append-empty?]
     (let [parts (string/split s re)]
       (if (and append-empty?
                (re-matches (re-pattern (str ".+" re "$")) s))
         (conj parts "")
         parts))))

(defn parts-match?
  "Tests if each part of the complete symbol starts with each
  respective part of the prefix. Both arguments should be lists of
  string."
  [prefix-parts complete-parts]
  (and (<= (count prefix-parts) (count complete-parts))
       (loop [p prefix-parts, n complete-parts]
         (if (first p)
           (when (.StartsWith ^String (first n) ^String (first p))
             (recur (rest p) (rest n)))
           true))))

(defn resolve-symbol
  "Tries to resolve a symbol in the current namespace, or returns nil
  if the symbol can't be resolved."
  [sym]
  (try (resolve sym)
       (catch Exception e)))

(defn resolve-class
  "Tries to resolve a classname from the given symbol, or returns nil
  if classname can't be resolved."
  [sym]
  (when-let [val (resolve-symbol sym)]
    (when (class? val) val)))

(defn resolve-namespace
  "Tries to resolve a namespace from the given symbol, either from a
  fully qualified name or an alias in the given namespace."
  [sym ns]
  (or (find-ns sym) ((ns-aliases ns) sym)))

(defn deep-merge
  "Merge maps recursively. When vals are not maps, last value wins."
  [& xs]
  (let [f (fn f [& xs]
            (if (every? map? xs)
              (apply merge-with f xs)
              (last xs)))]
    (apply f (filter identity xs))))

(defn as-sym
  [x]
  (if x (symbol x)))

(defmulti transform-value "Transform a value for output" type)

(defmethod transform-value :default [v]
  (if (number? v)
    v
    (str v)))

(defmethod transform-value nil [v] nil)

(defmethod transform-value System.IO.FileInfo
  [v]
  (Path/GetFullPath (str v)))

(defmethod transform-value clojure.lang.Sequential
  [v]
  (list* (map transform-value v)))

(defmethod transform-value clojure.lang.Symbol
  [v]
  (let [[the-ns the-name] [(namespace v) (name v)]]
    (or (and the-ns (str the-ns "/" the-name))
        the-name)))

(defmethod transform-value clojure.lang.Keyword
  [v]
  (transform-value (name v)))

(defmethod transform-value clojure.lang.Associative
  [m]
  (->> (for [[k v] m]
         [(str (transform-value k)) (transform-value v)])
       (into {})))

;; handles vectors
(prefer-method transform-value clojure.lang.Sequential clojure.lang.Associative)
