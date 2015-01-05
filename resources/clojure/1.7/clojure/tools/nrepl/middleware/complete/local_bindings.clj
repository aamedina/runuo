(ns clojure.tools.nrepl.middleware.complete.local-bindings
  "Completion source for local bindings introduced by defn, let and the like."
  (:require [clojure.tools.nrepl.middleware.complete.ns-mappings
             :refer [var-symbol? dash-matches?]]
            [clojure.tools.nrepl.middleware.complete.utils
             :refer [parts-match?]]
            [clojure.string :refer [split join]]))

(def let-like-forms '#{let if-let when-let if-some when-some doseq for})

(def defn-like-forms '#{defn defn- fn defmacro})

(defn parse-binding
  "Given a binding node returns the list of local bindings introduced by that
  node. Handles vector and map destructuring."
  [binding-node]
  (cond (vector? binding-node)
        (mapcat parse-binding binding-node)

        (map? binding-node)
        (let [normal-binds (->> (keys binding-node)
                                (filter symbol?)
                                (mapcat parse-binding))
              keys-binds (if-let [ks (:keys binding-node)]
                           (map str ks) ())
              as-binds (if-let [as (:as binding-node)]
                        [(str as)] ())]
          (concat normal-binds keys-binds as-binds))

        (not (#{'& '_} binding-node))
        [(str binding-node)]))

(defn extract-local-bindings
  "When given a form that has a binding vector traverses that binding vector and
  returns the list of all local bindings."
  [form]
  (when (list? form)
    (cond (let-like-forms (first form))
          (mapcat parse-binding (take-nth 2 (second form)))

          (defn-like-forms (first form))
          (mapcat parse-binding
                  (loop [[c & r] (rest form), bnodes []]
                    (cond (nil? c) bnodes
                          (list? c) (recur r (conj bnodes (first c)))
                          (vector? c) c
                          :else (recur r bnodes)))))))

(defn bindings-from-context
  "Returns all local bindings that are established inside the given context."
  [ctx]
  (distinct (mapcat (comp extract-local-bindings :form) ctx)))

(defn candidates
  "Returns a list of local bindings inside the context that match prefix."
  [prefix _ context]
  (when (var-symbol? prefix)
    (for [binding (bindings-from-context context)
          :when (dash-matches? prefix binding)]
      binding)))
