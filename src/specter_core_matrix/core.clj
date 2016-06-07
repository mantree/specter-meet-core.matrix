(ns specter-core-matrix.core
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.core.matrix :as cm]
            [com.rpl.specter :as s]
            [clojure.core.matrix.protocols :as mp]
            [com.rpl.specter.macros :as sm]
            [clojure.test :refer :all]))

(deftype DataSetColumn
    [col-name col-values]
    clojure.lang.ILookup                ; Allows keyword lookups
    (valAt [this key] 
      (if (= key col-name)
        this
        nil))
    (valAt [this key not-found]
      (if (= key col-name)
        col-values
        not-found))
    clojure.core.matrix.protocols.PColumnNames ; Allows dataset navigator to pull the column names of the row
    (column-name [m i]
      col-name)
    (column-names [m]
      [col-name])
    clojure.lang.Seqable                ; Require to play with specter
    (seq [m]
      (seq col-values))
    clojure.lang.Indexed           ; Allows s/FIRST and others to work
    (count [m] (count col-values))
    (nth [m i]
      (nth col-values i))
    (nth [m i not-found]
      (nth col-values i not-found))
  clojure.lang.IPersistentVector   
    (length [m] (count col-values))
    (assocN [m i v]
                                        ;(assoc (mp/convert-to-nested-vectors m) i v)
                                        ;not sure what to do here
      (prn "ASSOCING")
      )  
    clojure.lang.IPersistentCollection   
    (empty [_]
      (DataSetColumn. col-name []))
    (cons [_ o]
      (DataSetColumn. col-name (conj col-values o))) ;conj when adding to preserve ordering
    (equiv [this that]
      (and 
       (= DataSetColumn
          (type that))
       (= (.col-name this)
          (.col-name that))
       (= (.col-values this)
          (.col-values that)))))

(sm/defnav columns
  []
  (select*
   [this structure next-fn]
   (let [col-names (ds/column-names structure)
         results (keep identity  ;don't like having this nil pruning here...
                       (next-fn (map ->DataSetColumn
                                     (ds/column-names structure) 
                                     (map (partial ds/column structure) 
                                          (ds/column-names structure)))))]
     (ds/dataset (zipmap (map #(.col-name %) results) 
                         results))))
  (transform*
   [this structure next-fn]
   (let [col-names (ds/column-names structure)
         results (next-fn (mapv ->DataSetColumn 
                                (ds/column-names structure) 
                                (map (partial ds/column structure) 
                                     (ds/column-names structure))))]
     (prn (map type results))
     (ds/dataset (zipmap (map #(.col-name %) results) results)))))

(sm/defnav col-val
  [col-name]
  (select*
   [this structure next-fn]
   (next-fn (get structure col-name)))
  (transform*
   [this structure next-fn]))

(deftype DataSetRow
    [col-name->index col-values]
    clojure.lang.ILookup                ; Allows keyword lookups
    (valAt [this key] 
      (nth col-values (get col-name->index key)))
    (valAt [this key not-found]
      (nth col-values (get col-name->index key) not-found))
    clojure.core.matrix.protocols.PColumnNames ; Allows dataset navigator to pull the column names of the row
    (column-name [m i]
      (nth (vals col-name->index) i))
    (column-names [m]
      (apply vector (keys col-name->index)))
    clojure.lang.Seqable                ; Require to play with specter
    (seq [m]
      (seq col-values))
    clojure.lang.Indexed           ; Allows s/FIRST and others to work
    (count [m] (count col-name->index))
    (nth [m i]
      (nth col-values i))
    (nth [m i not-found]
      (nth col-values i not-found))
    java.util.Iterator
    (iterator [_]
      (.iterator col-values))
    clojure.lang.IPersistentVector      ; gives a nice toString
    (length [m] (count col-name->index))
    (assocN [m i v]
                                        ;(assoc (mp/convert-to-nested-vectors m) i v)
                                        ;not sure what to do here
      )
    clojure.lang.IPersistentMap         ; give us transformations
    (assoc [_ k v]
      (if-let [col-dex (get col-name->index k)]
        (DataSetRow. col-name->index 
                     (assoc col-values col-dex v))
        (DataSetRow. (assoc col-name->index k (inc (count col-name->index))) 
                     (conj col-values v))))
    (assocEx [_ k v]
                                        ; no-op. Can't add column to row
      (prn "assocex")
      )
    (without [_ k]
      (prn "without")
                                        ;no op. Can't remove column from row
      ))

(prefer-method print-method clojure.lang.IPersistentVector clojure.lang.IPersistentMap)

(defn dataset->col-name-index
  [dataset]
  (zipmap (ds/column-names dataset)
          (map (partial ds/column-index dataset) 
               (ds/column-names dataset))))

(sm/defnav rows
  []
  (select*
   [this structure next-fn]
   (let [col-name->index (dataset->col-name-index structure)
         results (next-fn 
                  (apply (partial mapv 
                                  (comp (partial ->DataSetRow col-name->index)
                                        vector))
                         (cm/columns structure)))]
     (ds/dataset (keys (.col-name->index (first results)))
                 results)))
  (transform*
   [this structure next-fn]
   (let [col-name->index (dataset->col-name-index structure)
         results (next-fn 
                  (apply (partial mapv 
                                  (comp (partial ->DataSetRow col-name->index)
                                        vector))
                         (cm/columns structure)))]
     (ds/dataset (keys (.col-name->index (first results)))
                 results))))


(sm/defnav as-map
  []
  (select*
   [this structure next-fn]
   (let [as-map (next-fn (zipmap (ds/column-names structure)
                                 (:columns structure)))]
     (ds/dataset as-map)))
  (transform*
   [this structure next-fn]
   (let [as-map (next-fn (zipmap (ds/column-names structure)
                                 (:columns structure)))]
     (ds/dataset as-map))))
