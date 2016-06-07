(ns specter-core-matrix.core-test
  (:require [clojure.test :refer :all]
            [specter-core-matrix.core :refer :all]
            [com.rpl.specter.macros :as sm]
            [com.rpl.specter :as s]
            [clojure.core.matrix.dataset :as ds]))


(defn d
  "Dataset builder"
  [ks values]
  (ds/dataset
   (map zipmap
        (repeat ks)
        values)))

(def raw-ds1 [[1 2] [2 4] [4 5]])

(def ds1 (d [:a :b] raw-ds1))


(deftest reflective-tests
  "Can we get the same dataset back by selecting all the data"
  (is
   (= ds1
      (sm/select [rows s/ALL] ds1)))
  (is
   (= ds1
      (sm/select [columns s/ALL] ds1))))

(deftest select-column
  "Select a column(s) by column name"
  (is
   (=
    (ds/dataset {:a (ds/column ds1 :a)})
    (sm/select [columns s/ALL :a] ds1)))
  (is
   (=
    (ds/dataset {:b (ds/column ds1 :b)})
    (sm/select [columns s/ALL :b] ds1)))
  (is
   (=
    ds1
    (sm/select [columns s/ALL (s/multi-path :a :b)] ds1))))

(deftest select-rows
  (is
   (=
    (d [:a :b] (filter (comp even? first) raw-ds1))
    (sm/select [rows (s/filterer :a even?) s/ALL] ds1))
    "Select rows where :a is even"))

(deftest increment-column
  (let [a-incd (d [:a :b] (map 
                           (fn [[a b]] 
                             [((if (even? a) inc identity) a) b]) 
                           raw-ds1))]
    (is
     (=
      a-incd
      (sm/transform [rows s/ALL :a even?]
                    inc
                    ds1))
     "Increment :a on a row by row basis")
    (is
     (=
      a-incd
      (sm/transform [columns (s/filterer :a s/ALL) s/ALL (fn [c] (= (.col-name c) :a)) s/ALL even?]
                    inc
                    ds1))
     "Increment :a just by looking at the column itself")))

(deftest create-new-column
  (let [fixed-c (d [:a :b :c] (mapv #(conj % 9) raw-ds1))]
    (is
     (= fixed-c
        (sm/transform [rows s/ALL]
                      #(assoc % :c 9)
                      ds1)))
    (is
     (= fixed-c
        (sm/transform [columns s/END]
                      (fn [& a]
                        (prn a)
                        (->DataSetColumn [:c] [(vec (repeat 3 9))]))
                      ds1)))))

(comment
  (deftest deride-column
    (let [derided-c (d [:a :b :c] (mapv #(conj % (apply * %)) raw-ds1))]
      (is
       (= derided-c
          (sm/transform [rows s/ALL]
                        #(assoc % :c (* (:a %) (:b %)))
                        ds1)))
      (is
       (= derided-c
          (sm/transform [columns s/ALL ]))))))

; Transform rows where :a is even?, by inc'ing :b

#_(sm/transform [s/ALL]
              inc
              [1 2 3])



(sm/transform [s/ALL (s/selected? :a s/ALL) :a s/ALL]
              inc
              [{:a [1 2 3]} {:b [2 3 4]} {:a [1 2 3]}])


(sm/transform [(s/filterer :a s/ALL) s/ALL :a s/ALL]
              inc
              [{:a [1 2 3]} {:b [2 3 4]} {:a [1 2 3]}])

(sm/transform [s/ALL :a s/ALL]
              inc
              [{:a [1 2 3]} {:b [2 3 4]} {:a [1 2 3]}])
