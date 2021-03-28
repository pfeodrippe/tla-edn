(ns tla-edn.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [tla-edn.core :as tla-edn]))

(deftest core-test
  (is (= "[a |-> 3]"
         (str (tla-edn/to-tla-value {:a 3}))))
  (is (= "<<[a |-> 3, b__a |-> 5], 2, {<<1, 2>>, \"look\"}, <<<<{[b |-> [c |-> {4, 5, 6}]]}>>>>>>"
         (str
          (tla-edn/to-tla-value [{:a 3 :b/a 5}
                                 2
                                 #{"look" '(1 2)}
                                 [[#{{"b" {:c #{6 4 5}}}}]]])))))

(deftest loopback-test
  (let [data [{"a" 3}
              2
              #{"look" '(1 2)}
              [[#{{"b" {"c" #{6 4 5}}}}]]]]
    (is (= data
           (tla-edn/to-edn (tla-edn/to-tla-value data))))
    (testing "namespaced string is parsed automatically to a keyword"
      (is (= :b/a (tla-edn/to-edn (tla-edn/to-tla-value :b/a))))
      (is (= {:b/a 5} (tla-edn/to-edn (tla-edn/to-tla-value {:b/a 5}))))
      (is (= {:b/a :c/d} (tla-edn/to-edn (tla-edn/to-tla-value {:b/a :c/d})))))
    (testing "all strings converted to keyword"
      (let [m {:account/alice 5
               :account/bob 5
               :wire/procs {:one {:amount 3
                                  :pc ::check-funds}}}]
        (is (= m (tla-edn/to-edn (tla-edn/to-tla-value m) {:string-to-keyword? true})))))))
