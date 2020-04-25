(ns tla-edn.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [tla-edn.core :as tla-edn]))

(deftest core-test
  (is (= "[a |-> 3]"
         (str (tla-edn/to-tla-value {:a 3}))))
  (is (= "<<[a |-> 3], 2, {<<1, 2>>, \"look\"}, <<<<{[b |-> [c |-> {4, 5, 6}]]}>>>>>>"
         (str
          (tla-edn/to-tla-value [{:a 3}
                                 2
                                 #{"look" '(1 2)}
                                 [[#{{"b" {:c #{6 4 5}}}}]]])))))

(deftest loopback-test
  (let [data [{"a" 3}
              2
              #{"look" '(1 2)}
              [[#{{"b" {"c" #{6 4 5}}}}]]]]
    (is (= data
           (tla-edn/to-edn (tla-edn/to-tla-value data))))))
