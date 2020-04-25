# TLA EDN Converter

A small library to convert TLA+ (TLC) values to/from edn data, it's incomplete,
I'm using it for my own projects, but I'm accepting PRs.

## Usage

``` clojure
(require '[tla-edn.core :as tla-edn])
;; converting from edn to TLA+ values
(str
 (tla-edn/to-tla-value [{:a 3}
                        2
                        #{"look" '(1 2)}
                        [[#{{"b" {:c #{6 4 5}}}}]]]))

;; => "<<[a |-> 3], 2, {<<1, 2>>, \"look\"}, <<<<{[b |-> [c |-> {4, 5, 6}]]}>>>>>>"

;; converting from TLA+ values to edn (metadata is created so you can refer to the original TLA type)
(import '(tlc2.value.impl Value IntValue RecordValue BoolValue FcnRcdValue
                    StringValue TupleValue SetEnumValue BoolValue)
        '(util UniqueString))
(let [tla-value (RecordValue.
                 (into-array UniqueString [(UniqueString/uniqueStringOf "eita")])
                 (into-array Value
                             [(TupleValue. (into-array Value [(IntValue/gen 4)
                                                              (IntValue/gen 100)]))])
                 false)]
  (binding [*print-meta* true]
    (prn (tla-edn/to-edn tla-value))))

;; => ^{:tla-plus-type tlc2.value.impl.RecordValue}
;;    {"eita" ^{:tla-plus-type tlc2.value.impl.TupleValue}
;;            (4 100)}

```

You can extend the protocol `tla_edn.core.TLAPlusEdn` for conversion to edn
data.

``` clojure
;; example of extension
(extend-protocol TLAPlusEdn
  tlc2.value.impl.BoolValue
  (to-edn [v]
    (.getVal v)))
```

You can create a `defmethod` for `tla-edn.core/to-tla-value` for conversion
from edn data.

``` clojure
;; example of dispatch using defmethod for tla-edn
(defmethod to-tla-value Boolean
  [v]
  (BoolValue. v))
```

## Thanks to

The people working at https://github.com/tlaplus/tlaplus, it's so great, your
software help and will help a lot of people =)
