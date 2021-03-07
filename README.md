# TLA EDN

A small library to write TLA+ (TLC) Operators and to convert TLA+ values
to/from EDN data.

## Installation

Add the following dependency to your `project.clj` file

[![Clojars Project](http://clojars.org/pfeodrippe/tla-edn/latest-version.svg)](http://clojars.org/pfeodrippe/tla-edn)

## Status

It's not ready for production (and maybe never will), but you should
use it only for testing anyway.

## Usage

Check an example [here](https://github.com/pfeodrippe/tla-edn-example).

### Overriding operators

Let's say you have one TLA+ operator called `Inc` which you would like to
override using Clojure.

``` ruby
(* module "Incrementer", this name is important as it will be referenced later *)
Inc(n) == n + 1
```

The required steps are the following, you create a overriding operator
with `defoperator`. Please, add the folder `classes` to your project file
classpath (project.clj or deps.edn or boot.clj).

``` clojure
(ns my-ns.core
  (:require
   [tla-edn.core :as tla-edn]
   [tla-edn.spec :as spec]))

(spec/defop Inc {:module "Incrementer"}
  [x]
  (-> (tla-edn/to-edn x)
      inc
      tla-edn/to-tla-value))
```

`defop` makes use of `gen-class`, so at first time you run it, the operators
will be compiled, then any change is automatically loaded when run again.

``` clojure
(defn -main
  []
  ;; this will run the TLA+ spec with its config file
  (spec/run-spec "path/to/Incrementer.tla" "Incrementer.cfg")
  (System/exit 0))
```

Then from the terminal

``` shell
;; assuming you are using `deps.edn`
$ clj -A:test -m my-ns.core # this will compile (just at first time)
# OK
$ clj -A:test -m my-ns.core
# TLA+ output (it will run the spec with the overrides, hopefully)
```

### Converting Data
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

The people working at https://github.com/tlaplus/tlaplus =D
