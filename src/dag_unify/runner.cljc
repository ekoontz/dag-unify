(ns dag_unify.runner
  (:require #?(:cljs [doo.runner :refer-macros [doo-tests]])
            [dag_unify.core]))

#?(:cljs (doo-tests 'dag_unify.core-test))


