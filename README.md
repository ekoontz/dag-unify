[![Clojars Project](http://clojars.org/dag-unify/latest-version.svg)](http://clojars.org/dag-unify)

[![Build Status](https://secure.travis-ci.org/ekoontz/dag-unify.png?branch=master)](http://travis-ci.org/ekoontz/dag-unify)

# dag-unify

A Clojure library for combining directed acyclic graphs (DAGs) via
unification. Unification is similar to merge
(http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/merge),
except that if arguments have the same keys, in the return value, the
arguments' values for those keys will be recursively combined via
unification, rather than only one key's value being used.

For example:

```
(let [foo {:a {:b 42}}
      bar {:a {:c 43}}]
  (merge foo bar))
{:a {:c 43}}
```

Note that the `{:b 42}` is lost from the return value - it was
overwritten by `bar`'s value for `:a`.  With `unify`, however, we
preserve both arguments' values for `:a` and combine them as follows:

```
(let [foo {:a {:b 42}}
      bar {:a {:c 43}}]
   (unify foo bar))
{:a {:c 43, :b 42}}
```

For unification of atomic values (numbers, strings, keywords) are
compared by equality. If they are not equal, then the special keyword
`:fail` is returned:

```
(unify 1 1)
1
```

```
(unify 1 2)
:fail
```

## Refs

In Clojure, a DAG may be represented by Clojure map where the values
of keys may be refs (http://clojure.org/refs).

If one argument or more arguments to `unify` is a map with a key whose
value is a reference, then the return value for that key will also be
a reference, and the reference's value will be combined recursively
via unification. For example, `foo`'s value for `:a` is a reference to
the value `{:b 42}`:

```
(let [foo {:a (ref {:b 42})}
      bar {:a {:c 43}}]
  (unify foo bar))
{:a #<Ref@344dc027: {:c 43, :b 42}>}
```

## `:top`

For the special keyword `:top`, the following is true:

- `(unify X :top) => X` for all `X`.

In mathematical terms, `:top` is the identity element of
unification. References work with `:top` as in the following example:

```
(let [reference (ref :top)
      foo {:a reference
           :b reference}
      bar {:a 42}]
  (unify foo bar))
{:b #<Ref@51670b57: 42>, :a #<Ref@51670b57: 42>}
```

## `:fail`

For the special keyword `:fail`, the following is true:

- `(unify X :fail) => :fail` for all `X`.

`:fail` will also be returned if the result of trying to unify values
where the result of unification would result in having incompatible
values for the same reference. For example:

```
(let [myref (ref :top)
      foo {:a myref
           :b myref}
      bar {:a 42
           :b 43}]
  (unify foo bar))
:fail
```

because the `:a` value and `:b` values of `bar` must be unified becase
of the ref shared between the `:a` and `:b` values in `foo`, but the
values themselves are atomic and not equal.

## Usage

```
(require '[dag-unify.core :refer [unify unifyc]])
```

`unify` is destructive: it will modify its arguments if they contain
references, whereas `unifyc` copies its arguments before performing
unification, so that the input arguments are never modified.

## License

Copyright © 2015 Eugene Koontz

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

## References

- "Feature Structures & Unification" http://www.nltk.org/howto/featstruct.html
A similar library written in Python for the the Natural Language Toolkit (NLTK).

- https://en.wikipedia.org/wiki/Feature_structure
