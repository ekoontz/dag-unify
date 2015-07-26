[![Clojars Project](http://clojars.org/dag-unify/latest-version.svg)](http://clojars.org/dag-unify)

[![Build Status](https://secure.travis-ci.org/ekoontz/dag-unify.png?branch=master)](http://travis-ci.org/ekoontz/dag-unify)

# dag-unify

A Clojure library designed to combine directed acyclic graphs (DAGs)
via unification. In Clojure, a DAG may be represented by Clojure map
where the values of keys may be refs
(http://clojure.org/refs). Unification is similar to merge
(http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/merge),
except that if the arguments to unification have the same key, their
values will be recursively unified rather than only one key's value being used.

For example:

```
(let [foo {:a {:b 42}}
      bar {:a {:c 43}}]
  (merge foo bar))
{:a {:c 43}}
```

but:

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

If one argument to `unify` is a map with a key whose value is a
reference, then the resulting unified map's value will also be a reference:

```
(let [foo {:a (ref {:b 42})}
      bar {:a {:c 43}}]
  (unify foo bar))
{:a #<Ref@344dc027: {:c 43, :b 42}>}
```

There are two special keywords, `:top` and `:fail`, for which the following are true:

- `(unify X :top) => X` for all `X`.
- `(unify X :fail) => :fail` for all `X`.

References work with `:top` as in the following example:

```
(let [reference (ref :top)
      foo {:a reference
           :b reference}
      bar {:a 42}]
  (unify foo bar))
{:b #<Ref@51670b57: 42>, :a #<Ref@51670b57: 42>}
```

`:fail` will be returned if the result of trying to unify values where
the result of unification would result in having incompatible values
for the same reference. For example:

```
(let [reference (ref :top)
      foo {:a reference
           :b reference}
      bar {:a 42
           :b 43}]
  (unify foo bar))
:fail
```

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
