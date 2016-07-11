[![Clojars Project](http://clojars.org/dag-unify/latest-version.svg)](http://clojars.org/dag-unify)

[![Build Status](https://secure.travis-ci.org/ekoontz/dag-unify.png?branch=master)](http://travis-ci.org/ekoontz/dag-unify)

# dag-unify

A Clojure library for combining directed acyclic graphs (DAGs) via
unification. Unification is similar to merge
(http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/merge),
except that if arguments have the same keys, the arguments' values for
those keys will be recursively combined via unification to yield the
value for the key in the combined map.

First consider the behavior of Clojure's `merge`:

```
(let [foo {:a {:b 42}}
      bar {:a {:c 43}}]
  (merge foo bar))
=> {:a {:c 43}}
```

Note that the `{:b 42}` is lost from the return value - it was
overwritten by `bar`'s value for `:a`.

With `unify`, however, we preserve both arguments' values for `:a` and
combine them as follows:

```
(let [foo {:a {:b 42}}
      bar {:a {:c 43}}]
   (unify foo bar))
=> {:a {:c 43, :b 42}}
```

For unification of atomic values (numbers, strings, keywords) are
compared by equality. If they are not equal, then the special keyword
`:fail` is returned:

```
(unify 1 1)
=> 1
```

```
(unify 1 2)
=> :fail
```

## Refs

In Clojure, a DAG may be represented by Clojure map where the values
of keys may be refs (http://clojure.org/refs).

If one argument or more arguments to `unify` is a map with a key whose
value is a reference, then the return value for that key will also be
the same reference, with its value being the unification of the
arguments. For example:

```
(let [myref (atom {:b 42})
      foo {:a myref}
      bar {:a {:c 43}}]
  (unify foo bar))
=> {:a #<Ref@344dc027: {:c 43, :b 42}>}
```

Above, `foo`'s value for `:a` is a reference to the value `{:b
42}`. `foo`'s value for `:a` is unified with `bar`'s value for `:a`
(`{:c 43}`), and the result `{:b 42, c 43}` is the new value of the
reference, and this reference is the value `:a` for the unification of
`foo` and `bar`.

## `:top`

For the special keyword `:top`, the following is true for all `X`:

```
(unify X :top) => X
```

In other words, `:top` is the [identity element](https://en.wikipedia.org/wiki/Identity_element) of
unification.

References work with `:top` as in the following example:

```
(let [myref (atom :top)
      foo {:a myref
           :b myref}
      bar {:a 42}]
  (unify foo bar))
=> {:b #<Ref@51670b57: 42>, :a #<Ref@51670b57: 42>}
```

In the immediately above case, `foo` had an unspecified value that was
shared by `foo`'s `:a` and `:b` key. `bar` had a specific value (42) for
`:a`, but no `:b` key. The result is a map with `:a` and `:b` both
sharing the same value `42`.

## `:fail`

For the special keyword `:fail`, the following is true for all `X`:

```
(unify X :fail) => :fail
```

In any map, if any key's value is equal to `:fail`, the entire map is
equal to `:fail`. For example, the following map, despite its
complicated structure:

```
{:a 42
 :b {:c {:d :fail
         :e {:f 43}
	 :g 44}}
 :h {:i "hello"}}
```

is no different from `:fail` as far as unification is concerned.


`:fail` is used to represent a failed attempt of trying to unify
values which are not equal (if atomic values) or have some part that
is not equal. For example:

```
(let [myref (atom :top)
      foo {:a myref
           :b myref}
      bar {:a 42
           :b 43}]
  (unify foo bar))
=> :fail
```

Above, the `:a` value and `:b` values of `bar` should be identical
because `:foo` has a shared value which `:a` and `:b` both point to.
However, these two atomic values (42 and 43) are not equal
to one another. The result is that the unification of `foo` and `bar`
is `:fail`.

## Usage

```
(require '[dag_unify.core :refer [unify unifyc]])
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
