[![Clojars Project](http://clojars.org/dag_unify/latest-version.svg)](http://clojars.org/dag_unify)

[![Build Status](https://secure.travis-ci.org/ekoontz/dag-unify.png?branch=master)](http://travis-ci.org/ekoontz/dag-unify)

# dag-unify

A Clojure library for combining directed acyclic graphs
([DAGs](https://en.wikipedia.org/wiki/Directed_acyclic_graph)) via
unification. Unification is similar to merge
(http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/merge),
except that if arguments have the same keys, the arguments' values for
those keys will be recursively combined via unification to yield the
value for the key in the combined map.

## Usage

```
% git clone git@github.com:ekoontz/dag-unify.git
% cd dag-unify
% lein repl
nREPL server started on port 56364 on host 127.0.0.1 - nrepl://127.0.0.1:56364
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.8.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (require '[dag_unify.core :as dag])
nil
```

First, let's create a directed acyclic graph by using a Clojure atom:

```
user=> (def foo (let [shared-value (atom :top)]
  #_=>                  {:a {:b shared-value}
  #_=>                   :c shared-value}))
#'user/foo
user=> (dag/pprint foo)
{:a {:b #<Atom@2bac2366: :top>}, :c #<Atom@2bac2366: :top>}
nil
```

Note in the above that in the directed acyclic graph `foo`, the value for the
path `[:a :b]` is set to the same value as the path `[:c]`.

Now, let's unify this with a specific value at the path: `[:c]`, namely `42`.

```
user=> (dag/pprint (dag/unify foo {:c 42}))
{:c #<Atom@7d8745bd: 42>, :a {:b #<Atom@7d8745bd: 42>}}
nil
user=>
```   

Using dag-unify's built-in `get-in`, we get the same value for the
paths `[:a :b]` and `[:c]`:

```
user=> (def foo2 (dag/unify foo {:c 42}))
#'user/foo2
user=> (dag/get-in foo2 [:a :b])
42
user=> (dag/get-in foo2 [:c])
42
```

## Unification

Consider the behavior of Clojure's `merge`:

```
user> (merge {:a {:b 42}}
             {:a {:c 43}})

{:a {:c 43}}

```

Note that the the first argument's `{:b 42}` is lost from the return
value - it was overwritten by the second argument's value for `:a` -
`{:c 43`}.

With `unify`, however, we preserve both arguments' values for `:a` and
combine them as follows:

```
user> (unify {:a {:b 42}}
             {:a {:c 43}})

{:a {:b 42, :c 43}}
```

We can use [Atoms](http://clojure.org/atoms) with a map to represent a DAG:

```
user> (def foo (let [shared-value (atom :top)]
                 {:a {:b shared-value}
                  :c shared-value}))
                  
#'user/foo
user> foo
{:a {:b #atom[:top 0x57212d5f]}, :c #atom[:top 0x57212d5f]}
```

And then `unify` that DAG with another map to cause the atom's value
to be modified:

```
user> (unify foo {:c 42})
{:c #atom[42 0x57212d5f], :a {:b #atom[42 0x57212d5f]}}
user> foo
{:a {:b #atom[42 0x57212d5f]}, :c #atom[42 0x57212d5f]}
user> 
```

If one or more arguments to `unify` is a map with a key whose value is
an atom, then the value of that key will still be that same atom, but its
value will be modified to be the unification of the arguments. For example:

```
(let [shared-value (atom {:b 42})
      foo {:a shared-value}
      bar {:a {:c 43}}]
  (unify foo bar))
=> {:a #<Atom@344dc027: {:c 43, :b 42}>}
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

In other words, `:top` is the [identity
element](https://en.wikipedia.org/wiki/Identity_element) of
unification. It is the most unspecific, most general value possible.

Atoms work with `:top` as in the following example:

```
(let [shared-value (atom :top)
      foo {:a shared-value
           :b shared-value}
      bar {:a 42}]
  (unify foo bar))
=> {:b #<Atom@51670b57: 42>, :a #<Atom@51670b57: 42>}
```

In the immediately above case, `foo` has an unspecified `:top` value
that is shared by `foo`'s `:a` and `:b` key. `bar` had a more specific
value (i.e. 42) for `:a`, but no `:b` key. The result of unification
of `foo` and `bar` is a map with `:a` and `:b` both sharing the same
value `42`.

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
(let [shared-value (atom :top)
      foo {:a shared-value
           :b shared-value}
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

## Unification of simple values

For unification of simple values (numbers, strings, keywords), they
are compared by equality. If the values to be unified are not equal,
then `:fail` is returned:

```
(unify 1 1)
=> 1
```

```
(unify 1 2)
=> :fail
```

## `unify!` versus `unify`

`unify!` is destructive: it will modify its arguments if they contain
references, whereas `unify` copies its arguments before performing
unification, so that the input arguments are never modified.

## License

Copyright © 2015 Eugene Koontz

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

## References

- "Feature Structures & Unification" http://www.nltk.org/howto/featstruct.html
A similar library written in Python for the the Natural Language Toolkit (NLTK).

- https://en.wikipedia.org/wiki/Feature_structure

## See also:

- [release-notes.txt](release-notes.txt)
