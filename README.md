[![Clojars Project](http://clojars.org/dag_unify/latest-version.svg)](http://clojars.org/dag_unify)

[![Build Status](https://secure.travis-ci.org/ekoontz/dag-unify.png?branch=master)](http://travis-ci.org/ekoontz/dag-unify)
[![Run on Repl.it](https://repl.it/badge/github/ekoontz/dag-unify)](https://repl.it/github/ekoontz/dag-unify)

# dag-unify

A Clojure library for combining directed acyclic graphs
([DAGs](https://en.wikipedia.org/wiki/Directed_acyclic_graph)) via
unification. Unification is similar to Clojure core's 
[merge](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/merge),
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

First, let's create a Clojure map that is also a directed acyclic
graph by using a Clojure atom within the map:

```
user=> (def foo (let [shared-value (atom :top)]
  #_=>            {:a {:b shared-value}
  #_=>                 :c shared-value}))
#'user/foo
```

If you print `foo` it looks like:

```
user=> foo
{:a {:b #object[clojure.lang.Atom 0x6ee6ded {:status :ready, :val :top}]}, :c #object[clojure.lang.Atom 0x6ee6ded {:status :ready, :val :top}]}
```

In the above output, it can be seen that in `foo`, the value for the path `[:a :b]` is
set to the same value (the atom `shared-value`) as the path `[:c]`.

You can use `dag/pprint` to show this more readably:

```
user=> (dag/pprint foo)
{:a {:b [[1] :top]}, :c [1]}
```

### The special keyword `:top`

We used the keyword `:top` in the above example because it is a special
keyword for the purposes of unification.

For this keyword `:top`, the following is true for all `X`:

```
(unify X :top) => X
```

In other words, `:top` is the [identity
element](https://en.wikipedia.org/wiki/Identity_element) of
unification. It is the most unspecific, most general value possible.

The result of unifying any value with `:top` is that same value, just
as in arithmetic, the result of multiplying any number with 1 is that same number.

### Unification with non-`:top` values.

Continuing with our `foo` map above. Now, let's unify it with another map: `{:c 42}`:

```
user=> (dag/pprint (dag/unify foo {:c 42}))
{:c [[1] 42], :a {:b [1]}}
```

Since `foo`'s value for `:c` is `:top`, the special identity element,
when we unify that with 42, the result is that same value 42.

However, suppose we unify that value again, with this map `{:a {:b 99}}`:

```
user=> (dag/pprint (-> foo (dag/unify {:c 42}) (dag/unify {:a {:b 99}})))
:fail
```

Unification of these three input maps _failed_, because it could not unify
the two non-identical values 42 and 99, within the three input maps.

### The special keyword `:fail`

The result of unifying any _a_ and _b_ is `:fail`, if: 
- _a_ and _b_ are not `:top`, and
- _a_ and _b_ are not maps (i.e. they are numbers, strings, keywords, etc), and
- `(= a b) => false`

Thus in the example above, `(unify 42 99) => :fail`, since 42 and 99 are not maps and
`(= 42 99) => false`.

The result of unifying any _a_ and _b_ is also `:fail`, if: 
- Either _a_ and _b_ are `:fail`.

Thus the result of unifying any value with `:fail` is `:fail`, just as
in arithmetic, the result of multiplying any number with 0 is 0.

## `clojure.core/get-in` vs. `dag_unify.core/get-in`


For the above graph like `foo`:

```
user=> (dag/pprint foo)
{:a {:b [[1] :top]}, :c [1]}
```

Compare the output of `clojure.core/get-in` on `foo` using the path `[:a :b]`:

```
user=> (get-in foo [:a :b])
#object[clojure.lang.Atom 0x6ee6ded {:status :ready, :val :top}]
```

Versus the output of `dag_unify.core/get-in` with `foo` on the same path:

```
user=> (dag/get-in foo2 [:a :b])
:top
```

Thus `dag_unify.core/get-in` resolves any atoms it finds as it
traverses within its input map and returns the value within the atom
rather than the atom itself.

## Unification with atoms whose values are maps

If one of the arguments to `unify` is a map with a key whose value is
an atom, then the value of that key will still be that same atom, but its
value will be an atom whose value is the unification of the arguments. For example:

```
(let [shared-value (atom {:b 42})
      foo {:a shared-value}
      bar {:a {:c 43}}]
	(dag/unify foo bar))
=> {:a #<Atom@344dc027: {:c 43, :b 42}>}
```

Above, `foo`'s value for `:a` is a reference to the value `{:b
42}`. `foo`'s value for `:a` is unified with `bar`'s value for `:a`
(`{:c 43}`), and the result `{:b 42, c 43}` is the new value of the
reference, and this reference is the value `:a` for the unification of
`foo` and `bar`.

In a graph where there is only a single path to an atom,
the atom will not be shown by `dag_unify.pprint` for legibility; thus, looking
at the same example immediately above, but with `dag_unify.pprint`:

```
(let [shared-value (atom {:b 42})
      foo {:a shared-value}
      bar {:a {:c 43}}]
	(dag/pprint (dag/unify foo bar)))
=> {:a {:c 43, :b 42}}
```

### `:fail` within maps

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

- [CHANGELOG.md](CHANGELOG.md)
