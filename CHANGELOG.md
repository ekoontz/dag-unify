# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [1.9.2] - 2020-04-15

- Improve `dag_unify.pprint`: output looks like:

```
{:c [[1] 42], :a {:b [1]}}
```

rather than:

```
{:c #<Atom@5fdb9f9b: 42>, :a {:b #<Atom@5fdb9f9b: 42>}}
```

## [1.9.1] - 2020-04-10

- Removed support for `:not`: Can't remember it why or if this was ever needed; so if not needed,
  I'm removing it for simplicity's sake.
- Separate `unify!`'s handling of unifying two DAGs (maps with references) into a separate function, `unify-dags`.
- Remove unneeded `(defn all-refs)` and related functions that were used by `unify!`: instead, doing cycle
  detection by passing references to `unify!` and `unify-dag`. Performance difference is not measurably better
  or worse, but at least amount of called code is smaller.

## [1.9.0] - 2020-03-30

- move diagnostic functions (fail-path, pprint, strip-refs, isomorphic?) to a new namespace: dag_unify.diagnostics
- unify! and unify both only take exactly 2 arguments (not 1 or more than 2); clojure.core/reduce can be used to
  instead to achieve the same effect.

## [1.8.0] - 2020-03-29

- rewrite (copy): instead of serializing and then deserializing, traverse the input map
and create return value as we go.

## [1.7.11] - 2020-03-24

- serialization optimizations

## [1.7.10] - 2020-03-23

- ~serialize: rewrite to improve performance
- ~merge: cleanup

## [1.7.9] - 2020-03-08

### Changed

- Improve performance of serialization functions by duduplicating the set of references of the input structure.

## [1.7.8] - 2020-03-08

### Changed

- Reduce size of structures where there is no internal structure sharing: avoid copying entire structure into
dag_unify.serialization/serialized.

## [1.7.7] - 2020-02-27

### Added

- (defn serialization/merge-skeleton): used during serialization to reduce size of serialized representation
and number of atoms in unserialized representation.

## [1.7.1] - 2019-11-1

### Changed

- unify! now includes merge-with-keys's loop within it.

### Removed

- simple-unify
- fail?
- failr?
- fail-path-between
- isomorphic?
- merge
- merge-with-keys (contents folded into unify!)

## [1.7.0] - 2019-10-30
### Added
- vec-contains?: contains? implementation that only works on vectors. Intention is
that if v is small, it's less expensive just to search it than create a set and then
(presumably) binary-search the tree behind that set.

### Changed
- unify! throws an exception if the references of val2 contain val1, or
the references of val1 contain val2.
- unify! calls vec-contains? rather than (contains?): the latter requires constructing
a set and does an efficient search on it. vec-contains? takes a vector and does an
inefficient (linear) search on it, but seems to be faster in practice if the input vector
is small.

### Removed
- Anything?

