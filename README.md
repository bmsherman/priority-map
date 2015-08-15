priority-map
============

See the [Haddock documentation](http://bmsherman.github.io/haddock/priority-map/).

This Haskell package provides a mutable data structure which combines a
heap with a map to allow random heap access. It seems to have various names,
such as *priority map* and *indexed priority queue*. There are comparable
data structures available in other languages:
[priority-map in clojure](http://clojure.github.io/data.priority-map/) and 
[pqdict in python](https://pypi.python.org/pypi/pqdict/);
the [pqdict page](https://pypi.python.org/pypi/pqdict/) gives a particularly
nice overview of the data structure and its properties.

While Haskell data structures are often immutable, this implementation bucks
that trend: the heap is implemented using a dynamically growing mutable array.
There is a reason for this! Suppose we want to delete an element with a
particular map key. Lookup up the key in the map tells us where the element is
in the heap. With an immutable heap, we still have to take time proportional
to the height of the element in the heap in order to reach it, but with the
array-based heap, we can access it immediately. So the mutable version has
better algorithmic complexity.

Because of this, the priority map data structure lives inside ST. However,
the priority map is actually polymorphic in the map data structure used.
A default implementation using Data.Map is provided.
