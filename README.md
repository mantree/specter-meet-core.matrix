# specter-core-matrix

An experiment trying to get [Specter](https://github.com/nathanmarz/specter) to work with [core.matrix](https://github.com/mikera/core.matrix) datasets.

Look to the tests to see what this kind of looks like. At the moment it's not looking super promising, generally rows seems to produce good looking paths, but is inefficent. The column navigation is running in to trouble when it comes to deriving columns. The as-map method makes some things a little easier, but is still very hampered.

It does look like something like this could be used for selecting data, but transformations I'm struggling to get to work in ways that I find acceptable. The trouble is if it only works well for selecting, it makes it not really worth using as for all interesting transformations you'd need other functions.

The experiments thus far have been limited to simple 2-D matrices though. Perhaps more use will come with more complex data structures, which is what Specter is really aimed at anyway.

I think what's here only scratches the surface of problems with the idea really. If you start to look at joins, rollups or rotations then I can't see this being a good fit really.

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
