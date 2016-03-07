#  Space

A minimal Newtonian gravity and collision simulator in Clojure

## Demo

Demo rendered with the help of [Quil](https://github.com/quil/quil)

#### Gravitational constant adjustments

![Gravity](https://raw.githubusercontent.com/balysv/space/master/assets/gravity.gif)

#### Permutations applied to every entity in space. Currently supported:
- `apply-movement` - adjust position according to velocity vector
- `apply-gravity` - applies gravitational forces from each entity to another
- `apply-inelastic-collisions` - collision strategy to merge entities into one preserving their properties and momentum
- `apply-elastic-collisions` - collision strategy to bounce entities that touch with equivalent force

![Permutations](https://raw.githubusercontent.com/balysv/space/master/assets/permutations.gif)

## Running

The project uses [Leiningen](http://leiningen.org/)

`lein run` or `lein repl`

and 

`lein test`

## License

Eclipse Public Licence 1.0 or any later version.



