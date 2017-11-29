# mcon

A Clojure library for multi-strategy (and meta-strategy) software contracts.

## Installation

1. clone this repository
2. do `lein install`
3. add to your project dependencies `[mcon "0.1.0-SNAPSHOT"]`
4. add to your namespace declaration `(:require [mcon.core :as mcon])`

## Usage

The basic system allows you to write contracts for programs under different
enforcement strategies:

    (def natc (predc (fn [x] (>= x 0))))
    (def monitored-fact
         (mcon/mon eager
                   (func (memo eager) natc semi natc)
                   factorial))

For a full description of strategies, see 
[the associated paper](https://github.com/cgswords/mcon/blob/master/paper.pdf).

For more examples, check out the the code in `src/mcon/paper.clj`.

## To Do

- Add a tutorial

## License

Copyright © 2017 Cameron Swords

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

