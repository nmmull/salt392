# salt392: A Series of Rusty Interpreters

This is a collection of interpreteres use for the course `CAS CS 392 M1`: *Rust, In Practice and in Theory* at Boston University (Fall 2025).
They are based on *Featherweight Rust* [[David J. Pierce 2021](https://dl.acm.org/doi/10.1145/3443420)], which is based on a number of important previous systems, notably the early operational intepretations of linear logic [[Turner and Wadler 1999](https://www.sciencedirect.com/science/article/pii/S0304397599000547)].
The motivation for these systems is entirely pedogogical; I wanted a more gradual approach (à la [[Pierce 2002](https://www.cis.upenn.edu/~bcpierce/tapl/)]) to Featherweight Rust, in order to better motivate the formalisms.
Each system introduces a new construct:

* `salt0`: Straight Line Programs
* `salt1`: `salt0` + Mutable Variables, Immutable References
* `salt2`: `salt1` + Mutable References and Boxes
* `salt3`: `salt2` + Blocks (and Lifetimes)
* `salt4`: `salt3` + Tuples (and Paths)
* `salt5`: `salt4` + Conditionals (and Weak Updates)
* `salt6`: `salt5` + Functions

`salt6` is essentially full Featherweight Rust (with the extensions given in the paper).
