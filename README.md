## Automatic Differentiation

### What?
I read [this](https://vmartin.fr/understanding-automatic-differentiation-in-30-lines-of-python.html) about automatic differentiation, and thought it would be fun to play around with the idea in Haskell. I was right, it was fun.

I used [Parsec](https://github.com/haskell/parsec/tree/master) to parse expressions, including variables bound to scalar values. Those expressions are then evaluated or differentiated with respect to one of the variables, like in the article. I used [HUnit](https://github.com/hspec/HUnit) to write some unit tests. I used [cabal](https://www.haskell.org/cabal/) to build and test the project.

TODOs:
* More tests, this thing can't possibly work
* Ditch the `<var>@<scalar>` syntax and accept a range of values to bind to each variable
* Graph the resulting curves
* Learn more Haskell


#### How
Run the automatic differentiation unit tests with `cabal test`, or the stub app using the expression parser with `cabal run`.
