## Automatic Differentiation

I read [this](https://vmartin.fr/understanding-automatic-differentiation-in-30-lines-of-python.html) and thought it would be fun to play around with the idea in Haskell. I used [Parsec](https://github.com/haskell/parsec/tree/master) to parse expressions and variables (bound to scalar values as per the article) to be evaluated and differentiated with respect to one of the variables.

Run the automatic differentiation unit tests with `cabal test`, or the stub app using the expression parser with `cabal run`.

#### TODO list
* Support more math functions
* Write more differentiation test cases
* Pass state obj to `eval` and `diff` so we don't have to bind each Var to a fixed value
* Remove `var@scalar` notation in parser to support the above :point_up:
* Implement supplying a range of values for each variable and evaluating at each in sequence
* Use `Graphics.Gnuplot.Simple` to graph the `eval` and `diff` results on supplied ranges
* Learn more Haskell!

