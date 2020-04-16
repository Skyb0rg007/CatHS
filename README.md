# Cat implementation in Haskell

Reasons for doing so:

* Removes requirement to think about lifetimes and cloning
* Recursion via higher-order functions, rather than Iterators
* More comfortable with the language
* Laziness (see next)

* Because Haskell is lazy, we can skip the X64S intermediate language:
  - When compiling an LIRFunction, we can use the labels of the other functions
    before they are created via MonadFix
* With Monads, we can hide away the implementation details of compilation,
  exposing higher level combinators so that assembly looks more like the real thing
* With GADTs and DataKinds, the 'Operator' type is parametrized by whether or
  not the operator is read-only (such as a constant). By annotating each
  Assembly operator individually, we get additional type safety that Rust does not afford
* By using `polysemy`, we can get rid of the 'GlobalState' arguments to functions,
  and instead implicitly thread the state throughout our programs

