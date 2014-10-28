# Capita Selecta of Programming Languages: Homework 1

Code for the first assignment of the Capita Selecta of Programming Languages
course at the Vrije Universiteit Brussel. A simple type checker and evaluator
for the language as described in Chapter 8 of the book Types and Programming
Languages by B. C. Pierce, with the addition of some language constructs.

## Technology

Versions mentioned are those used during development.

* [Haskell](http://www.haskell.org/haskellwiki/Haskell), `ghc` version 7.6.3
* [Happy - Parser generator for Haskell](http://hackage.haskell.org/package/happy), version 1.19.0

## Reading the code

### Lexer

The lexer is defined in `Parser.y`. It takes the input string and turns it into
a list of `Token`s.

### Parser

The parser is created by Happy according to the rules described in `Parser.y`.
Happy uses them to creates a `calc` function that takes as input a list of
tokens (the ones we got from the lexer) and outputs something of type `Exp`,
another type we define. The expected format that Happy requires to create a
`calc` function is described in its
[documentation](http://www.haskell.org/happy/doc/html/sec-using.html).

Running Happy on `Parser.y` creates `Parser.hs` which can then be used as any
regular Haskell file.

### Type checking

The type checker resides in `Main.hs` under the name `getType`. Following the
rules as described in the book, it finds out the type of an expression by
recursively finding the types of the subexpressions that are of relevance.

For example, `if t1 t2 t3` will be of type `T` if `t1` is of type `Bool`, `t2`
is of type `T` and `t3` is of type `T`. So the types of the three arguments are
checked to see if things add up. The base cases are `true`, `false` and `zero`
which are of type `Bool`, `Bool` and `Nat`, respectively.

If something is badly typed, this is indicated with an appropriately named
return value. (yes, I realize this isn't very Haskell-y)

### Evaluator

The evaluator is also defined in `Main.hs`, under the name `eval`. Before
evaluating an expression, it evaluates its terms as needed - in accordance with
the rules as described in the book. After this, pattern matching is used to be
able to apply the correct rule.

Note that a function was added to convert a `Value` (that is, something at the
end of a chain of evaluation) back to an `Exp` as sometimes the result of an
evaluation would require a new evaluation on a different expression. Needing
this helper function was a consequence of deciding to use a different type as
the return type of the evaluation function.

In the case of evaluation getting stuck due to a lack of evaluation rules
(for example, `pred true`), an error is thrown. (again, not very Haskell-y)

### Main

The actual execution is handled by the `main` function in `Main.hs`. It takes
input from standard input and parses it using first the `lexer` and then the
`calc` function. This resulting expression is then analyzed first by the type
checker (`getType`) and then evaluated by the evaluator (`eval`).

Output of each of these three steps is written to standard output.

## Compiling the code

A `Makefile` is provided, so simply run `make`.

## Running the code

`Main` reads from the standard input, so to test something of your liking you
have some options (non-exhaustive list)

1. Run `./Main`, enter what you want parsed, press `Enter`, then `CTRL+D` to end
   your input.
2. Run `./Main <<< "your input"`.
3. Run `echo "your input" | ./Main`.

You can also run `test.sh`, which will run every expression in `test_input.txt`
one after the other.
