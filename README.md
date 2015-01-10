# Capita Selecta of Programming Languages: Homework

Code for the assignments of the Capita Selecta of Programming Languages course
at the Vrije Universiteit Brussel. The first assignment was a simple type
checker and evaluator for the language described in Chapter 8 of the book Types
and Programming Languages by B. C. Pierce, with the addition of some language
constructs. The second assignment asked for the extension of this with the
simply typed lambda calculus as described in chapter 9. The final assignment is
concerned with implementing System Fω as described in chapter 30. At this point
we have also decided to radically change the syntax for clearer reading and less
ambiguity. This is done by using Lisp-like syntax.

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
tokens (the ones we got from the lexer) and outputs something of type `[Exp]`,
another type we define. The expected format that Happy requires to create a
`calc` function is described in its
[documentation](http://www.haskell.org/happy/doc/html/sec-using.html).

Running Happy on `Parser.y` creates `Parser.hs` which can then be used as any
regular Haskell file.

### Type checking

The type checker resides in `Main.hs` under the name `findType`. Though this
simply immediately calls `getTypeAll` with a starting environment. At this point
there is a list of expressions `Exp`. Actual analysis of each is done in
`getType`. Following the rules as described in the book, this function finds out
the type of an expression by recursively finding the types of the subexpressions
that are of relevance. It is aided in this endeavour by `getKind`, which allows
to find the kind of a type expression, and `eqType` which handles type
equivalence.

As a very simple example, `(if t1 t2 t3)` will be of type `T` if `t1` is of type
`Bool`, `t2` is of type `T` and `t3` is of type `T`. So the types of the three
arguments are checked to see if things add up. The base cases are `true`,
`false` and `0` which are of type `bool`, `bool` and `int`, respectively.

If something is badly typed, an error is thrown.

### Evaluator

The evaluator is also defined in `Main.hs`, under the name `evalAll` which, like
`getTypeAll` does to `getType`, offloads the actual work to `eval`. Before
evaluating an expression, it evaluates its terms as needed - in accordance with
the rules as described in the book. Afterwards the actual evaluation takes
place (computation rule).

### Other

A few substitution functions are present which are needed in both the type
checker and the evaluator. Finally there is also a helper function which checks
whether something is a value, as considered by the syntax definition.

### Main

The actual execution is handled by the `main` function in `Main.hs`. It takes
input from standard input and parses it using first the `lexer` and then the
`calc` function. This resulting expression is then analyzed first by the type
checker (`findType`) and then evaluated by the evaluator (`evalAll`).

Output of each of these three steps is written to standard output.

## Compiling the code

A `Makefile` is provided, so simply run `make`.

## Running the code

The example program resides in `exampleprogram.csopl`. It can be fed into `Main`
the usual way. However, since the output is rather lengthy, you are advised to
either dump the result in a file or piping it to `less`.

    ./Main < exampleprogram.csopl > tmp

`Main` reads from the standard input, so you have some options to test a
particular input of your liking (non-exhaustive list).

1. Run `./Main`, enter what you want parsed, press `Enter`, then `CTRL+D` to end
   your input.
2. Run `./Main <<< "your input"`.
3. Run `echo "your input" | ./Main`.
4. Save your input in a file and feed it to `Main` as was done with the example
    program.

## Syntax

A quick overview of the syntax of the implemented language. Booleans are present
as `true` and `false`. The number zero is simply `0`. Successors and
predecessors of a number `n` can be found by using `(succ n)` and `(pred n)`. If
statements appear as `(if condition truebranch falsebranch)`.

Abstractions from term to term are introduced using `(λ variable type body)`.
Application of `f` on `x` via `(f x)`. Type abstractions have slightly differing
syntax to prevent confusion with regular abstractions: `(Λ variable kind body)`.
The same goes for application, which is done with `[f x]`.

The type mentioned in the previous paragraph has as basis `int` and `bool`.
These can be combined to arrow types described as `(→ type₁ type₂)`. There is
also operator abstraction which is rather similar in looks to regular
abstraction, `(λ variable kind body)`, with the same being true for operator
application: `(f x)`. The universal type finally can be entered as
`(∀ variable kind body)`.

Kinding is rather simple. It is either `*` or of the form `(⇒ kind kind)`.

Finally at the top level one can use `(define variable body)` to bind variables
similarly to how it would be done in an abstraction. Comments can be placed
anywhere and start with a `;`. They run till the end of the line.
