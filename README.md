# Differentiate

Differentiate any algebraic expression

This is something I wrote while learning functional programming in Haskell. I had already written something similar in [objected-oriented Python](https://github.com/archisman-dey/differentiate), and I learned a lot from rewriting it in a purely functional way.

Supported functions: `sin`, `cos`, `tan`, `sec`, `log`, `exp`, `sqrt`

The book I used to learn Haskell - [Learn You a Haskell for Great Good](http://learnyouahaskell.com/chapters)

The parser is written using Parsec - [Parsec](https://wiki.haskell.org/Parsec)

## How to run

    ghc --make differentiate.hs
    ./differentiate

### Notes

* Use `~` instead of `-` for unary minus.
* Integers are not supported yet, use float instaed : 2.0 instead of 2.
* Brackets have to be used in some places to get the expression parsed correctly, specifically, while writing unary expressions.

## Examples

```
$ ./differentiate
Expression in terms of x? (Note: Use ~ for unary minus)
sin(cos(x))
Parsed as: (sin(cos('x')))
Result: ((cos(cos('x'))) * (-(sin('x'))))

$ ./differentiate
Expression in terms of x? (Note: Use ~ for unary minus)
sin(x)*(cos(x)-(tan(x)))
Parsed as: ((sin('x')) * ((cos('x')) - (tan('x'))))
Result: (((sin('x')) * ((-(sin('x'))) - ((sec('x')) ^ (2.0)))) + ((cos('x')) * ((cos('x')) - (tan('x')))))
```
