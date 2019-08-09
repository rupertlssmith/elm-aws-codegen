# elm-codegen-spike

Trying out codegen and pretty printing in Elm.

The aim here was to come up with an AST for some imaginary programming language (not a useful one), generate around 10K 
lines of code and pretty print them into a file. The purpose of this being to check that this kind of thing can run in 
Elm without being too slow, and without blowing up the stack on deeply recursive algorithms.

There are some slides for an associated talk under `/slides`. These slides are best viewed with 
[Elm Dive SVG](https://myrho.github.io/dive-svg/).
