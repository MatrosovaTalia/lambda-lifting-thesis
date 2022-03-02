# lambda-lifting-thesis
The source code for the Bachelor thesis: Lambda lifting via data parallel syntax tree representation in Haskell.

[Work is in progress].
The final goal of the research is to build a language independent  library for parallel computations. 
The main purpose of this work is to research the function (lambda) lifting mechanism for the parallel computation on GPU. This work is partially based on Aaron Hsu paper on data-parallel compilers. 

Aaron Hsu in his thesis refers to function lifting in the context of creating a fully data-parallel compiler using lambda lifting technique. The algorithm is written for APL language. Operations on an AST, especially grouping the nodes of the Node Coordinate Matrix, are performed with the help of the Key operator, which does not have an analogue in Haskell programming language. 

This article demonstrates that it is possible to use Accelerate Haskell library to work with parallel arrays. 


