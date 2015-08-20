Example of a union find algorithm in Haskell.

Usage:

    app gen 100000 13 > in
    app < in > out

The first command generates 1000000 random sets with the seed 13.
The second command will merge the sets and write the results to stdout.

Example input file:

    6
    dog cat 
    cat gerbil
    snake
    lion tiger
    bear lion
    gerbil hamster mouse

There are 6 sets of words, each of the following 6 lines is a set of equivalent words.

The program outputs the merged equivalence classes, resulting in:

    dog cat gerbil hamster mouse
    snake
    lion tiger bear

