# Lab 3 : Genetic Coding in OCaml
One of the amazing scientific discovery of the century is that inherited characteristic of all Earth's life-forms can be encoded in DNA by surprisingly simple sequences of just four chemical compounds, called bases. Furthermore, biochemists have been able to decipher large parts of this code, and can now reliably describe
large parts of the genotype of many living things by listing appropriate sequences of bases.

The four bases that make up DNA are guanine (G), adenine (A), cytosine (C), and thymine (T). A gene is a long string of (typically several thousand of) these bases. The presence or absence of particular bases, and their order, are significant.

A small but important tool in cracking the genetic code is the determination of how similar two different sequences are. One metric for determining similarity is described in Cormen, Leiserson, Rivest, and Stein (Problem 15-3). Each of the two sequences being compared (x and y) is modified by inserting spaces at arbitrary
locations (including at either end) so that the resulting sequences (x0 and y0) are of the same length k but do not have a space in the same position (i.e. for no position j are both x0[j] and y0[j] a space).
Then we assign an integer score to each position j:

sc(j)  
= +1 if x0[j] = y0[j]  
= -1 if x0[j] != y0[j], and neither is a space  
= -2 if either x0[j] or y0[j] is a space.

The score for an alignment is the sum of the scores over all positions: sum {j=1 to k} sc(j). 

The aim is to find an alignment that maximizes the total score. For example, given sequences x = GATCGGCAT and y = CAATGTGAATC, three different alignments (and their scores) are:

    G ATCG GCAT
    CAAT GTGAATC
    -*++*+*+-++* (-4)

    GA TC GG  CAT
    CAATGTGAATC
    -+*+-*+-**+** (-11)

    GATCGGCA T
    CAATGTGAATC
    -+--+--+*-* (-7)

A + under a position indicates a score of +1 for that position,  
a - indicates a score of -1, and  
a * indicates a score of -2;  
so the first alignment shown has a total score of (6x1)+(2x-1)+(4x-2) = -4.

Among these three, this is the best alignment.

## Tasks
1. Using Ocaml, write a recursive procedure that would compute one best alignment between two sequences (given as strings). Print your outcome as follows:

   G ATCG GCAT  
   CAAT GTGAATC  
   -*++*+*+-++*  
   Score : -4

2. Using memorization, drastically improve the performance of your algorithm. If you like, you may use an OCaml object and Hash Table.
