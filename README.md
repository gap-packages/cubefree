[![Build Status](https://travis-ci.com/gap-packages/cubefree.svg?branch=master)](https://travis-ci.com/gap-packages/cubefree)
[![Code Coverage](https://codecov.io/github/gap-packages/cubefree/coverage.svg)](https://codecov.io/gh/gap-packages/cubefree)

# The GAP 4 package Cubefree


## Installation

The installation of the Cubefree package follows standard GAP rules.
So the standard method is to unpack the package into the `pkg`
directory  of your GAP distribution.  This will create an `cubefree`
subdirectory. 


## Requirements

The package is written for GAP 4. It requires the packages GrpConst
and Polycyclic, and, if the test file `tst/testMat.g` is called, the 
package IrredSol.


## Abilities

This package contains an implementation of an algorithm to construct all 
groups of a (reasonable) given cubefree order up to isomorphism. 
This algorithm is based on the ideas in [1] and [2] and it is fully described 
in [3]. The implementation needs a method to construct all irreducible 
subgroups of GL(2,p) up to conjugacy. We use the method described in [4] 
for this purpose. In turn, the algorithm of [4] requires a method for writing
an irreducible matrix group over a minimal finite field. We use the
algorithm described in [5] for this purpose.

The main functions of the package are the following. Please see the
documentation for a more detailed description. 

1. `ConstructAllCFGroups( n )`
... constructs all groups of a given cubefree order n.

2. `ConstructAllCFSimpleGroups( n )`
... constructs all simple groups of a given cubefree order n.

3. `ConstructAllCFSolvableGroups( n )`
... constructs all solvable groups of a given cubefree order n.

4. `ConstructAllCFNilpotentGroups( n )`
... constructs all nilpotent groups of a given cubefree order n.

5. `ConstructAllCFFrattiniFreeGroups( n )`
... constructs all Frattini-free groups of a given cubefree order n.

6. `NumberCFGroups( n )`
... returns the number of all groups of a given cubefree order n.

7. `NumberCFSolvableGroups( n )`
... returns the number of all solvable groups of given cubefree order n.

8. `CountAllCFGroupUpTo( n )`
... counts all cubefree groups of order at most n. 
The output is a list L whose i.th entry is the number of groups 
of order i up to isomorphism if i is cube-free and unbound, otherwise.

9. `IrreducibleSubgroupsOfGL( 2, q )`
... computes all irreducible subgroups of GL(2,q) up to conjugacy where q=p^r
is a prime-power with p>=5.

10. `RewriteAbsolutelyIrreducibleMatrixGroup( G )`
... rewrites an absolutely irreducible subgroup G\leq GL(n,q) over the
subfield generated by the traces of the elements of G.

11. `CubefreeOrderInfo( n )`
... displays some (very vague) information about the complexity 
of the construction of the groups of (cubefree) order <n>. It returns the 
number of possible pairs (a,b) where a is the order of a Frattini-free 
group F with socle S of order b which has to be constructed in order 
to construct all groups of order n.

12. `CubefreeTestOrder( n )`
... tests the functionality of the functions (1)--(7) and compares it with 
the data of the SmallGroups library. It returns true if everything is okay,
otherwise an error message will be displayed.


For some input the counting functions use some data of the SmallGroups
library. This can be avoided, see the documentation of (6) and (7). 
Moreover, in the case of squarefree orders and orders of the type p^2 or
p^2q it is more practical to use the functions `AllSmallGroups` and 
`NumberSmallGroups` of the `SmallGroups` library; please see the documentation for 
more information.


## Test files

There are several test files to call to check the results of the standard
methods. The standard test file is `tst/testQuick.g`. Moreover, there are
`tst/testSG.g`, `tst/testSGlong.g`, `tst/testBig.g` and `tst/testMat.g`. 
Please see the documentation for more information.


## Contact

   Heiko Dietrich <heiko.dietrich@monash.edu>, Monash University

Issues can also be reported at <https://github.com/gap-packages/cubefree/issues>.


## References

[1] H. U. Besche and B. Eick.
    Construction of finite groups,
    J. Symb. Comput. **27** (1999), 387 -- 404.

[2] H. U. Besche and B. Eick.
    The groups of order at most 1000 except 512 and 768,
    J. Symb. Comput. **2** (1999), 405 -- 413.

[3] H. Dietrich and B. Eick.
    On The Groups Of Cube-Free Order
    J. Algebra **292** (2005), 122 -- 137

[4] D. L. Flannery and E. A. O`Brien.
    Linear Groups of small degree over finite fields,
    Intern. J. Alg. Comput. **15** (2005), 467 -- 502

[5] S. P. Glasby and R. B. Howlett.
    Writing representations over minimal fields,
    Comm. Alg. **25**(6) (1997) 1703 -- 1711.
