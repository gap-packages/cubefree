gap> START_TEST("construct.tst");

#
gap> testConstructAllCFSolvableGroups:= function(n)
>   local L, L2;
>   L  := ConstructAllCFSolvableGroups(n);;
>   L  := List(L, x->rec(order:=n, code:=CodePcGroup(x)));;
>   L2 := RandomIsomorphismTest(L,5);;
>   return L = L2;
> end;;
gap> Assert(0, testConstructAllCFSolvableGroups(1300));
gap> Assert(0, testConstructAllCFSolvableGroups(15345));
gap> Assert(0, testConstructAllCFSolvableGroups(23079686));

# ConstructAllCFGroups: test corner/edge cases
gap> ConstructAllCFGroups(0);
Error, Argument has to be a positive cube-free integer.
gap> ConstructAllCFGroups(1);
[ <pc group of size 1 with 0 generators> ]
gap> List(ConstructAllCFGroups(2), IdGroup);
[ [ 2, 1 ] ]
gap> ConstructAllCFGroups(4);
[ <pc group of size 4 with 2 generators>, 
  <pc group of size 4 with 2 generators> ]
gap> ConstructAllCFGroups(8);
Error, Argument has to be a positive cube-free integer.

# ConstructAllCFSolvableGroups: test corner/edge cases
gap> ConstructAllCFSolvableGroups(0);
Error, Argument has to be a positive cube-free integer.
gap> ConstructAllCFSolvableGroups(1);
[ <pc group of size 1 with 0 generators> ]
gap> List(ConstructAllCFSolvableGroups(2), IdGroup);
[ [ 2, 1 ] ]
gap> ConstructAllCFSolvableGroups(4);
[ <pc group of size 4 with 2 generators>, 
  <pc group of size 4 with 2 generators> ]
gap> ConstructAllCFSolvableGroups(8);
Error, Argument has to be a positive cube-free integer.

# ConstructAllCFSimpleGroups: test corner/edge cases
gap> ConstructAllCFSimpleGroups(0);
Error, Argument has to be a positive cube-free integer.
gap> ConstructAllCFSimpleGroups(1);
[  ]
gap> List(ConstructAllCFSimpleGroups(2), IdGroup);
[ [ 2, 1 ] ]
gap> ConstructAllCFSimpleGroups(4);
[  ]
gap> ConstructAllCFSimpleGroups(8);
Error, Argument has to be a positive cube-free integer.

# ConstructAllCFNilpotentGroups: test corner/edge cases
gap> ConstructAllCFNilpotentGroups(0);
Error, Argument has to be a positive cube-free integer.
gap> ConstructAllCFNilpotentGroups(1);
[ <pc group of size 1 with 0 generators> ]
gap> List(ConstructAllCFNilpotentGroups(2), IdGroup);
[ [ 2, 1 ] ]
gap> List(ConstructAllCFNilpotentGroups(4), IdGroup);
[ [ 4, 1 ], [ 4, 2 ] ]
gap> ConstructAllCFNilpotentGroups(8);
Error, Argument has to be a positive cube-free integer.

# CubefreeOrderInfo
gap> CubefreeOrderInfo(0);
Argument has to be a positive cube-free integer.
0
gap> CubefreeOrderInfo(1);
1
gap> CubefreeOrderInfo(2);
#I This order is either squarefree or of the type p^2, p^2q.
#I You can use 'AllSmallGroups' or 'NumberSmallGroups'
#I of the SmallGroups library.
#####################################################################
#I -- Information: Construction of the groups of order 2 --
#I What kind of socle complements are to construct:
[  ]
#I
#I The above list has entries [n,[[p1,e1],..,[pl,el]]] with n>1
#I and for such an entry one has to construct up to conj. all
#I subgroups of order n of GL(p1,e1)x...xGL(pl,el) (socle complements).
#I The following GL(1,p)'s are to consider: p in  [  ]
#I The following GL(2,p)'s are to consider: p in [  ]
#I
#I The possible solvable direct factors have orders [ 2 ]
#I The number of pairs
#I      (order of solv. Fratt.-free group / order of its socle)
#I for solvable Frattini-free groups which are to compute is 1.
#####################################################################
1
gap> CubefreeOrderInfo(4);
#I This order is either squarefree or of the type p^2, p^2q.
#I You can use 'AllSmallGroups' or 'NumberSmallGroups'
#I of the SmallGroups library.
#####################################################################
#I -- Information: Construction of the groups of order 4 --
#I What kind of socle complements are to construct:
[  ]
#I
#I The above list has entries [n,[[p1,e1],..,[pl,el]]] with n>1
#I and for such an entry one has to construct up to conj. all
#I subgroups of order n of GL(p1,e1)x...xGL(pl,el) (socle complements).
#I The following GL(1,p)'s are to consider: p in  [  ]
#I The following GL(2,p)'s are to consider: p in [  ]
#I
#I The possible solvable direct factors have orders [ 4 ]
#I The number of pairs
#I      (order of solv. Fratt.-free group / order of its socle)
#I for solvable Frattini-free groups which are to compute is 2.
#####################################################################
2
gap> CubefreeOrderInfo(8);
Argument has to be a positive cube-free integer.
0
gap> CubefreeOrderInfo((2*3*5)^2*7*11);
#####################################################################
#I -- Information: Construction of the groups of order 69300 --
#I What kind of socle complements are to construct:
[ [ 30, [ [ 7, 1 ], [ 11, 1 ] ] ], [ 15, [ [ 2, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 10, [ [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 6, [ [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 5, [ [ 2, 1 ], [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 3, [ [ 2, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 2, [ [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 60, [ [ 7, 1 ], [ 11, 1 ] ] ], [ 30, [ [ 2, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 20, [ [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 15, [ [ 2, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 12, [ [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 10, [ [ 2, 1 ], [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 6, [ [ 2, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 5, [ [ 2, 2 ], [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 4, [ [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 3, [ [ 2, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 2, [ [ 2, 1 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 30, [ [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 15, [ [ 2, 1 ], [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 10, [ [ 3, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 6, [ [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 5, [ [ 2, 1 ], [ 3, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 3, [ [ 2, 1 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 2, [ [ 3, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 30, [ [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 15, [ [ 2, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 10, [ [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 6, [ [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 5, [ [ 2, 1 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 3, [ [ 2, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 2, [ [ 3, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 60, [ [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 45, [ [ 2, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 30, [ [ 2, 1 ], [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 20, [ [ 3, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 15, [ [ 2, 2 ], [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 12, [ [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 10, [ [ 2, 1 ], [ 3, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 9, [ [ 2, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 6, [ [ 2, 1 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 5, [ [ 2, 2 ], [ 3, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 4, [ [ 3, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 3, [ [ 2, 2 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 2, [ [ 2, 1 ], [ 3, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 60, [ [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 30, [ [ 2, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 20, [ [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 15, [ [ 2, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 12, [ [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 10, [ [ 2, 1 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 6, [ [ 2, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 5, [ [ 2, 2 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 4, [ [ 3, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 3, [ [ 2, 2 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 2, [ [ 2, 1 ], [ 3, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 30, [ [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 18, [ [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 15, [ [ 2, 1 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 10, [ [ 3, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 9, [ [ 2, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 6, [ [ 3, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 5, [ [ 2, 1 ], [ 3, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 3, [ [ 2, 1 ], [ 3, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 2, [ [ 3, 2 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 60, [ [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 45, [ [ 2, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 36, [ [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 30, [ [ 2, 1 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 20, [ [ 3, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 18, [ [ 2, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 15, [ [ 2, 2 ], [ 3, 1 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 12, [ [ 3, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 10, [ [ 2, 1 ], [ 3, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 9, [ [ 2, 2 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 6, [ [ 2, 1 ], [ 3, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 5, [ [ 2, 2 ], [ 3, 2 ], [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 4, [ [ 3, 2 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 3, [ [ 2, 2 ], [ 3, 1 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 2, [ [ 2, 1 ], [ 3, 2 ], [ 5, 2 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 15, [ [ 7, 1 ], [ 11, 1 ] ] ], [ 5, [ [ 3, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], 
  [ 3, [ [ 5, 1 ], [ 7, 1 ], [ 11, 1 ] ] ], [ 3, [ [ 5, 1 ], [ 7, 1 ] ] ] ]
#I
#I The above list has entries [n,[[p1,e1],..,[pl,el]]] with n>1
#I and for such an entry one has to construct up to conj. all
#I subgroups of order n of GL(p1,e1)x...xGL(pl,el) (socle complements).
#I The following GL(1,p)'s are to consider: p in  [ 2, 3, 5, 7, 11 ]
#I The following GL(2,p)'s are to consider: p in [ 2, 3, 5 ]
#I
#I The possible solvable direct factors have orders [ 69300, 1155, 105 ]
#I The number of pairs
#I      (order of solv. Fratt.-free group / order of its socle)
#I for solvable Frattini-free groups which are to compute is 94.
#####################################################################
94

#
gap> STOP_TEST( "construct.tst", 1);
