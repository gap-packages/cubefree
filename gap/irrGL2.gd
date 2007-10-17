##############################################################################
##
#W  irrGL2.gd           Cubefree                                Heiko Dietrich
##
##

#############################################################################
##
#O  IrreducibleSubgroupsOfGL( 2, q )   
##
## The current version of this function allows only <n>=2. The input <q> has 
## to be a prime-power <q>$=p^r$ with $p\geq 5$ a prime. The output is a 
## list of all irreducible subgroups of GL$(2,q)$ up to conjugacy.
## 
DeclareOperation("IrreducibleSubgroupsOfGL",[IsPosInt,IsPosInt]);
