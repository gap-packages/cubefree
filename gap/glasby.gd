##############################################################################
##
#W  glasby.gd           Cubefree                               Heiko Dietrich
##
##

##############################################################################
## 
#F  RewriteAbsolutelyIrreducibleMatrixGroup( G ) 
## 
## G has to be an absolutely irreducible subgroup of GL(d,q). The output
## is a group H such that H is conjugate to G in GL(d,q) and H is a subgroup
## of GL(d, r) where GF(r) is the subfield of GF(q) generated by the traces 
## of the elements in G. This implementation is based on an algorithm of 
## Howlett and Glasby.
##
DeclareGlobalFunction("RewriteAbsolutelyIrreducibleMatrixGroup");
