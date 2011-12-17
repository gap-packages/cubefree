#############################################################################
##
#W  frattExt.gd           Cubefree                             Heiko Dietrich
##                                                               
##


############################################################################# 
## 
#F FrattiniExtensionCF( code, o ) 
## 
## Computes the Frattini extensions of the group given by <code> 
## of order <o>. This is a modification of a method in GrpConst.
##
DeclareGlobalFunction("FrattiniExtensionCF");
 
############################################################################# 
## 
#F ConstructAllCFGroups( size ) 
## 
## The input <size> has to be a positive cubefree integer. The output is a 
## complete and irredundant list of isomorphism type representatives of 
## groups of this size. If possible, the groups are given as pc groups and 
## as permutations groups otherwise.
##
DeclareGlobalFunction("ConstructAllCFGroups");

############################################################################# 
## 
#F ConstructAllCFSolvableGroups( size ) 
## 
## The input <size> has to be a positive cubefree integer. The output is a 
## complete and irredundant list of isomorphism type representatives of 
## solvable groups of this size. The groups are given as pc groups.
##
DeclareGlobalFunction("ConstructAllCFSolvableGroups");



############################################################################# 
## 
#F CubefreeTestOrder( n )
##
## The input must be a cubefree integer between 1 and 50000. This functions
## tests the functionality of Cubefree and compares it with the SmallGroups-
## library. It returns true if everything is okay, otherwise an error will
## be displayed.
##
DeclareGlobalFunction("CubefreeTestOrder");