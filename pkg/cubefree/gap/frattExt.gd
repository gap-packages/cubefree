#############################################################################
##
#W  frattExt.gd           Cubefree                             Heiko Dietrich
##                                                               
#H   @(#)$Id: $
##


############################################################################# 
## 
#F FrattiniExtensionCF( code, o ) 
## 
## Computes the Frattini extensions of the group given by 'code' of order 'o'
DeclareGlobalFunction("FrattiniExtensionCF");
 
############################################################################# 
## 
#F ConstructAllCFGroups( size ) 
## 
## Computes all cube-free groups of order n up to isomorphism
##
DeclareGlobalFunction("ConstructAllCFGroups");

############################################################################# 
## 
#F ConstructAllCFSolvableGroups( size ) 
## 
## Computes all cube-free solvable groups of order n up to isomorphism
##
DeclareGlobalFunction("ConstructAllCFSolvableGroups");
