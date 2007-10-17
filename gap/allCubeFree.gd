#############################################################################
##
#W  allCubeFree.gd                Cubefree                    Heiko Dietrich
##                                                             
##


############################################################################# 
## 
#F  CountAllCFGroupsUpTo( n[,bool] ) 
## 
## The input is a positive integer <n> and the output is a list $L$ of 
## size <n> such that $L[i]$ contains the number of isomorphism types of 
## groups of order $i$ if $i$ is cubefree and $L[i]$ is not bound, otherwise,
## $1\leq i \leq n$. The {\SmallGroups} library  is used for squarefree 
## orders, orders of the type $p^2$ and $p^2q$, and cubefree orders less than
## 50000. Only if <bool> is set to false, then only  the squarefree orders 
## and orders of the type $p^2$ and $p^2q$,are taken from the {\SmallGroups} 
## library. This function was implemented only for experimental purposes and 
## its implementation could be improved.
##
DeclareGlobalFunction("CountAllCFGroupsUpTo");
