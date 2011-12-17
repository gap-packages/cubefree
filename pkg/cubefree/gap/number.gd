#############################################################################
##
#W  number.gd           Cubefree                               Heiko Dietrich
##                                                              
##


##############################################################################
##
#F  NumberCFSolvableGroups( n,[bool] )
##
## The input <n> has to be a positive cubefree integer and the output is the 
## number of all cubefree solvable groups of order <n>. The {\SmallGroups} 
## library  is used for squarefree orders, orders of the type $p^2$ and
## $p^2q$, and cubefree orders less than 50000. Only if <bool> is set to 
## false, then only  the squarefree orders and orders of the type $p^2$ and 
## $p^2q$,are taken from the {\SmallGroups} library.
##
DeclareGlobalFunction("NumberCFSolvableGroups");


##############################################################################
##
#F  NumberCFGroups( size )
##
## The input <n> has to be a positive cubefree integer and the output is the 
## number of all cubefree groups of order <n>. The {\SmallGroups} 
## library  is used for squarefree orders, orders of the type $p^2$ and
## $p^2q$, and cubefree orders less than 50000. Only if <bool> is set to 
## false, then only  the squarefree orders and orders of the type $p^2$ and 
## $p^2q$,are taken from the {\SmallGroups} library.
##
DeclareGlobalFunction("NumberCFGroups");
