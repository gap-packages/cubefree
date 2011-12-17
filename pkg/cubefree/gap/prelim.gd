#############################################################################
##
#W  prelim.gd           Cubefree                               Heiko Dietrich
##                                                              
##


##############################################################################
##
#P  IsCubeFreeInt( n )
##
## The output is <true> if <n> is a cubefree integer and <false> otherwise.
##
DeclareProperty( "IsCubeFreeInt", IsInt );


##############################################################################
##
#P  IsSquareFreeInt( n )
##
## The output is <true> if <n> is a squarefree integer and <false> otherwise.
##
DeclareProperty( "IsSquareFreeInt", IsInt );


############################################################################# 
## 
#F  ConstructAllCFSimpleGroups( n ) 
## 
## The input <n> has to be a positive cubefree integer. The output is a 
## complete and irredundant list of isomorphism type representatives of 
## simple groups of this size. In particular, there exists either none or 
## exactly one simple group of the given order.
##
DeclareGlobalFunction("ConstructAllCFSimpleGroups");

 
############################################################################# 
## 
#F  ConstructAllCFNilpotentGroups( n ) 
## 
## The input <n> has to be a positive cubefree integer. The output is a 
## complete and irredundant list of isomorphism type representatives of 
## nilpotent groups of this size. The groups are given as pc groups.
##
DeclareGlobalFunction("ConstructAllCFNilpotentGroups");


############################################################################# 
## 
#F  CubefreeOrderInfo( n[,bool] ) 
## 
## This function displays some (very vague) information about the complexity 
## of the construction of the groups of (cubefree) order <n>. It returns the 
## number of possible pairs <(a,b)> where <a> is the order of a Frattini-free 
## group <F> with socle <S> of order <b> which has to be constructed in order 
## to construct all groups of order <n>: In fact, for each of these pairs 
## <(a,b)> one would have to construct up to conjugacy all subgroups of order
## <a/b> of Aut<(S)>. The sum of the numbers of these subgroups for all pairs 
## <(a,b)> as above is the number of groups of order <n>. Thus the output of 
## <CubeFreeOrderInfo> is a trivial lower bound for the number of groups of 
## order <n>. There is no additional information displayed if <bool> is set 
## to false.
##
DeclareGlobalFunction("CubefreeOrderInfo");
