#############################################################################
##
#W  CubeFree.gd                 Cubefree                       Heiko Dietrich
#W                                                               
##
#############################################################################
##
##
#############################################################################
##
#I InfoClass
##
DeclareInfoClass( "InfoCF" );

#############################################################################
##
DeclareGlobalFunction("IsSquareFreeInt");
DeclareGlobalFunction("IsCubeFreeInt");

DeclareGlobalFunction("ConstructAllCFSimpleGroups");
DeclareGlobalFunction("ConstructAllCFNilpotentGroups");
DeclareGlobalFunction("ConstructAllCFSolvableGroups");
DeclareGlobalFunction("ConstructAllCFFrattiniFreeGroups");
DeclareGlobalFunction("ConstructAllCFGroups");

DeclareGlobalFunction("FrattiniExtensionCF");

DeclareGlobalFunction("NumberCFSolvableGroups");
DeclareGlobalFunction("NumberCFGroups");
DeclareGlobalFunction("CountAllCFGroupsUpTo");

DeclareGlobalFunction("RewriteAbsolutelyIrreducibleMatrixGroup");

DeclareGlobalFunction("IrreducibleSubgroupsOfGL");