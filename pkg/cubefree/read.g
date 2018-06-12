#############################################################################
##
#W    read.g               GAP 4 package Cubefree             Heiko Dietrich
##
##                                                             


#############################################################################
##
## Files containing the algorithm to construct and count cubefree groups
##
ReadPackage( CubefreePkgName, "gap/prelim.gi");
ReadPackage( CubefreePkgName, "gap/frattFree.gi");
ReadPackage( CubefreePkgName, "gap/frattExt.gi");
ReadPackage( CubefreePkgName, "gap/allCubeFree.gi");
ReadPackage( CubefreePkgName, "gap/number.gi");
ReadPackage( CubefreePkgName, "gap/isom.gi");

#############################################################################
##
## File containing the algorithm to rewrite absolutely irreducible matrix
## groups over minimal subfields 
##
ReadPackage( CubefreePkgName, "gap/glasby.gi");

#############################################################################
##
## File containing the algorithm to construct all irreducible subgroups
## of GL(2,q) up to conjugacy
##
ReadPackage( CubefreePkgName, "gap/irrGL2.gi");
