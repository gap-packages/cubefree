#############################################################################
##
#W    read.g               share package 'Cubefree'            Heiko Dietrich
##                                                             

#############################################################################
##
## the banner
##
if not QUIET and BANNER then
    ReadPackage( "Cubefree", "gap/banner.g");
fi;

#############################################################################
##
## Files containing the algorithm to construct and count cubefree groups
##
ReadPackage( "Cubefree", "gap/diagonalMatrices.gi");
ReadPackage( "Cubefree", "gap/prelim.gi");
ReadPackage( "Cubefree", "gap/frattFree.gi");
ReadPackage( "Cubefree", "gap/frattExt.gi");
ReadPackage( "Cubefree", "gap/allCubeFree.gi");
ReadPackage( "Cubefree", "gap/number.gi");

#############################################################################
##
## Files containing the algorithm to rewrite absolutely irreducible matrix
## groups over minimal subfields
##
ReadPackage( "Cubefree", "gap/glasby.gi");

#############################################################################
##
## Files containing the algorithm to construct all irreducible subgroups
## of GL(2,q) up to conjugacy
##
ReadPackage( "Cubefree", "gap/irrGL2.gi");
