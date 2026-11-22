#############################################################################
##
#W    init.g            GAP 4 package 'cubefree'               Heiko Dietrich
#W                                                             
##


#############################################################################
##
## Put the name of the package into a single variable.  This makes it
## easer to change it to something else if necessary.
##
CubefreePkgName := "cubefree";

############################################################################
##
#I InfoClass
##
DeclareInfoClass( "InfoCF" );

#############################################################################
##
#D Read .gd files
##
ReadPackage(CubefreePkgName,"gap/prelim.gd");      #preliminary functions
ReadPackage(CubefreePkgName,"gap/frattExt.gd");    #construct frat. ext.
ReadPackage(CubefreePkgName,"gap/frattFree.gd");   #construct frat. free gr.
ReadPackage(CubefreePkgName,"gap/glasby.gd");      #matrix rewriting alg.
ReadPackage(CubefreePkgName,"gap/irrGL2.gd");      #irred. subg. of gl(2,p)
ReadPackage(CubefreePkgName,"gap/number.gd");      #count cf groups
ReadPackage(CubefreePkgName,"gap/allCubeFree.gd"); #count cf groups up to
ReadPackage(CubefreePkgName,"gap/isom.gd");        #isom test




