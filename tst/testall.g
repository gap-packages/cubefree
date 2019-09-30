LoadPackage( "cubefree" );
LoadPackage("irredsol" ); # for exampleMat.tst
dirs := DirectoriesPackageLibrary( "cubefree", "tst" );
TestDirectory( dirs, rec(exitGAP := true ) );
FORCE_QUIT_GAP(1);
