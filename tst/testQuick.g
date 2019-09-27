LoadPackage( "cubefree" );
Print("Construct and test some small cubefree groups\n");
dirs := DirectoriesPackageLibrary( "cubefree", "tst" );
Test( Filename( dirs, "exampleQuick.tst" ) );

