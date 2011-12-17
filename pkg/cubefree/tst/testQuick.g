LoadPackage( "cubefree" );
Print("Construct and test some small cubefree groups\n");
dirs := DirectoriesPackageLibrary( "cubefree", "tst" );
ReadTest( Filename( dirs, "exampleQuick.tst" ) );

