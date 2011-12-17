LoadPackage( "cubefree" );
Print("Constructs the solvable groups of a random cubefree number and \n");
Print("calls RandomIsomorphismTest..\n");
Print("This may take a while...\n");
dirs := DirectoriesPackageLibrary( "cubefree", "tst" );
ReadTest( Filename( dirs, "exampleBig.tst" ) );

