LoadPackage( "cubefree" );
Print("Test 3 random orders and compare with SmallGroups Library\n");
Print("This may take a while...\n");
dirs := DirectoriesPackageLibrary( "cubefree", "tst" );
ReadTest( Filename( dirs, "exampleSG.tst" ) );

