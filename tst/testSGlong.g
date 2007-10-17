LoadPackage( "cubefree" );
Print("Constructs the cubefree groups of order at most 50000 and \n");
Print("compares it with the SmallGroups Library\n");
Print("This will take quite long...\n");
Print("Please abort whenever you want.\n");
dirs := DirectoriesPackageLibrary( "cubefree", "tst" );
ReadTest( Filename( dirs, "exampleSGlong.tst" ) );

