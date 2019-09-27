LoadPackage( "cubefree" );

if not (LoadPackage("irredsol" )=true) then
    Print("There are problems loading the packing IRREDSOL.");
fi; 
 
Print("Test IrreducibleSubgroupsOfGL and compare with IrredSol\n");
Print("This may take a while...\n");
dirs := DirectoriesPackageLibrary( "cubefree", "tst" );
Test( Filename( dirs, "exampleMat.tst" ) );

