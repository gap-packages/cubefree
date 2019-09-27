LoadPackage( "cubefree" );
dirs := DirectoriesPackageLibrary( "cubefree", "tst" );

result := Test( Filename( dirs, "autoTest.tst" ) );

# signal success / failure to Travis via exit code
if result then
  QUIT_GAP(0);
else
  QUIT_GAP(1);
fi;
