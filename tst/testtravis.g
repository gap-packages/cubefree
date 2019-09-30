LoadPackage( "cubefree" );
LoadPackage("irredsol" ); # for exampleMat.tst
dirs := DirectoriesPackageLibrary( "cubefree", "tst" );

failures := 0;
for test in [ "autoTest.tst", "exampleMat.tst", "manual.tst" ] do
  if not Test( Filename( dirs, test ) ) then
    failures := failures + 1;
  fi;
od;

# signal success / failure to Travis via exit code
if failures = 0 then
  QUIT_GAP(0);
else
  QUIT_GAP(1);
fi;
