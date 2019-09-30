gap> START_TEST("smallgroups.tst");

# Compare with SmallGroups Library
gap> L := Filtered([1..500],IsCubeFreeInt);;
gap> for i in L do
> if not CubefreeTestOrder(i)=true then Error("sth wrong at",i); fi;
> od;

#
gap> STOP_TEST( "smallgroups.tst", 1);
