gap> START_TEST("Compare with SmallGroups Library:");  
gap> L := Filtered([1..1000],IsCubeFreeInt);;
gap> for i in L do
> if not CubefreeTestOrder(i)=true then Error("sth wrong at",i); fi;
> if i mod 50=0 then
> Print("Up to order ",i," everything is ok\n");
> fi;
> od;
gap> STOP_TEST( "exampleSGlong.tst", 100000);   





















