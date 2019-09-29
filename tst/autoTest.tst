gap> START_TEST("autoTest.tst");

# Compare with SmallGroups Library
gap> L := Filtered([1..1000],IsCubeFreeInt);;
gap> for i in L do
> if not CubefreeTestOrder(i)=true then Error("sth wrong at",i); fi;
> od;

#
gap> testConstructAllCFSolvableGroups:= function(n)
>   local L, L2;
>   L  := ConstructAllCFSolvableGroups(n);;
>   L  := List(L, x->rec(order:=n, code:=CodePcGroup(x)));;
>   L2 := RandomIsomorphismTest(L,5);;
>   return L = L2;
> end;;
gap> Assert(0, testConstructAllCFSolvableGroups(1300));
gap> Assert(0, testConstructAllCFSolvableGroups(15345));
gap> Assert(0, testConstructAllCFSolvableGroups(23079686));

#
gap> STOP_TEST( "autoTest.tst", 1);
