LoadPackage( "cubefree" );
Print("Test 3 random orders and compare with SmallGroups Library\n");
Print("This may take a while...\n");

L := Filtered([1..50000],IsCubeFreeInt);;
L := List([1..3],x->Random(L));;
Print("The chosen random numbers are ",L,"\n");
L := List(L,x->CubefreeTestOrder(x));;
if ForAll(L,x->x=true) then
   Print("success\n");
else
   Print("failure\n");
fi;
