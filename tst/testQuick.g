LoadPackage( "cubefree" );
Print("Construct and test some small cubefree groups\n");

L := Filtered([1..2000],IsCubeFreeInt);;
L := List([1..3],x->Random(L));;
Print("#I The following orders are tested: ",L,"\n");
L := List(L, x-> CubefreeTestOrder(x));;
if ForAll(L,x->x=true) then
   Print("success\n");
else
   Print("failure\n");
fi;
