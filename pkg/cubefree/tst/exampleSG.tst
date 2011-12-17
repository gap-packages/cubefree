
gap> L := Filtered([1..50000],IsCubeFreeInt);;
gap> L := List([1..3],x->Random(L));;
gap> Print("The chosen random numbers are ",L,"\n");
gap> L := List(L,x->CubefreeTestOrder(x));;
gap> ForAll(L,x->x=true);
true






















