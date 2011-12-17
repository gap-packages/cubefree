
gap> L := Filtered([1..2000],IsCubeFreeInt);;
gap> L := List([1..3],x->Random(L));;
gap> Print("#I The following orders are tested: ",L,"\n");
gap> L := List(L, x-> CubefreeTestOrder(x));;
gap> ForAll(L,x->x=true);
true




















