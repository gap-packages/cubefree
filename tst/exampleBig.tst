
gap> L := [1..2^28-1];;
gap> repeat 
> n:=Random(L); 
> until IsCubeFreeInt(n) and not IsSquareFreeInt(n) and
> Maximum(List(Collected(FactorsInt(n)),x->x[1]))<200;;
gap> Print("The chosen order is ",n,"\n");
gap> Print("with factorisation ",Collected(FactorsInt(n)),"\n");
gap> L  := ConstructAllCFSolvableGroups(n);;
gap> L  := List(L, x->rec(order:=n, code:=CodePcGroup(x)));;
gap> L2 := RandomIsomorphismTest(L,5);;
gap> Length(L)=Length(L2);
true






















