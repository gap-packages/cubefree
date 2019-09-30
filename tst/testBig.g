LoadPackage( "cubefree" );
Print("Constructs the solvable groups of a random cubefree number and \n");
Print("calls RandomIsomorphismTest..\n");
Print("This may take a while...\n");

L := [1..2^28-1];;
repeat 
  n:=Random(L); 
until IsCubeFreeInt(n) and not IsSquareFreeInt(n) and
      Maximum(FactorsInt(n))<200;;
Print("The chosen order is ",n,"\n");
Print("with factorisation ",Collected(FactorsInt(n)),"\n");
L  := ConstructAllCFSolvableGroups(n);;
L  := List(L, x->rec(order:=n, code:=CodePcGroup(x)));;
L2 := RandomIsomorphismTest(L,5);;
if Length(L) = Length(L2) then
   Print("success\n");
else
   Print("failure\n");
fi;
