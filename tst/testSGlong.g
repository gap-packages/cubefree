LoadPackage( "cubefree" );
Print("Constructs the cubefree groups of order at most 50000 and \n");
Print("compares it with the SmallGroups Library\n");
Print("This will take quite long...\n");
Print("Please abort whenever you want.\n");

L := Filtered([1..50000],IsCubeFreeInt);;
for i in [1..Length(L)] do
  n := L[i];
  if not CubefreeTestOrder(n)=true then Error("sth wrong for order",n); fi;
  Print("\r"); # return to start of line
  Print("order ", n, " is ok (", i, "/", Length(L), ")");
  Print("\c"); # flush output
od;
Print("\n");
