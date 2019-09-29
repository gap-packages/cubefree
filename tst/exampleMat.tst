gap> START_TEST("Test IrreducibleSubgroupsOfGL (solvable)"); 
gap> for p in Filtered(Primes,x-> x>3 and x<20) do
> ind := AllIrreducibleSolvableMatrixGroups(Degree,2,FieldOfMatrixGroup,GF(p));
> ind := List(ind,IdIrreducibleSolvableMatrixGroup);
> new := Filtered(IrreducibleSubgroupsOfGL(2,p),IsSolvable);;
> new := List(new, IdIrreducibleSolvableMatrixGroup);
> if not IsDuplicateFreeList(new) or not Difference(new,ind)= [] or 
> not Difference(ind,new)=[] then
> Error("Something wrong at ",p);
> fi;
> od;

# Primes between 5 and 19 are ok. Now consider 25 and 49
gap> ind := AllIrreducibleSolvableMatrixGroups(Degree,2,FieldOfMatrixGroup,GF(25));;
gap> ind := List(ind,IdIrreducibleSolvableMatrixGroup);; 
gap> new := Filtered(IrreducibleSubgroupsOfGL(2,25),IsSolvable);;
gap> new := Filtered(new, x->TraceField(x)=GF(25));;
gap> new := List(new,IdIrreducibleSolvableMatrixGroup);;
gap> IsDuplicateFreeList(new);
true
gap> Difference(new,ind)= [] and Difference(ind,new)=[];
true
gap> ind := AllIrreducibleSolvableMatrixGroups(Degree,2,FieldOfMatrixGroup,GF(49));;
gap> ind := Collected(List(ind,Size));;
gap> new := Filtered(IrreducibleSubgroupsOfGL(2,49),IsSolvable);;
gap> new := Filtered(new, x->TraceField(x)=GF(49));;
gap> new := Collected(List(new,Size));;
gap> Difference(new,ind)= [] and Difference(ind,new)=[];
true
gap> STOP_TEST( "exampleMat.tst", 100000);   
