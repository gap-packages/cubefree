#############################################################################
##
#W  frattExt.gi           Cubefree                             Heiko Dietrich
##                                                               
##


##
## These functions compute the  Frattini extensions. 
## Basically, already implemented methods of the GAP Package GrpConst are
## used.
##

############################################################################# 
## 
#F FrattiniExtensionCF( code, o ) 
## 
## Computes the Frattini extensions of the group given by 'code' of order 'o'
InstallGlobalFunction(FrattiniExtensionCF, function( code, o ) 
    local F, rest, primes, modus, H, i, modul, found, j, M, cc, c; 
 
    # get F and the trivial case 
    F      := PcGroupCodeRec( code ); 
    rest   := o / code.order; 
    if rest = 1 then return F; fi; 
 
    # construct irreducible modules for F 
    primes := Factors( rest ); 
    modus  := List( primes, x -> IrreducibleModules( F, GF(x), 1 )[2] ); 
    FindUniqueModules( modus ); 
 
    # set up 
    H := PcGroupCodeRec( code ); 
 
    # loop over primes 
    for i in [1..Length(primes)] do 
        modul := List( modus[i], x -> EnlargedModule( x, F, H ) ); 
        found := false; 
        j := 0; 
        while not found do 
            j := j+1; 
            M := modul[j]; 
            cc := TwoCohomology( H, M ); 
            if Dimension( Image( cc.cohom ) ) > 0 then 
                c := PreImagesRepresentative( cc.cohom, 
                                      Basis(Image(cc.cohom))[1]); 
                H := ExtensionSQ( cc.collector, H, M, c ); 
                found := true; 
            fi; 
        od; 
    od; 
    return H; 
end); 
 
############################################################################# 
## 
#F ConstructAllCFGroups( size ) 
## 
## Computes all cube-free groups of order n up to isomorphism
##
InstallGlobalFunction(ConstructAllCFGroups, function ( size ) 
    local cl, free, ext, t, primes, ffOrd, lv, nonAb, p, A, nSize, facNSize,
          groups, arg1, arg2, pos, autPos, autGrps, tmp; 
 
    Info(InfoCF,1,"Construct all groups of order ",size,".");   

    # check
    if not IsPosInt( size ) or not IsCubeFreeInt( size ) then
        Error("Argument has to be a positive cube-free integer."); 
    fi;

    # catch the case of size = 1 
    if size = 1 then 
        return [TrivialGroup()]; 
    fi; 
  
    # if size is square-free then the groups of order size
    # are Frattini-free and solvable
    if IsSquareFreeInt(size) then
        return(List(cf_FrattFreeSolvGroups(size,0,0,0)[1] ,
                     x->PcGroupCodeRec(x)));
    fi;

    # set up
    groups := [];
    cl     := Collected( Factors( size ) ); 
    # to store the subgroups and normalizers of GL(2,p)
    autPos := [];
    for t in cl do
        Add(autPos,t[1]);
        if t[2]=2 then
            Add(autPos,t[1]^2);
        fi;
    od;
    autGrps := ListWithIdenticalEntries( Length( autPos ), 0 );
    pos     := function(x) return Position( autPos, x); end;

    # determine the possible non-abelian factors PSL(2,p)
    nonAb:=[TrivialGroup()];
    if size mod 4 = 0 then
        for p in cl do
            arg1 := (p[1]>3) and (size mod (p[1]*(p[1]-1)*(p[1]+1) / 2)=0);
            arg2 := IsCubeFreeInt(p[1]+1) and IsCubeFreeInt(p[1]-1);
            if arg1 and arg2 then
                A := PSL(2,p[1]);
                if Size( A )=size then
                    Add(groups,A );
                else
                    Add(nonAb,A);
                fi;
            fi;
        od;
    fi;

    # for every non-abelian A compute a solvable complement
    for A in nonAb do
        nSize    := size/Size(A);
        facNSize := Collected(FactorsInt(nSize));    
 
        # determine the possible Frattini-factors
        primes := Product(List(facNSize,x->x[1]));
        ffOrd  := Filtered(DivisorsInt(nSize),x-> x mod primes =0);
        free   := [];
        for lv in ffOrd do
            tmp     := cf_FrattFreeSolvGroups(lv,autPos, autGrps, pos);
            autGrps := tmp[2];
            free    := Concatenation(free,tmp[1]);
        od;
        Info(InfoCF,1,"Construct ",Length(free)," Frattini extensions.");
        ext    := List(free,x -> FrattiniExtensionCF(x,nSize)); 
        groups := Concatenation(groups,List(ext,x->DirectProduct(A,x)));
    od;

    return groups; 
end ); 
 
############################################################################# 
## 
#F ConstructAllCFSolvableGroups( size ) 
## 
## Computes all cube-free solvable groups of order n up to isomorphism
##
InstallGlobalFunction(ConstructAllCFSolvableGroups, function ( size ) 
    local cl, free, ext, t, primes, ffOrd, lv, p, groups, pos, autGrps,
          autPos, tmp; 

    # check
    if not IsPosInt( size ) or not IsCubeFreeInt( size ) then
        Error("Argument has to be a positive cube-free integer."); 
    fi;

    Info(InfoCF,1,"Construct all solvable groups of order ",size,".");

    # catch the case of size = 1 
    if size = 1 then 
        return [TrivialGroup()]; 
    fi; 
  
    # if size is square-free, then the groups of order size
    # are Frattini-free and solvable
    if IsSquareFreeInt(size) then
        return(List(cf_FrattFreeSolvGroups(size,0,0,0)[1] ,
                     x->PcGroupCodeRec(x)));
    fi;

    # set up
    groups := [];
    cl     := Collected( Factors( size ) );  
    # to store the subgroups and normalizers of GL(2,p)
    autPos := [];
    for t in cl do
        Add(autPos,t[1]);
        if t[2]=2 then
            Add(autPos,t[1]^2);
        fi;
    od;
    autGrps := ListWithIdenticalEntries( Length( autPos ), 0 );
    pos     := function(x) return Position( autPos, x); end;
 
    # determine the possible Frattini-factors
    primes := Product(List(cl,x->x[1]));
    ffOrd  := Filtered(DivisorsInt(size),x-> x mod primes =0);
    free   := [];
    for lv in ffOrd do
        tmp     := cf_FrattFreeSolvGroups(lv,autPos, autGrps, pos);
        autGrps := tmp[2];
        free    := Concatenation(free,tmp[1]);
    od;
    Info(InfoCF,1,"Construct ",Length(free)," Frattini extensions.");
    ext    := List(free,x -> FrattiniExtensionCF(x,size)); 
    groups := Concatenation(groups,ext);
    
    return groups; 
end );  




##############################################################################
##
#F  CubefreeTestOrder( n )
##
## Computes information about the groups of order n and compares it with the 
## SmallGroups library
##
InstallGlobalFunction(CubefreeTestOrder, function( n )
local nr, nrs, ff, solv, nil, sim, all, groups, alltmp;

    if not IsCubeFreeInt(n) and n<50001 and IsPosInt(n) then
        Error("wrong input: need a cubefree integer between 1..50000");
    fi;

    all := AllSmallGroups(n);

    Info(InfoCF,1,"Count groups");
    nr := NumberCFGroups(n,false);
    if not nr = NumberSmallGroups(n) then Error("wrong number of groups"); fi;

    Info(InfoCF,1,"Count solvable groups");
    nrs := NumberCFSolvableGroups(n,false);
    if not nrs = Length(Filtered(all,IsSolvableGroup)) then
         Error("wrong number of solvable groups");
    fi;

    Info(InfoCF,1,"Construct simple groups");
    groups := List(ConstructAllCFSimpleGroups(n),IdSmallGroup);
    alltmp := List(Filtered(all,IsSimpleGroup),IdSmallGroup);
    if not Difference(groups,alltmp)=Difference(alltmp,groups) then
        Error("wrong result (simple groups)");
    fi;

    Info(InfoCF,1,"Construct nilpotent groups");
    groups := List(ConstructAllCFNilpotentGroups(n),IdSmallGroup);
    alltmp := List(Filtered(all,IsNilpotentGroup),IdSmallGroup);
    if not Difference(groups,alltmp)=Difference(alltmp,groups) then
        Error("wrong result (nilpotent groups)");
    fi;
    
    Info(InfoCF,1,"Construct all groups");
    groups := List(ConstructAllCFGroups(n),IdSmallGroup);
    alltmp := List(all,IdSmallGroup);
    if not Difference(groups,alltmp)=Difference(alltmp,groups) then
        Error("wrong result (nilpotent groups)");
    fi;

    Info(InfoCF,1,"Construct Fratt.free groups");
    groups := List(ConstructAllCFFrattiniFreeGroups(n),IdSmallGroup);
    alltmp := List(Filtered(all,x->FrattinifactorSize(x)=n),IdSmallGroup);
    if not Difference(groups,alltmp)=Difference(alltmp,groups) then
        Error("wrong result (nilpotent groups)");
    fi;

    Info(InfoCF,1,"Everything ok");
    return true;

end);