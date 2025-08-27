#############################################################################
##
#W  number.gi           Cubefree                               Heiko Dietrich
##                                                              
##


##############################################################################
##
#F  NumberCFSolvableGroups( arg )
##
## Counts the number of all cubefree solvable groups using the one-to-one
## correspondence. If the argument is [size,false] then the SmallGrps
## library is not used for non-squarefree but cubefree orders. 
## If the argument is 'size' or [size,true] then it will be used.
##
InstallGlobalFunction(NumberCFSolvableGroups, function( arg ) 
    local smallGrp, size, number, cl, i,j, FOrders, F, autPos, pos, autGrps,
           tmp, t;

    # check
    if Size(arg)=1 then
        size     := arg[1];
        smallGrp := true;
    elif Size(arg) in [2,5] then
        size     := arg[1];
        smallGrp := arg[2];
    else
        Error("Wrong input format: Either arg='size' or arg='size,bool'.");
    fi;
    if not IsBool(smallGrp) then
        Error("Second argument has to be Boolean.");
    fi;
    if not (IsInt( size ) and size>0) then
        Error("First argument has to be a positive  integer.");
    elif not IsCubeFreeInt( size ) then
        Error("First argument has to be a cube-free integer."); 
    fi;

    Info(InfoCF,1," Count number of solvable groups of order ",size,".");

    if size = 1 then
        return 1;
    fi;

    cl := Collected(FactorsInt(size));
    # to store the subgroups and normalizers of GL(2,p)
    # if Length(arg)<5 then the function was called from NumberCFGroups
    if Length(arg)<5 then
        autPos := [];
        for t in cl do
            Add(autPos,t[1]);
            if t[2]=2 then
                Add(autPos,t[1]^2);
            fi;
        od;
        autGrps := ListWithIdenticalEntries( Length( autPos ), 0 );
        pos     := function(x) return Position( autPos, x); end;
    else
        autPos  := arg[3];
        autGrps := arg[4];
        pos     := arg[5];
    fi;

    # Squarefree groups are solvable;
    # groups of order p^2q an p^2 are solvable as well.
    if cf_canUseSG(cl) then
        return NumberSmallGroups(size);
    fi;
    if smallGrp and size<50001 then
        i := 0;
        for F in [1..NumberSmallGroups(size)] do
            if IsSolvableGroup(SmallGroup(size,F)) then i := i+1; fi;
        od; 
        return i;
    fi;

    cl := Product(List(cl,x->x[1]));
   
    # Count all cube-free Frattini-free solvable groups F with
    # cl | |F| | size
    FOrders := Filtered(DivisorsInt(size),x-> x mod cl =0);
    number  := 0;
    for F in FOrders do
        if cf_canUseSG(F) or (smallGrp and F<50001) then
            if IsOddInt(F) then
                i := Length(Filtered([1..NumberSmallGroups(F)], x->
                      FrattinifactorSize(SmallGroup(F,x))= F));
            else
                i := Length(Filtered([1..NumberSmallGroups(F)], x->
                      FrattinifactorSize(SmallGroup(F,x))= F and
                      IsSolvable(SmallGroup(F,x)) ));
            fi;
            number := number + i;
        else
            tmp     := cf_FrattFreeSolvGroups(F,autPos, autGrps, pos);
            autGrps := tmp[2];
            number  := number + Length( tmp[1] );
        fi;;
    od;
    
    if Length(arg)<5 then
        return number;
    else
        return [number,autGrps];
    fi;
end);




##############################################################################
##
#F  NumberCFGroups( size )
##
## Counts all groups of cube-free order n. If the argument is [size,false]
## then the SmallGrps library is not used. If the argument is 'size' or 
## [size,true] then the SmallGroups library will be used.
##
InstallGlobalFunction(NumberCFGroups, function( arg ) 
    local nonAb, solvff, number, i, A, l, p, G, cl, FOrders, F, Fcl, psl, I,
          size, smallGrp,test, autPos,autGrps, pos, tmp, t;

    # check
    if Size(arg)=1 then 
        size     := arg[1];
        smallGrp := true;
    elif Size(arg)=2 then
        size     := arg[1];
        smallGrp := arg[2];
    else
        Error("Wrong input format: Either arg='size' or arg='size,bool'.");
    fi;
    if not IsBool(smallGrp) then
        Error("Second argument has to be Boolean.");
    fi;
    if not (IsInt( size ) and size>0) then
        Error("First argument has to be a positive integer.");
    elif not IsCubeFreeInt( size ) then
        Error("First argument has to be a cube-free integer."); 
    fi;
  
    Info(InfoCF,1,"Count number of groups of order ",size,".");

    if size = 1 then
        return 1;
    fi;

    cl := Collected(FactorsInt(size));
    autPos := [];
    for t in cl do
        Add(autPos,t[1]);
        if t[2]=2 then
            Add(autPos,t[1]^2);
        fi;
    od;
    autGrps := ListWithIdenticalEntries( Length( autPos ), 0 );
    pos     := function(x) return Position( autPos, x); end;

    if (size <50001 and smallGrp) or cf_canUseSG(cl) then
        return NumberSmallGroups(size);
    fi;

    # determine possible non-abelian factors
    cl     := List(cl,x->x[1]);
    cl     := Filtered(cl, x-> IsCubeFreeInt(x-1) and IsCubeFreeInt(x+1)
                                  and x>3);
    nonAb  := List( cl, x-> x*(x-1)*(x+1)/2);
    nonAb  := Filtered(nonAb, x-> size mod x=0);
    nonAb  := Concatenation([1],nonAb);
    number := 0;
   
    for A in nonAb do
        tmp     := NumberCFSolvableGroups(size/A, smallGrp,autPos,autGrps,pos);
        if IsInt(tmp) then
            number := number + tmp;
        else
            autGrps := tmp[2];
            number  := number + tmp[1];
       fi;
    od;

   return(number);
end);
