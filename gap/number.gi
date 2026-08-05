#############################################################################
##
#W  number.gi           Cubefree                               Heiko Dietrich
##
##


##############################################################################
##
#F  cf_CheckNumberArgs( arg )
##
## checks the arguments of NumberCFSolvableGroups and NumberCFGroups and
## returns [size, smallGrp]
##
cf_CheckNumberArgs := function( arg1 )
    local size, smallGrp;

    if Size( arg1 ) = 1 then
        size     := arg1[1];
        smallGrp := true;
    elif Size( arg1 ) = 2 then
        size     := arg1[1];
        smallGrp := arg1[2];
    else
        Error("Wrong input format: Either arg='size' or arg='size,bool'.");
    fi;
    if not IsBool( smallGrp ) then
        Error("Second argument has to be Boolean.");
    fi;
    if not (IsInt( size ) and size>0) then
        Error("First argument has to be a positive integer.");
    elif not IsCubeFreeInt( size ) then
        Error("First argument has to be a cube-free integer.");
    fi;
    return [ size, smallGrp ];
end;


##############################################################################
##
#F  NumberCFSolvableGroups( arg )
##
## Counts the cubefree solvable groups of order 'size' via the one-to-one
## correspondence with the solvable Frattini-free groups. The SmallGroups
## library is only consulted for squarefree orders and orders p^2 and p^2q;
## the boolean argument is accepted for compatibility but has no effect.
##
InstallGlobalFunction(NumberCFSolvableGroups, function( arg )
    local tmp, size, smallGrp, cl;

    tmp      := cf_CheckNumberArgs( arg );
    size     := tmp[1];
    smallGrp := tmp[2];

    Info(InfoCF,1," Count number of solvable groups of order ",size,".");

    if size = 1 then
        return 1;
    fi;

    # groups of squarefree order and of order p^2, p^2q are solvable, so the
    # library answers without constructing anything
    cl := Collected(FactorsInt(size));
    if cf_canUseSG(cl) then
        return NumberSmallGroups(size);
    fi;

    # for the rest, counting beats searching the library, which does not store
    # the number of solvable groups; so smallGrp has no effect here
    return cf_NrCFSolvableGroups( size );
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
    local tmp, size, smallGrp, cl, nonAb;

    tmp      := cf_CheckNumberArgs( arg );
    size     := tmp[1];
    smallGrp := tmp[2];

    Info(InfoCF,1,"Count number of groups of order ",size,".");

    if size = 1 then
        return 1;
    fi;

    cl := Collected(FactorsInt(size));
    if cf_canUseSG(cl) or (smallGrp and size < 50001) then
        return NumberSmallGroups(size);
    fi;

    # every cubefree group is a direct product of a solvable group and a
    # (possibly trivial) simple group PSL(2,p)
    cl    := List(cl,x->x[1]);
    cl    := Filtered(cl, x-> IsCubeFreeInt(x-1) and IsCubeFreeInt(x+1)
                             and x>3);
    nonAb := List( cl, x-> x*(x-1)*(x+1)/2);
    nonAb := Filtered(nonAb, x-> size mod x=0);
    nonAb := Concatenation([1],nonAb);

    return Sum( nonAb, A -> cf_NrCFSolvableGroups( size/A ) );
end);
