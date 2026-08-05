#############################################################################
##
#W  allCubeFree.gi                Cubefree                    Heiko Dietrich
##                                                             
##

##
## The implementation of the code  below can be improved; actually, it
## was implemented only for experimental purposes.
##

############################################################################# 
## 
#F  CountAllCFGroupsUpTo( arg ) 
## 
InstallGlobalFunction(CountAllCFGroupsUpTo, function( arg ) 
    local bound, smallGrp, numbers, size; 

    # check
    if Size(arg)=1 then 
        bound    := arg[1];
        smallGrp := true;
    elif Size(arg)=2 then
        bound    := arg[1];
        smallGrp := arg[2];
    else
        Error("Wrong input format: Either arg='size' or arg='size,bool'.");
    fi;
    if not IsPosInt(bound) then
        Error("First argument has to be a positiv integer.");
    fi;
    if not IsBool(smallGrp) then
        Error("Second argument has to be Boolean.");
    fi;

    Info(InfoCF,1,"Count all cubefree groups up to ",bound,".");

    # NumberCFGroups caches the subgroups of the GL(2,p) it needs as well as
    # the number of solvable Frattini-free groups of every order it meets, so
    # a plain loop already shares all the work between the orders.
    numbers := [1];
    for size in Filtered([2..bound],IsCubeFreeInt) do    
        Info(InfoCF,2,"    Count groups of order ",size,".");        
        numbers[size] := NumberCFGroups( size, smallGrp );
    od;

    return numbers; 
end ); 
