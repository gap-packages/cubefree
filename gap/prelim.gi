
#############################################################################
##
#W  prelim.gi           Cubefree                               Heiko Dietrich
#W                                                              
##
#############################################################################


##############################################################################
##
#F IsCubeFreeInt( n )
##
## Checks if the integer n is cube-free
##
InstallGlobalFunction(IsCubeFreeInt, function( n ) 
    local list;

    if not IsInt(n) then
        return( false );
    else
        list := Collected(FactorsInt(n));
        return(ForAll(list,x->x[2]<3));
    fi;
end);

##############################################################################
##
#F IsSquareFreeInt( n )
##
## Checks if the integer n is square-free
##
InstallGlobalFunction(IsSquareFreeInt, function( n ) 
    local list;

    if not IsInt(n) then
        return( false );
    else
        list := Collected(FactorsInt(n));
        return(ForAll(list,x->x[2]<2));
    fi; 
end );

############################################################################# 
## 
#F ConstructAllCFSimpleGroups( n ) 
## 
## Computes all cube-free simple groups of order n up to isomorphism
##
InstallGlobalFunction(ConstructAllCFSimpleGroups, function ( size ) 
    local cl, p, A; 
 
    # check
    if not (IsInt( size ) and size>0) then
        Error("Argument has to be a positive cube-free integer.\n");
    elif not IsCubeFreeInt( size ) then
        Error("Argument has to be a positive cube-free integer.\n"); 
    fi;

    if size = 1 then 
        return []; 
    elif IsPrime( size) then
        return [CyclicGroup(size)];
    fi;

    cl := List(Collected( Factors( size ) ), x->x[1]);
    cl := Filtered(cl, x-> IsCubeFreeInt(x+1) and IsCubeFreeInt(x-1) and
                           x>3);
    for p in cl do
        if size = (p*(p-1)*(p+1) / 2) then
            return [PSL(2,p)];
        fi;
    od;

    return []; 
end ); 
 
############################################################################# 
## 
#F ConstructAllCFNilpotentGroups( n ) 
## 
## Computes all cube-free nilpotent groups of order n up to isomorphism
##
InstallGlobalFunction(ConstructAllCFNilpotentGroups, function ( size ) 
    local cl, p, A,arg1, arg2, G, temp, C, groups; 
 
    # check
    if not (IsInt( size ) and size>0) then
        Error("Argument has to be a positive cube-free integer.\n");
    elif not IsCubeFreeInt( size ) then
        Error("Argument has to be a positive cube-free integer.\n"); 
    fi;

    if size = 1 then 
        return [TrivialGroup()]; 
    fi;

    cl     := Collected(FactorsInt( size ));
    groups := [[]];
    
    for p in cl do
        temp := [];
        for G in groups do
            if p[2] = 1 then
                Add(temp, Concatenation(G,[p[1]]));
            else
                Add(temp, Concatenation(G,[p[1]^2]));
                Add(temp, Concatenation(G,[p[1],p[1]]));
            fi;
        od;
        groups := ShallowCopy( temp );
    od;

    return List(groups,x->AbelianGroup(x)); 
end );


