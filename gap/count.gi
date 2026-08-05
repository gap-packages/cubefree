#############################################################################
##
#W  count.gi           Cubefree
##
##
## Counting cubefree groups, for NumberCFGroups and NumberCFSolvableGroups.
##
## No group is ever constructed: ExtensionBySocle returns one Frattini-free
## group per socle complement, so counting socle complements suffices.
##

#############################################################################
##
##  n -> number of solvable Frattini-free groups of order n. Shared across
##  calls, which is what makes counting many orders in a row cheap.
##
cf_NrFFCache := rec();

cf_ClearCountCaches := function()
    cf_NrFFCache := rec();
    cf_ClearAutGroupCaches();
end;

#############################################################################
##
#F  cf_NrFrattFreeSolvGroups( n ) . . number of solvable Frattini-free groups
##
## Counting counterpart of cf_FrattFreeSolvGroups.
##
cf_NrFrattFreeSolvGroups := function( n )
    local key, SocOrders, temp, s, ord, lv, possible, number, tempAutGr, k;

    if n = 1 then return 1; fi;

    key := String( n );
    if IsBound( cf_NrFFCache.(key) ) then return cf_NrFFCache.(key); fi;

    Info(InfoCF,1,"  Count solvable Frattini-free groups of order ",n,".");

    # all socle orders s such that n/s divides |Aut(socle)|
    SocOrders := Filtered( DivisorsInt( n ), x -> x > 1 );
    temp      := [];
    for s in SocOrders do
        ord := 1;
        for lv in Collected( FactorsInt( s ) ) do
            if lv[2] = 1 then
                ord := ord * (lv[1]-1);
            else
                ord := ord * (lv[1]*(lv[1]-1)^2*(lv[1]+1));
            fi;
        od;
        if ord mod (n/s) = 0 then Add( temp, s ); fi;
    od;
    SocOrders := temp;

    # if S is a socle of order s and p divides n/s, then p has to divide the
    # order of a projection of a direct factor of the socle complement
    temp := [];
    for s in SocOrders do
        possible := true;
        ord      := 2;
        for lv in Collected( FactorsInt( s ) ) do
            if lv[2] = 1 then
                ord := ord * (lv[1]-1);
            else
                ord := ord * (lv[1]^2-1);
            fi;
        od;
        for lv in Collected( FactorsInt( n/s ) ) do
            if not ord mod lv[1] = 0 then possible := false; fi;
        od;
        if possible then Add( temp, s ); fi;
    od;
    SocOrders := temp;

    number := 0;
    for s in SocOrders do
        if s = n then
            # socle = whole group: the abelian one
            number := number + 1;
        else
            k         := n/s;
            tempAutGr := [];
            for lv in Collected( FactorsInt( s ) ) do
                if lv[2] = 1 then
                    Add( tempAutGr, cf_AutGroupsC( lv[1], k ) );
                else
                    Add( tempAutGr, cf_AutGroupsGL2( lv[1], k ) );
                fi;
            od;
            Info(InfoCF,2,"    Count socle complements of order ",k,".");
            number := number + cf_NrSocleComplements( tempAutGr, k );
        fi;
    od;

    cf_NrFFCache.(key) := number;
    return number;
end;

#############################################################################
##
#F  cf_NrCFSolvableGroups( n ) . . . . . number of solvable groups of order n
##
## One-to-one with the solvable Frattini-free groups of order F, where F runs
## over the divisors of n divisible by the radical of n.
##
cf_NrCFSolvableGroups := function( n )
    local pr;

    if n = 1 then return 1; fi;
    pr := Product( Set( FactorsInt( n ) ) );
    return Sum( Filtered( DivisorsInt( n ), x -> x mod pr = 0 ),
                cf_NrFrattFreeSolvGroups );
end;
