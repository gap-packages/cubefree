#############################################################################
##
#W  subdirect.gi           Cubefree
##
##
## Socle complements of a given order, as subdirect products of the models of
## the automorphism groups of the socle factors, up to conjugacy.
##

#############################################################################
##
#F  cf_OrbitReps( G, list ) . . orbit representatives of G on a list of subgroups
##
cf_OrbitReps := function( G, list )
    local reps, o, N;

    reps := [];
    while list <> [] do
        N := list[1];
        Add( reps, N );
        o := Orbit( G, N, OnPoints );
        if Length( o ) = 1 then
            list := list{[2..Length(list)]};
        else
            list := Filtered( list, x -> not x in o );
        fi;
    od;
    return reps;
end;

#############################################################################
##
#F  cf_GoursatData( U, primes ) . . . . . . . . . . . . Goursat data of  U
##
## Returns, up to conjugacy under Parent(U) (which normalises U), one record
##    rec( ind := index of N, hom := U -> quo, quo := U/N,
##         aut := automorphisms of quo induced by N_{Parent(U)}(N) )
## per normal subgroup N of U with U/N a {primes}-group.
##
## Cached in U; Aut(quo) is expensive and left to cf_GoursatAutData.
##
cf_GoursatData := function( U, primes )
    local key, G, fac, gens, syl, norms, res, N, hom, quo, aut, imgs, n, alpha;

    key := String( primes );
    if not IsBound( U!.ctQuot ) then U!.ctQuot := rec(); fi;
    if IsBound( U!.ctQuot.(key) ) then return U!.ctQuot.(key); fi;

    G   := Parent( U );

    # U/N has to embed into a {primes}-group, so N contains the normal closure
    # of the Sylow subgroups for all other primes
    fac := Filtered( PrimeDivisors( Size(U) ), x -> not x in primes );
    if fac = [] then
        syl := TrivialSubgroup( U );
    else
        gens := Concatenation( List( fac,
                     x -> GeneratorsOfGroup( SylowSubgroup( U, x ) ) ) );
        syl  := NormalClosure( U, SubgroupNC( U, gens ) );
    fi;
    norms := cf_OrbitReps( G, NormalSubgroupsAbove( U, syl, [] ) );

    res  := [];
    gens := GeneratorsOfGroup( U );
    for N in norms do
        hom  := NaturalHomomorphismByNormalSubgroup( U, N );
        quo  := Image( hom );
        imgs := List( gens, x -> Image( hom, x ) );
        aut  := [];
        for n in GeneratorsOfGroup( Normalizer( G, N ) ) do
            alpha := List( gens, x -> Image( hom, x^n ) );
            if alpha <> imgs then
                alpha := GroupHomomorphismByImagesNC( quo, quo, imgs, alpha );
                SetFilterObj( alpha, IsMultiplicativeElementWithInverse );
                Add( aut, alpha );
            fi;
        od;
        Add( res, rec( ind := Size(U)/Size(N), hom := hom, quo := quo,
                       aut := aut ) );
    od;

    U!.ctQuot.(key) := res;
    return res;
end;

#############################################################################
##
#F  cf_GoursatAutData( dat ) . . . . . . Aut(quo) as a permutation group, on demand
##
cf_GoursatAutData := function( dat )
    local gamma;

    if not IsBound( dat.perm ) then
        gamma      := IsomorphismPermGroup( AutomorphismGroup( dat.quo ) );
        dat.gamma  := gamma;
        dat.perm   := Image( gamma );
        dat.autgrp := SubgroupNC( dat.perm,
                                  List( dat.aut, a -> Image( gamma, a ) ) );
    fi;
end;

#############################################################################
##
#F  cf_SubdirectProds( U, V, k, last ) . . . . . . . . . . . subdirect products
##
## U <= G := Parent(U) and V <= H := Parent(V), with G normalising U and H
## normalising V. Gives the subdirect products S of U and V up to conjugacy in
## G x H: their number if last = true and |S| = k, otherwise the list of those
## with |S| dividing k, each with its parent set to its normaliser so that the
## invariant holds again next round.
##
cf_SubdirectProds := function( U, V, k, last, proj )
    local sU, sV, prod, inds, primes, quU, quV, G, H, D, e1, e2, p1, p2, res,
          cnt, du, dv, iso, autV, reps, rep, gens, r, g, h, S, gensS;

    sU   := Size( U );
    sV   := Size( V );
    prod := sU * sV;

    # a Goursat pair (N,M) of common index d gives |S| = |U|*|V|/d with d
    # dividing Gcd(|U|,|V|), which cuts the pairs down sharply
    if last then
        if prod mod k <> 0 then return 0; fi;
        inds := Filtered( [ prod/k ], d -> sU mod d = 0 and sV mod d = 0 );
    else
        inds := Filtered( DivisorsInt( Gcd(sU,sV) ),
                          d -> (d*k) mod prod = 0 );
    fi;
    if inds = [] then
        if last then return 0; else return []; fi;
    fi;

    primes := PrimeDivisors( Gcd( sU, sV ) );
    quU := Filtered( cf_GoursatData( U, primes ), x -> x.ind in inds );
    quV := Filtered( cf_GoursatData( V, primes ), x -> x.ind in inds );
    if quU = [] or quV = [] then
        if last then return 0; else return []; fi;
    fi;

    res := [];
    cnt := 0;
    if not last then
        G  := Parent( U );
        H  := Parent( V );
        D  := DirectProduct( G, H );
        e1 := Embedding( D, 1 );
        e2 := Embedding( D, 2 );
        if proj then
            p1 := Projection( D, 1 );
            p2 := Projection( D, 2 );
        fi;
    fi;

    for du in quU do
        for dv in quV do
            if dv.ind <> du.ind then continue; fi;
            iso := IsomorphismGroups( du.quo, dv.quo );
            if iso = fail then continue; fi;

            # the subdirect products for this pair of kernels correspond to
            # double cosets of the two induced automorphism groups
            cf_GoursatAutData( du );
            autV := SubgroupNC( du.perm,
                      List( dv.aut, a -> Image( du.gamma,
                            iso * a * InverseGeneralMapping( iso ) ) ) );
            reps := DoubleCosets( du.perm, du.autgrp, autV );

            if last then
                cnt := cnt + Length( reps );
            else
                for rep in reps do
                    rep  := PreImagesRepresentative( du.gamma,
                                                     Representative( rep ) );
                    gens := Concatenation(
                      List( GeneratorsOfGroup(
                             KernelOfMultiplicativeGeneralMapping( du.hom ) ),
                            x -> Image( e1, x ) ),
                      List( GeneratorsOfGroup(
                             KernelOfMultiplicativeGeneralMapping( dv.hom ) ),
                            x -> Image( e2, x ) ) );
                    for r in GeneratorsOfGroup( du.quo ) do
                        g := PreImagesRepresentative( du.hom, r );
                        h := Image( iso, Image( rep, r ) );
                        h := PreImagesRepresentative( dv.hom, h );
                        Add( gens, Image( e1, g ) * Image( e2, h ) );
                    od;
                    S := SubgroupNC( D, gens );
                    SetSize( S, prod/du.ind );
                    # rebuild S inside its normaliser, restoring the invariant
                    S := SubgroupNC( Normalizer( D, S ), gens );
                    SetSize( S, prod/du.ind );
                    # S projects onto U and onto V, so the maps into the
                    # general linear groups compose
                    if proj then
                        gensS := GeneratorsOfGroup( S );
                        SetProjections( S, Concatenation(
                          List( Projections(U), x ->
                            GroupHomomorphismByImagesNC( S, U, gensS,
                              List( gensS, y -> Image(p1,y) ) ) * x ),
                          List( Projections(V), x ->
                            GroupHomomorphismByImagesNC( S, V, gensS,
                              List( gensS, y -> Image(p2,y) ) ) * x ) ) );
                        SetSocleDimensions( S, Concatenation(
                          SocleDimensions(U), SocleDimensions(V) ) );
                    fi;
                    Add( res, S );
                od;
            fi;
        od;
    od;

    if last then return cnt; else return res; fi;
end;

#############################################################################
##
#F  cf_NrSocleComplements( gr, k ) . . . . . number of socle complements only
##
## gr is a list of lists of models; counts the subdirect products of order k
## up to conjugacy.
##
cf_NrSocleComplements := function( gr, k )
    local lGr, all, i, tmp, U, V, cnt, rest;

    gr  := List( gr, x -> Filtered( x, y -> k mod Size(y) = 0 ) );
    if ForAny( gr, IsEmpty ) then return 0; fi;
    lGr := Length( gr );
    if lGr = 1 then
        return Number( gr[1], x -> Size(x) = k );
    fi;

    # shortest lists first keeps the intermediate results small
    SortBy( gr, Length );

    all := gr[1];
    for i in [ 2 .. lGr-1 ] do
        # a partial product of order o reaches k only if k divides o times
        # what the remaining factors can contribute
        rest := Product( [i+1..lGr], j -> Lcm( List( gr[j], Size ) ) );
        tmp  := [];
        for U in all do
            for V in gr[i] do
                Append( tmp, cf_SubdirectProds( U, V, k, false, false ) );
            od;
        od;
        all := Filtered( tmp, x -> (Size(x)*rest) mod k = 0 );
    od;

    cnt := 0;
    for U in all do
        for V in gr[lGr] do
            cnt := cnt + cf_SubdirectProds( U, V, k, true, false );
        od;
    od;
    return cnt;
end;

#############################################################################
##
#F  cf_SocleComplements( gr, k ) . . . . . . . . the socle complements
##
## As cf_NrSocleComplements, but returns the groups, each with the maps into
## the general linear groups as Projections. The factors are kept in the given
## order, so that Projections and SocleDimensions match the socle.
##
cf_SocleComplements := function( gr, k )
    local lGr, all, i, tmp, U, V, rest;

    gr := List( gr, x -> Filtered( x, y -> k mod Size(y) = 0 ) );
    if ForAny( gr, IsEmpty ) then return []; fi;
    lGr := Length( gr );
    if lGr = 1 then
        return Filtered( gr[1], x -> Size(x) = k );
    fi;

    all := gr[1];
    for i in [ 2 .. lGr ] do
        rest := Product( [i+1..lGr], j -> Lcm( List( gr[j], Size ) ) );
        tmp  := [];
        for U in all do
            for V in gr[i] do
                Append( tmp, cf_SubdirectProds( U, V, k, false, true ) );
            od;
        od;
        all := Filtered( tmp, x -> (Size(x)*rest) mod k = 0 );
    od;
    return Filtered( all, x -> Size(x) = k );
end;

