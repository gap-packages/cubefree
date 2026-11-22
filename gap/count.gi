#############################################################################
##
#W  count.gi           Cubefree
##
##
## Counting cubefree groups: NumberCFGroups and NumberCFSolvableGroups.
##
## The construction code (frattFree.gi) keeps the cube-free subgroups of
## GL(2,p) in a permutation representation of degree p^2-1, up to 196248
## points, and SubdirectProducts forms direct products of two of those --
## although the subgroups that matter have order at most a few hundred.
##
## A socle complement is determined up to conjugacy by the isomorphism type of
## a subgroup U <= GL(d,p) plus the automorphisms of U induced by
## N_{GL(d,p)}(U), conjugation being the only way the ambient group enters. So
## each U is replaced by a model: a small permutation group isomorphic to U
## whose parent induces exactly those automorphisms.
##
## No extension is built either: ExtensionBySocle returns one group per socle
## complement, so counting subdirect products suffices.
##
## The models carry the isomorphism back into GL(d,p) as Projections, so
## cf_FrattFreeSolvGroups uses them for the construction as well.
##

#############################################################################
##
##  Caches, shared across calls; cf_ClearCountCaches frees them.
##
cf_ctMats   := rec();   # p -> the cube-free subgroups of GL(2,p)
cf_ctModels := rec();   # p -> their models, bound only where already computed
cf_ctCycMats:= rec();   # p -> the cube-free subgroups of GL(1,p)
cf_ctCyc    := rec();   # p -> their models, bound only where already computed
cf_ctNrFF   := rec();   # n -> number of solvable Frattini-free groups of order n

cf_ClearCountCaches := function()
    cf_ctMats   := rec();
    cf_ctModels := rec();
    cf_ctCycMats:= rec();
    cf_ctCyc    := rec();
    cf_ctNrFF   := rec();
end;

#############################################################################
##
#F  cf_SmallModel( U ) . . . . . . . . . small model of a subgroup of GL(d,p)
##
## U <= GL(d,p) with p not dividing |U| and d in {1,2}. Returns M isomorphic
## to U, normal in W := Parent(M), such that W induces on M exactly the
## automorphisms N_{GL(d,p)}(U) induces on U. The isomorphism back to U is
## stored as Projections(M), which is what ExtensionBySocle needs.
##
## The induced automorphisms are found without touching GL(d,p): the natural
## module is semisimple and p > d, so by Brauer-Nesbitt two d-dimensional
## GF(p)-representations of U agree iff their trace functions do; hence an
## automorphism is induced by conjugation in GL(d,p) iff it preserves traces.
##
cf_SmallModel := function( Umat )
    local d, gl, iso, U, gens, mats, finish, elms, tr, A, hom, stab, v, poss,
          reg, W, M, small;

    d   := Length( One( Umat ) );
    gl  := GL( d, Characteristic( One(Umat)[1][1] ) );
    iso := IsomorphismPermGroup( Umat );
    U   := Image( iso );
    SetSize( U, Size( Umat ) );

    finish := function( M, W, mgens )
        SetSize( M, Size( Umat ) );
        SetParent( M, W );
        SetProjections( M,
            [ GroupHomomorphismByImagesNC( M, gl, mgens, mats ) ] );
        return M;
    end;

    gens := GeneratorsOfGroup( U );
    mats := List( gens, x -> PreImagesRepresentative( iso, x ) );

    # GL(1,p) is abelian and induces nothing on its subgroups
    if d = 1 or Size( U ) = 1 then
        return finish( U, U, gens );
    fi;

    elms := AsSSortedList( U );
    tr   := List( elms, x -> TraceMat( PreImagesRepresentative( iso, x ) ) );

    # the trace preserving automorphisms, as permutations of the elements
    stab := fail;
    A    := AutomorphismGroup( U );
    if Size( A ) > 1 then
        hom  := ActionHomomorphism( A, elms,
                                    function(x,a) return Image(a,x); end );
        stab := Image( hom );
        for v in Set( tr ) do
            poss := Filtered( [1..Length(elms)], i -> tr[i] = v );
            stab := Stabilizer( stab, poss, OnSets );
        od;
        if Size( stab ) = 1 then stab := fail; fi;
    fi;

    if stab = fail then
        return finish( U, U, gens );
    fi;

    # the trace preserving automorphisms already permute the elements of U,
    # so they normalise the regular representation of U on that set
    reg := List( gens, u -> PermListList( elms, List( elms, x -> x*u ) ) );
    M   := Group( reg );
    SetSize( M, Size(U) );
    W   := Group( Concatenation( reg, GeneratorsOfGroup( stab ) ) );
    SetSize( W, Size(U) * Size(stab) );

    small := SmallerDegreePermutationRepresentation( W : cheap );
    if NrMovedPoints( Image( small ) ) < NrMovedPoints( W ) then
        reg := List( reg, x -> Image( small, x ) );
        M   := Group( reg );
        W   := Image( small );
    fi;
    return finish( M, W, reg );
end;

#F  cf_ctMatGroupsGL2( p ) . . cube-free subgroups of GL(2,p) up to conjugacy
##
cf_ctMatGroupsGL2 := function( p )
    local key, groups, lv;

    key := String( p );
    if IsBound( cf_ctMats.(key) ) then return cf_ctMats.(key); fi;

    Info(InfoCF,2,"    Compute the cube-free subgroups of GL(2,",p,").");
    if p > 2 then
        groups := Concatenation( cf_Th52Red(p), cf_Th53(p), cf_Th51(p) );
    else
        groups  := [];
        lv      := Group([[Z(2),0*Z(2)],[0*Z(2),Z(2)]]);
        lv!.red := true;
        Add( groups, lv );
        lv      := Group([[Z(2),Z(2)],[Z(2),Z(2)*0]]);
        lv!.red := false;
        Add( groups, lv );
    fi;
    for lv in groups do Size( lv ); od;

    cf_ctMats.(key)   := groups;
    cf_ctModels.(key) := [];
    return groups;
end;

#############################################################################
##
#F  cf_ctAutGroupsGL2( p, k ) . . models of the subgroups of order dividing k
##
## Built lazily: GL(2,461) has 1388 cube-free subgroups, of which 9 have order
## dividing a k that can occur below 200000.
##
InstallGlobalFunction( cf_ctAutGroupsGL2, function( p, k )
    local mats, models, res, i, M;

    mats   := cf_ctMatGroupsGL2( p );
    models := cf_ctModels.(String(p));

    res := [];
    for i in [1..Length(mats)] do
        if k mod Size( mats[i] ) = 0 then
            if not IsBound( models[i] ) then
                M := cf_SmallModel( mats[i] );
                if mats[i]!.red then
                    SetSocleDimensions( M, [1,1] );
                else
                    SetSocleDimensions( M, [2] );
                fi;
                models[i] := M;
            fi;
            Add( res, models[i] );
        fi;
    od;
    return res;
end );

#############################################################################
##
#F  cf_ctMatGroupsC( p ) . . . . . . . . cube-free subgroups of GL(1,p)
##
cf_ctMatGroupsC := function( p )
    local key, b, groups, d, gr;

    key := String( p );
    if IsBound( cf_ctCycMats.(key) ) then return cf_ctCycMats.(key); fi;

    b      := [[ Z(p) ]];
    groups := [];
    for d in Filtered( DivisorsInt( p-1 ), IsCubeFreeInt ) do
        gr := Group( b^((p-1)/d) );
        SetSize( gr, d );
        Add( groups, gr );
    od;

    cf_ctCycMats.(key) := groups;
    cf_ctCyc.(key)     := [];
    return groups;
end;

#############################################################################
##
#F  cf_ctAutGroupsC( p, k ) . . . models of the subgroups of GL(1,p) = C_{p-1}
##
InstallGlobalFunction( cf_ctAutGroupsC, function( p, k )
    local mats, models, res, i, M;

    mats   := cf_ctMatGroupsC( p );
    models := cf_ctCyc.(String(p));

    res := [];
    for i in [1..Length(mats)] do
        if k mod Size( mats[i] ) = 0 then
            if not IsBound( models[i] ) then
                M := cf_SmallModel( mats[i] );
                SetSocleDimensions( M, [1] );
                models[i] := M;
            fi;
            Add( res, models[i] );
        fi;
    od;
    return res;
end );

#############################################################################
##
#F  cf_ctOrbitReps( G, list ) . . orbit representatives of G on a list of subgroups
##
cf_ctOrbitReps := function( G, list )
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
#F  cf_ctQuotients( U, primes ) . . . . . . . . . . . . Goursat data of  U
##
## Returns, up to conjugacy under Parent(U) (which normalises U), one record
##    rec( ind := index of N, hom := U -> quo, quo := U/N,
##         aut := automorphisms of quo induced by N_{Parent(U)}(N) )
## per normal subgroup N of U with U/N a {primes}-group.
##
## Cached in U; Aut(quo) is expensive and left to cf_ctAutData.
##
cf_ctQuotients := function( U, primes )
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
    norms := cf_ctOrbitReps( G, NormalSubgroupsAbove( U, syl, [] ) );

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
#F  cf_ctAutData( dat ) . . . . . . Aut(quo) as a permutation group, on demand
##
cf_ctAutData := function( dat )
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
#F  cf_ctSubdirect( U, V, k, last ) . . . . . . . . . . . subdirect products
##
## U <= G := Parent(U) and V <= H := Parent(V), with G normalising U and H
## normalising V. Gives the subdirect products S of U and V up to conjugacy in
## G x H: their number if last = true and |S| = k, otherwise the list of those
## with |S| dividing k, each with its parent set to its normaliser so that the
## invariant holds again next round.
##
cf_ctSubdirect := function( U, V, k, last, proj )
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
    quU := Filtered( cf_ctQuotients( U, primes ), x -> x.ind in inds );
    quV := Filtered( cf_ctQuotients( V, primes ), x -> x.ind in inds );
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
            cf_ctAutData( du );
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
#F  cf_ctSocleComplements( gr, k ) . . . . . number of socle complements only
##
## gr is a list of lists of models; counts the subdirect products of order k
## up to conjugacy.
##
cf_ctSocleComplements := function( gr, k )
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
                Append( tmp, cf_ctSubdirect( U, V, k, false, false ) );
            od;
        od;
        all := Filtered( tmp, x -> (Size(x)*rest) mod k = 0 );
    od;

    cnt := 0;
    for U in all do
        for V in gr[lGr] do
            cnt := cnt + cf_ctSubdirect( U, V, k, true, false );
        od;
    od;
    return cnt;
end;

#############################################################################
##
#F  cf_ctSocleComplementsFull( gr, k ) . . . . . . . . the socle complements
##
## As cf_ctSocleComplements, but returns the groups, each with the maps into
## the general linear groups as Projections. The factors are kept in the given
## order, so that Projections and SocleDimensions match the socle.
##
InstallGlobalFunction( cf_ctSocleComplementsFull, function( gr, k )
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
                Append( tmp, cf_ctSubdirect( U, V, k, false, true ) );
            od;
        od;
        all := Filtered( tmp, x -> (Size(x)*rest) mod k = 0 );
    od;
    return Filtered( all, x -> Size(x) = k );
end );

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
    if IsBound( cf_ctNrFF.(key) ) then return cf_ctNrFF.(key); fi;

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
                    Add( tempAutGr, cf_ctAutGroupsC( lv[1], k ) );
                else
                    Add( tempAutGr, cf_ctAutGroupsGL2( lv[1], k ) );
                fi;
            od;
            Info(InfoCF,2,"    Count socle complements of order ",k,".");
            number := number + cf_ctSocleComplements( tempAutGr, k );
        fi;
    od;

    cf_ctNrFF.(key) := number;
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
