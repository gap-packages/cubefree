#############################################################################
##
#W  autgrps.gi           Cubefree
##
##
## The cube-free subgroups of GL(1,p) and GL(2,p) up to conjugacy, and small
## models of them.
##
## A model of U <= GL(d,p) is a permutation group M isomorphic to U whose
## parent induces on M exactly the automorphisms N_{GL(d,p)}(U) induces on U,
## together with the isomorphism back into GL(d,p) as Projections(M). That is
## all the rest of the package needs of U: a socle complement is determined up
## to conjugacy by the isomorphism types of its projections plus those induced
## automorphisms, conjugation being the only way the ambient general linear
## group enters, and ExtensionBySocle only needs the map back.
##
## Models matter because Image(IsomorphismPermGroup(GL(2,p))) has degree
## p^2-1, up to 196248 points for the primes below 200000, while the
## subgroups that matter have order at most a few hundred.
##

#############################################################################
##
##  Caches; cf_ClearAutGroupCaches frees them.
##
cf_MatsGL2   := rec();  # p -> the cube-free subgroups of GL(2,p)
cf_ModelsGL2 := rec();  # p -> their models, bound only where already computed
cf_MatsC     := rec();  # p -> the cube-free subgroups of GL(1,p)
cf_ModelsC   := rec();  # p -> their models, bound only where already computed

cf_ClearAutGroupCaches := function()
    cf_MatsGL2   := rec();
    cf_ModelsGL2 := rec();
    cf_MatsC     := rec();
    cf_ModelsC   := rec();
end;

##############################################################################
##
#F  cf_Th51( p )
##
##  Theorem 5.1 of Flannery and O'Brien (cube-free case).
##
cf_Th51 := function( p )
local b, div, groups, lv, gr, K, prEl, prElq;

    Info(InfoCF,3,"      Start cf_Th51.");

    groups := [];
    K      := GF( p^2 );
    prEl   := PrimitiveElement(K);
    prElq  := prEl^(p+1);
    b      := [[0*prEl,prEl^0],[-prElq, prEl+prEl^p]];

    # compute all possible orders
    div := DivisorsInt(p^2-1);
    div := Filtered( div , x -> IsCubeFreeInt( x ) and (not x mod p =0)
                           and (not (p-1) mod x = 0) and x>1);

    # construct groups
    for lv in div do
        gr := Group(b^((p^2-1)/lv));
        SetSize(gr,lv);
        SetIsSolvableGroup(gr,true);
        gr!.red := false;
        Add(groups,gr);
    od;
    return(groups);
end;


##############################################################################
##
#F  cf_Th52Red( p )
##
## Theorem 5.2 of Flannery and O'Brien (cube-free case) and the
## required cube-free reducible subgroups of GL(2,p)
##
cf_Th52Red := function( p )
    local a, temp, h001, h002, groups, Gstrich, Gnsk, w, z0, z1, C2, aMat,
          G1, G2, G, norm, erz1, erz2, reducible, b, M, C, D, d, nat,
          sub, K, k, g, act, i, IsScalarGS, ppos, s;

    Info(InfoCF,3,"      Start cf_Th52Red.");

    # auxiliary functions
    IsScalarGS := function(L)
    local g;

        for g in L do
            if not g[1][1]=g[2][2] then
                 return false;
            fi;
        od;
        return true ;
    end;

    Info(InfoCF,4,"            Compute reducible subgroups.");
    ## a reducible subgroup of GL(2,p) with order coprime to p
    ## is completely reducible and hence a diagonal matrix group.
    ## Diagonal matrix groups are conjugated in GL(2,p) iff
    ## they are conjugated in M(2,p) \cong C_2 \ltimes Diag(2,p)

    reducible  := cf_completelyReducibleSG( p );
    norm       := Filtered(reducible, x-> x!.isNormal = true and
                                          not Order(x) mod 2=0);


    # begin of Th 5.2
    groups  := [];
    a       := [[0*Z(p),Z(p)^0],[Z(p)^0,0*Z(p)]];
    Gstrich := norm;

    if p mod 4 =1 then

        # groups
        z0   := Z(p)^((p-1)/2);
        z1   := Z(p)^((p-1)/4);
        w    := [[z0,0*Z(p)],[0*Z(p),(z0)^-1]];
        h001 := Group([a,w,[[z0,0*Z(p)],[0*Z(p),z0]]]);
        h002 := Group([a*[[z1,0*Z(p)],[0*Z(p),z1]],w]);

        # the list Gnsk of groups if G2' is not scalar
        Gnsk := [Group(a)];
        Add(Gnsk,h001);
        Add(Gnsk,h002);

        for G1 in Gstrich do
            erz1 := GeneratorsOfGroup(G1);

            if IsTrivial(G1) then
                G := [[[1,0],[0,1]]];
            else
                G := erz1;
            fi;

            if not IsScalarGS(G) then
                for G2 in Gnsk do
                    erz2 := GeneratorsOfGroup(G2);
                    Add(groups,Group(Concatenation(erz1,erz2)));
                od;
            fi;

        od;

    # the case p mod 4 = 3
    else

        # the list Gnsk of groups if G2' is not scalar
        Gnsk := [Group(a)];
        temp := Group(a,[[-Z(p)^0,0*Z(p)],[0*Z(p),-Z(p)^0]]);
        Add(Gnsk,temp);
        temp := Group(a*[[Z(p)^0,0*Z(p)],[0*Z(p),-Z(p)^0]]);
        Add(Gnsk,temp);

        for G1 in Gstrich do
            erz1 := GeneratorsOfGroup(G1);

            if IsTrivial(G1) then
                G := [[[1,0],[0,1]]];
            else
                G := erz1;
            fi;

            if not IsScalarGS(G) then
                for G2 in Gnsk do
                    erz2 := GeneratorsOfGroup(G2);
                    Add(groups,Group(Concatenation(erz1,erz2)));
                od;
            fi;

        od;
    fi;

   for i in [1..Size(groups)] do
       groups[i]!.red := false;
   od;
   return(Concatenation(reducible,groups));
end;

##############################################################################
##
#F  cf_Th53( p )
##
## Theorem 5.3 of Flannary and O'Brien (cube-free case)
##
cf_Th53 := function(p)
    local b, c, temp, groups, lv, Zent, t, l, div, erz1, el, order, G,
          gr, A, center, k, ord, r, K, prEl, prElp, one, i, m, mat;

    Info(InfoCF,3,"      Start cf_Th53.");
    K      := GF(p^2);
    prEl   := PrimitiveElement( K );
    prElp  := prEl^(p+1);
    one    := One( K );
    groups := [];
    mat    := [[prElp,0*prElp],[0*prElp,prElp]];
    Zent   := Group(mat);
    SetSize(Zent,p-1);

    # generator of singer-cycle
    b      := [[0,1],[-(prEl^(p+1)),prEl+prEl^p]]*one;

    # element c with <c,b> = normalizer of singer-cycle
    c      := [[1,0],[prEl+prEl^p,-(prElp^0)]]*one;

    t      := Collected(FactorsInt(p-1))[1][2];
    l      := (p-1) / (2^t);
    div    := DivisorsInt(p^2-1);

    # compute groups of the form <c,\hat{A}>
    temp := Filtered(div,x-> (not (2*(p-1)) mod x =0) and IsCubeFreeInt(x));
    temp := Filtered(temp,x-> not x mod p =0);
    temp := Filtered(temp,x-> not x mod 4 =0);
    G    := Group([b,c]);
    SetSize(G,p^2-1);

    # set size of groups
    for lv in temp do
        gr := Group(c,b^((p^2-1)/lv));
        SetSize(gr,2*lv);
        Add(groups,gr);
    od;

    # compute groups of the form <cb^(...),\tilde(A)>
    temp := Filtered(div,x-> (not x mod p =0) and (not x mod 4 =0));
    temp := Filtered(temp,x-> (not (p-1) mod x =0) and IsCubeFreeInt(x));
    temp := Filtered(temp,x ->
                          not x mod 2^(Collected(FactorsInt(p^2-1))[1][2])=0);


    for lv in temp do
        el := b^((p^2-1)/lv);
        A  := Group(el);
        SetSize(A,lv);

        Info(InfoCF,4,"            Compute order of intersection.");
        #r := DivisorsInt(lv);
        #r := First(r,x->el^x in Zent);
        r  := DivisorsInt(lv);
        for i in r do
            m := el^i;
            if IsDiagonalMat(m) and m[1][1]=m[2][2] and
               not First([1..p-1],x->mat[1][1]^x=m[1][1]) = fail then
                r := i;
                break;
            fi;
        od;

        order := lv/r;

        if order mod 4 =2 then
            k    := 1;
            erz1 := c*b^(2^(t-k)*l);
            gr   := Group([erz1,el]);
            SetSize(gr,2*lv);
            Add(groups,gr);
        fi;

    od;

    # for technical reasons
    for lv in [1..Size(groups)] do
       groups[lv]!.red := false;
    od;

    return(groups);

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

#F  cf_MatGroupsGL2( p ) . . cube-free subgroups of GL(2,p) up to conjugacy
##
cf_MatGroupsGL2 := function( p )
    local key, groups, lv;

    key := String( p );
    if IsBound( cf_MatsGL2.(key) ) then return cf_MatsGL2.(key); fi;

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

    cf_MatsGL2.(key)   := groups;
    cf_ModelsGL2.(key) := [];
    return groups;
end;

#############################################################################
##
#F  cf_AutGroupsGL2( p, k ) . . models of the subgroups of order dividing k
##
## Built lazily: GL(2,461) has 1388 cube-free subgroups, of which 9 have order
## dividing a k that can occur below 200000.
##
cf_AutGroupsGL2 := function( p, k )
    local mats, models, res, i, M;

    mats   := cf_MatGroupsGL2( p );
    models := cf_ModelsGL2.(String(p));

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
end;

#############################################################################
##
#F  cf_MatGroupsC( p ) . . . . . . . . cube-free subgroups of GL(1,p)
##
cf_MatGroupsC := function( p )
    local key, b, groups, d, gr;

    key := String( p );
    if IsBound( cf_MatsC.(key) ) then return cf_MatsC.(key); fi;

    b      := [[ Z(p) ]];
    groups := [];
    for d in Filtered( DivisorsInt( p-1 ), IsCubeFreeInt ) do
        gr := Group( b^((p-1)/d) );
        SetSize( gr, d );
        Add( groups, gr );
    od;

    cf_MatsC.(key) := groups;
    cf_ModelsC.(key)     := [];
    return groups;
end;

#############################################################################
##
#F  cf_AutGroupsC( p, k ) . . . models of the subgroups of GL(1,p) = C_{p-1}
##
cf_AutGroupsC := function( p, k )
    local mats, models, res, i, M;

    mats   := cf_MatGroupsC( p );
    models := cf_ModelsC.(String(p));

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
end;

