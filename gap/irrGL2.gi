##############################################################################
##
#W  irrGL2.gi          Cubefree                                Heiko Dietrich
##

##
## IrreducibleSubgroupsOfGL( 2, q ) constructs all irreducible subgroups of 
## GL(2,q) up to conjugace where q=p^r is a prime power with p>=5.
## This algorithm was developed by Flannery and O'Brien.
##
## Bottlenecks are the rewritting over a minimal subfiels (see glasby.gi)
## and the computations of intersections, see cf_GL2_Th53


##############################################################################
##
#F  cf_GL2_Th51( q )
##
## (Theorem 5.1 of Flannary & O'Brien)
##
cf_GL2_Th51 := function( q )
    local b, div, groups, lv, gr, K, prEl, prElq;

    Info(InfoCF,2,"    Start cf_GL2_Th51.");

    groups := [];
    K      := GF( q^2 );
    prEl   := PrimitiveElement(K);
    prElq  := prEl^(q+1);
    b      := [[0*prEl,prEl^0],[-prElq, prEl+prEl^q]];

    # compute all possible orders
    div := DivisorsInt(q^2-1);
    div := Filtered(div,x-> not (q-1) mod x = 0);

    # construct groups
    for lv in div do
        gr := Group(b^((q^2-1)/lv));
        SetSize(gr,lv);
        SetIsSolvableGroup(gr,true);
        Add(groups,gr);
    od;

    return(groups);
end;
 
##############################################################################
##
#F  cf_NormSubD ( q )
##
## Constructs the subgroups of D(2,q) normal in M(2,q) of odd order, which are
## required in cf_GL2_Th52( q ).
##
cf_NormSubD := function( q )
    local elem, groups, G, proj, subDirProds, elements, toAdd, 
          matGrps, temp, C, makeMat, div, gr;

    Info(InfoCF,2,"    Start cf_NormSubD.");
    # auxiliary function
    makeMat := function(q,x,y)
       return [[x,0],[0,y]]*One(GF(q)); 
    end;

    groups := [];    
    div    := DivisorsInt(q-1);

    # A subgroup U\leq D(2,q) normal in M(2,q) is a subdirect product
    # a of subgroup V\leq Group(Z(q)) with V in proj since
    # U\leq D(2,q) normal in M(2,q) <=> [(x,y)\in U \iff (y,x)\in U]
    Info(InfoCF,4,"          Compute symm. subdirect products.");
    subDirProds := Flat(List(div,x->cf_symmSDProducts(q,x)));
    subDirProds := Filtered(subDirProds, x-> not Size(x) mod 2 = 0); 

    # Paraphrase the computed groups as matrix-groups
    matGrps  := [];
    for G in subDirProds do
        temp := [];
        for elem in GeneratorsOfGroup(G) do
            Add(temp, makeMat(q,elem[1],elem[2]));
        od;
        gr := Group(temp);
        SetSize(gr, Size(G));
        Add(matGrps, gr);
    od; 
    return matGrps;
end;



##############################################################################
##
#F  cf_GL2_Th52( q )
## 
## (Theorem 5.2 of Flannery and O'Brien) 
##
cf_GL2_Th52 := function( q )
    local a, groups, alpha, fac, t, i, j, G2sk, G2nsk, Gstrich, K, prEl,
          erz1, G, G1, G2, erz2, g, IsScalarGS, o, w, z, H, makeMat, one;

    Info(InfoCF,2,"    Start cf_GL2_Th52.");
    K    := GF( q );
    prEl := PrimitiveElement( K );
    one  := One( K );

    # local auxiliary functions#
    ###########################

    IsScalarGS := function( L ) 
        local g;
 
        for g in L do
            if not g[1][1] = g[2][2] then
                return(false);
            fi;
        od;
        return(true);
    end;

    ####

    o := function( q, i)
        return(prEl^((q-1)/(2^(i+1))));
    end;

    ####

    w := function(q,i)
    local a, elem;

        elem := [[o(q,i),0],[0,o(q,i)^-1]] * one;
        return(elem);
    end;

    ####

    z := function(q,i)
    local elem;

        elem := [[o(q,i),0],[0,o(q,i)]] * one;
        return(elem);
    end;

    ####

    H := function(q,i,j,k)
        local a, group;

        a:=[[0*prEl,prEl^0],[prEl^0,0*prEl]];
        if k=1 then
            group := Group([a, z(q,i), w(q,j)]);
        elif k=2 then
            group := Group([a*z(q,i+1), w(q,j)]);
        elif k=3 then
            group := Group([a, z(q,i+1)*w(q,j+1), w(q,j)]);
        fi;
        return(group);
    end;

    ####

    makeMat:=function(q,x,y)
       return [[x,0],[0,y]] * one; 
    end;

    #####

    # Determination of the subgroups of D(2,q) normal in M(2,q)
    Gstrich := cf_NormSubD(q);

    groups := [];
    a      := [[0*prEl,prEl^0],[prEl^0,0*prEl]];
  
    if q mod 4 = 1 then

        fac  := Collected(FactorsInt(q-1));
        t    := fac[1][2];
        alpha:= prEl^((q-1)/(2^(t)));

        G2sk := [];
        for i in [0..t-1] do
            for j in [1..t-1] do
                Add(G2sk, H(q,i,j,1));
            od;
        od;
        for i in [0..t-2] do
            for j in [1..t-1] do
                Add(G2sk, H(q,i,j,2));
            od;
        od;
        for i in [0..t-2] do
            for j in [1..t-2] do
                Add(G2sk, H(q,i,j,3));
            od;
        od;
        for j in [1..t-1] do
            Add(G2sk, Group(a*([[alpha,0],[0,1]]*one), w(q,j)));
        od;
        Add(G2sk, Group([a, [[o(q,t-1),0],[0,1]]*one, w(q,t-1)]));  
    
        G2nsk:=[];
        Add(G2nsk, Group(a));
        for i in [0..t-1] do
            for j in [0..t-1] do
                Add(G2nsk, H(q,i,j,1));
            od;
        od;
        for i in [0..t-2] do
            for j in [0..t-1] do
                Add(G2nsk, H(q,i,j,2));
            od;
        od;
        for i in [0..t-2] do
            for j in [0..t-2] do
                Add(G2nsk, H(q,i,j,3));
            od;
        od;
        for j in [0..t-1] do
            Add(G2nsk, Group(a*([[alpha,0],[0,1]]*one), w(q,j)));
        od;
        Add(G2nsk,Group([a,[[o(q,t-1),0],[0,1]]*one,w(q,t-1)])); 

        for G1 in Gstrich do

            erz1 := GeneratorsOfGroup(G1);

            if IsTrivial(G1) then
                G := [[[1,0],[0,1]]*one];
            else
                G := erz1;
            fi;

            if not IsScalarGS(G) then
                for G2 in G2nsk do
                    erz2 := GeneratorsOfGroup(G2);
                    Add(groups, Group(Concatenation(erz1,erz2)));
                od;
            else
                for G2 in G2sk do
                    erz2 := GeneratorsOfGroup(G2);
                    Add(groups, Group(Concatenation(erz1,erz2)));
                od;
            fi;

        od;
        
    # the case q mod 4 = 3
    else 
       
        G2sk  := [Group(a,[[1,0],[0,-1]]*one)];

        G2nsk := [ Group(a), Group(a, [[-1,0],[0,-1]]*one),
                   Group(a*[[1,0],[0,-1]]*one),
                   Group(a,[[1,0],[0,-1]]*one) ];
 
        for G1 in Gstrich do

            erz1 := GeneratorsOfGroup(G1);
            if IsTrivial(G1) then
                G := [[[1,0],[0,1]]];
            else
                G := erz1;
            fi;
           
            if not IsScalarGS(G) then
                for G2 in G2nsk do
                    erz2 := GeneratorsOfGroup(G2);
                    Add(groups, Group(Concatenation(erz1,erz2)));
                od;
            else
                for G2 in G2sk do
                    erz2 := GeneratorsOfGroup(G2);
                    Add(groups, Group(Concatenation(erz1,erz2)));
                od;
            fi;
        od;
    fi;
   
   for i in groups do SetIsSolvableGroup(i,true); od;
   return(groups);
end;


##############################################################################
##
#F  cf_GL2_Th53( q )
##
## (Theorem 5.3 of Flannery and O'Brien)
##
cf_GL2_Th53 := function( q )
    local b, c, temp, groups, lv, Zent, t, l, div, erz1, order, G, p,
          gr, A, k, ord,fac, el,r, K, one, prEl, prElq, i, m, mat;

    Info(InfoCF,2,"    Start cf_GL2_Th53.");
    p     := Collected(FactorsInt(q))[1][1];
    K     := GF(q^2);
    one   := One( K );
    prEl  := PrimitiveElement( K );
    prElq := prEl^(q+1);

    groups := [];
    mat    := [[prElq,0*prElq],[0*prElq,prElq]];
    Zent   := Group(mat);
    SetSize(Zent,q-1);

    # generator of singer-cycle
    b := [[0,1],[-(prEl^(q+1)),prEl+prEl^q]] * one;

    # element c with <c,b> = normalizer of singer-cycle
    c := [[1,0],[prEl+prEl^q,-(prElq^0)]] * one;    

    t   := Collected(FactorsInt(q-1))[1][2];
    l   := (q-1) / (2^t);
    div := DivisorsInt(q^2-1);

    # compute groups of the form <c,\hat{A}>
    temp := Filtered(div,x-> (not (2*(q-1)) mod x =0));
    for lv in temp do
        gr := Group(c,b^((q^2-1)/lv));
        SetSize(gr,2*lv);
        Add(groups,gr);
    od;

    # compute groups of the form <cb^(...),\tilde(A)>
    temp := Filtered(div,x-> (not (q-1) mod x =0));
    temp := Filtered(temp, x ->
                          not x mod 2^(Collected(FactorsInt(q^2-1))[1][2])=0);
   
    for lv in temp do
        el:= b^((q^2-1)/lv);
        A := Group(el);
        SetSize(A,lv);
 
        # compute  order=Size(Intersection(A,Zent));
        Info(InfoCF,4,"          Compute order of intersection.");
        r  := DivisorsInt(lv);
        ## r := First(r, x-> el^x in Zent);
        for i in r do
            m := el^i;
            if IsDiagonalMat(m) and m[1][1]=m[2][2] and 
               not First([1..q-1],x->mat[1][1]^x=m[1][1]) = fail then
                r := i;
                break;    
            fi;
        od;       
       
        order := lv/r;
        
        if order mod 2 = 0 then
            k    := Collected(FactorsInt(order))[1][2];
            erz1 := c*b^(2^(t-k)*l);
            gr   := Group([erz1,el]);
            SetSize(gr,2*lv);
            Add(groups,gr);  
        fi; 
    od;
    
    for i in groups do SetIsSolvableGroup(i,true); od;
    return(groups); 
end; 


##############################################################################
##
#F  cf_GL2_Th55( q )    
##
## (Theorem 5.5 of Flannery and O'Brien, q=p^r, p>=5)
##
cf_GL2_Th55 := function( q )
    local evenSc, s, w, a, b, c,i,fac, list, vz, groups, p, L,r, 
          prElp, prElq, prEl, Kp,Kq,K, one, prElq3;

    Info(InfoCF,2,"    Start cf_GL2_Th55.");

    # set up: find the right field
    groups := [];
    p      := Collected(FactorsInt(q))[1];
    r      := p[2];
    p      := p[1];
    if not q mod 3 = 1 then
        if IsEvenInt(r) then
            K     := GF( q );
            prEl  := PrimitiveElement(K);
            one   := One(K);
            prElq := prEl;
            prElp := prEl^((q-1)/(p^2-1));
        else
            K     := GF( p^(2*r) );
            prEl  := PrimitiveElement(K);
            one   := One(K);
            prElq := prEl^(q+1);
            prElp := prEl^((p^(2*r)-1)/(p^2-1));
        fi;
      
        w      := prElp^((p^2-1)/4);
        s      := 1/2 * [[w-1,w-1],[w+1,-w-1]]*one;
        fac    := DivisorsInt(q-1);
        fac    := Filtered(fac,x-> x mod 2 =0);
        evenSc := List(fac,x->[[prElq^((q-1)/x),0],[0,prElq^((q-1)/x)]]*one);

        for a in evenSc do
            L := [a, [[w,0],[0,-w]]*one, s];
            L := List(L, Immutable);
            for i in L do
                ConvertToMatrixRep(i,FieldOfMatrixList(L));
            od;  
            Add(groups, GroupByGenerators( L ) );
        od;

        for i in groups do SetIsSolvableGroup(i,true); od;
        return(groups);

    else
        if IsEvenInt(r) then
            K     := GF( p^(3*r) );
            prEl  := PrimitiveElement(K);
            one   := One(K);
            prElq := prEl^((p^(3*r)-1)/(q-1));
            prElp := prEl^((p^(3*r)-1)/(p^2-1));
            prElq3 := prEl;
        else
            K     := GF( p^(6*r) );
            prEl  := PrimitiveElement(K);
            one   := One(K);
            prElq := prEl^((p^(6*r)-1)/(q-1));
            prElp := prEl^((p^(6*r)-1)/(p^2-1));
            prElq3  := prEl^((p^(6*r)-1)/(p^(3*r)-1));
        fi;
        w      := prElp^((p^2-1)/4);
        s      := 1/2 * [[w-1,w-1],[w+1,-w-1]];
        fac    := DivisorsInt(q-1);
        fac    := Filtered(fac,x-> x mod 2 =0);
        evenSc := List(fac,x->[[prElq^((q-1)/x),0*prElq],
                               [0*prElq,prElq^((q-1)/x)]]);
        fac    := List(fac, x-> (q^3-1)/(3*x));
        vz     := [];
        for b in fac do
            a := prElq3^b;
            if a in GF(q) then
                Add(vz, [[a,0*a],[0*a,a]]);
            fi;
        od;
        for a in evenSc do
            L := [a, [[w,0*w],[0*w,-w]], s];
            L := List(L, Immutable);
            for i in L do
                ConvertToMatrixRep(i,FieldOfMatrixList(L));
            od;  
            Add(groups, GroupByGenerators( L ) );
        od;

        for b in vz do
            L := [b*s, [[w,0*w],[0*w,-w]]];
            L := List(L, Immutable);
            for i in L do
                ConvertToMatrixRep(i,FieldOfMatrixList(L));
             od; 
            Add(groups, GroupByGenerators( L ) );
        od;
    
        for i in groups do SetIsSolvableGroup(i,true); od;
        return(groups);
    fi;
end;



##############################################################################
##
#F  cf_GL2_Th56( q )   
##
## (Theorem 5.6 of Flannery and O'Brien, q=p^r, p>=5)
##
cf_GL2_Th56 := function( q )
    local El, s, w, a, fac, groups, p, alpha, th_x1, Kp, prElp,
          th_x2, u, M1, M2, r, log, x, K, one, prEl, prElq;

    Info(InfoCF,2,"    Start cf_GL2_Th56.");
    # set up: find the right field
    groups := [];
    p      := Collected(FactorsInt(q))[1];
    log    := p[2];
    p      := p[1];
    K     := GF( q^2 );
    prEl  := PrimitiveElement(K);
    one   := One(K);
    prElq := prEl^(q+1);
    prElp := prEl^((q^2-1)/(p^2-1));
    Kp    := Subfield(K,[prElp]);
  
    w      := prElp^((p^2-1)/4);
    th_x2  := Indeterminate(K,"th_x2":new);
    th_x1  := Indeterminate(Kp,"th_x1":new);
    alpha  := RootsOfUPol(K,th_x1^2-2)[1];    
    s      := 1/2 * [[w-1,w-1],[w+1,-w-1]] * one;
    u      := alpha^-1 * [[w+1,0],[0,-w+1]] * one;

    # The set of even order scalars and its square-roots in GL(2,q^2)
    fac := DivisorsInt(q-1);
    fac := Filtered(fac,x-> x mod 2 =0);
    El  := [];
    for x in fac do
        M1 := [[prElq^((q-1)/x),0*prElq],
               [0*prElq,prElq^((q-1)/x)]];
        r  := RootsOfUPol(K,th_x2^2-prElq^((q-1)/x))[1];
        M2 := [[r,0*r],[0*r,r]];
        Add(El,[M1,M2]);
    od;

    if (p mod 8 = 1) or (p mod 8 = 7) or (log mod 2 = 0) then
        for a in El do
            Add(groups, Group(s, u, a[1]));
            if a[2][1][1] in GF(q) then
                Add(groups, Group([s, a[2]*u, a[1]]));
            fi; 
        od;
    else
        for a in El do
            if a[2]*alpha in GL(2,q) then
                Add(groups, Group([s, a[2]*u, a[1]]));
            fi; 
        od;
    fi;
    for a in groups do SetIsSolvableGroup(a,true); od;
    return(groups);
end;
    


##############################################################################
##
#F  cf_GL2_Th58( q )   
##
## (Theorem 5.8 of Flannery and O'Brien, q=p^r, p>5)
##
cf_GL2_Th58 := function( q )
    local El, w, s, v, a, fac, groups, p, th_x1, beta, K, one, prEl, prElq,
          prElp, Kp, log;

    Info(InfoCF,2,"    Start cf_GL2_Th58.");
    groups := [];
    p      := Collected(FactorsInt(q))[1];
    log    := p[2];
    p      := p[1];
    K     := GF( q^2 );
    prEl  := PrimitiveElement(K);
    one   := One(K);
    prElq := prEl^(q+1);
    prElp := prEl^((q^2-1)/(p^2-1));
    Kp    := Subfield(K,[prElp]);
    w      := prElp^((p^2-1)/4);
    th_x1  := Indeterminate(Kp,"th_x1":new);
    beta   := RootsOfUPol(K,th_x1^2-5)[1];    
    s      := 1/2 * [[w-1,w-1],[w+1,-w-1]];
    v      := 1/2 * [[w,(1-beta)/2 - w*(1+beta)/2],
                     [(-1+beta)/2-w*(1+beta)/2,-w]];

    if (q mod 5 = 1) or (q mod 5 = 4) then
        fac := DivisorsInt(q-1);
        fac := Filtered(fac,x-> x mod 2 =0);
        # The set of even order scalars in GL(2,q)
        El := List(fac,x->[[prElq^((q-1)/x),0*prElq],
                           [0*prElq,prElq^((q-1)/x)]]);
         for a in El do
            Add(groups, Group([s, v, a, [[w,0*w],[0*w,-w]]]));
        od;
    fi;

    return(groups);

end;


##############################################################################
##
#F  cf_GL2_Th59( q )   
##
## (Theorem 5.9 of Flannery and O'Brien, q=p^r, p=5)
##
cf_GL2_Th59 := function( q )
    local groups, p, fac, El, a;

    Info(InfoCF,2,"    Start cf_GL2_Th59.");

    groups := [];
    p      := Collected(FactorsInt(q))[1][1];

    if p=5 then  
        fac := DivisorsInt(q-1);
        fac := Filtered(fac,x-> x mod 2 =0);

        #The set of even order scalars in GL(2,q)
        El := List(fac,x->[[Z(q)^((q-1)/x),0],[0,Z(q)^((q-1)/x)]]*One(GF(q)));
        for a in El do
            Add(groups, Group([ [[ Z(5), 0*Z(5) ], [0*Z(5), Z(5)^3]],
                                [[Z(5)^2, Z(5)^0], [Z(5)^2, 0*Z(5)]], a]));
        od;
    fi;

    return(groups);
end;

#############################################################################
##
#F  cf_GL2_Th514( q )    
##
## (Theorem 5.14 of Flannery and O'Brien, q=p^r, p>5)
##
cf_GL2_Th514 := function( q )
    local p, alpha, qL, qs, r, L, s, divs, groups, pot;

    Info(InfoCF,2,"        Start cf_GL2_Th514.");

    groups := [];
    p      := Collected(FactorsInt(q))[1][1];
    pot    := Collected(FactorsInt(q))[1][2];

    if (not (p mod 2 = 1)) or (not (q>5)) then
        return(groups);
    fi;

    alpha  := Z(q);
    qL     := DivisorsInt(pot);
    qL     := List(qL, x-> p^x);

    if p=3 then
        qL := Filtered(qL,x-> x>3);
    fi;

    qL     := List(qL,x-> [x,(q-1)/(x-1)]);

 
    for qs in qL do
        L := [];
        for s in Filtered(DivisorsInt(q-1), x-> IsEvenInt((q-1)/x)) do  
            if p<>5 or (p=5 and qs[1]>5) then
                Add(L, Group(Concatenation([[[alpha^s,0],[0,alpha^s]]
                            *One(GF(q))], GeneratorsOfGroup(SL(2,qs[1])))));
            fi;
            if IsEvenInt(qs[2]) then 
                Add(L,Group(Concatenation([[[alpha^s,0],[0,alpha^s]]
                            *One(GF(q)), 
                            [[alpha^(qs[2]/2),0],[0,alpha^(-qs[2]/2)]]
                            *One(GF(q))],
                            GeneratorsOfGroup(SL(2,qs[1])))));
            fi;
            if (IsEvenInt(s) and IsEvenInt(qs[2])) or 
               (IsOddInt(s) and IsOddInt(qs[2])) then 
                
                Add(L, Group(Concatenation([[[alpha^s,0],[0,alpha^s]]
                             *One(GF(q)),
                             [[alpha^((qs[2]+s)/2),0],[0,alpha^((s-qs[2])/2)]]
                             *One(GF(q))],
                             GeneratorsOfGroup(SL(2,qs[1])))));
            fi;
        od;
        groups:=Concatenation(groups,L);
    od;

    return(groups);
end;


###########################################################################
##
#F  cf_GL2_Th515( q )   
##
## (Theorem 5.15 of Flannery and O'Brien)
##
cf_GL2_Th515 := function( q )
    local p, groups, i;

    Info(InfoCF,2,"    Start cf_GL2_Th515.");

    groups := [];
    p      := Collected(FactorsInt(q))[1][1];

    if q=5 then
        return([SL(2,5), GL(2,5),
                Group(Concatenation(GeneratorsOfGroup(SL(2,5)),
                      [ [[4,0],[0,1]]*One(GF(5)) ]))]);
    fi;

    if (p=5 and q>5) then
        groups:=Concatenation(cf_GL2_Th514(q),cf_GL2_Th59(q));
    fi;

    if (p>5) then
        groups:=Concatenation(cf_GL2_Th514(q),cf_GL2_Th58(q));
    fi; 

    # Rewrite the groups over GL(2,q)
    for i in [1..Size(groups)] do
        Info(InfoCF,3,"      Rewrite Matrix groups over trace field.");
        if Size(FieldOfMatrixGroup(groups[i])) > q then
            groups[i]:=RewriteAbsolutelyIrreducibleMatrixGroup(groups[i]);
        fi;
    od;
   
    for i in groups do SetIsSolvableGroup(i,false);od;
    return(groups);
end;


#############################################################################
##
#M  IrreducibleSubgroupsOfGL( 2, q )   
##
## computes the irreducible subgroups of GL(2,q), p>=5, up to conjugacy
## 
InstallMethod(IrreducibleSubgroupsOfGL,
    true, 
    [ IsPosInt, IsPosInt], 0,
    function( n, q )
    local p, groups, temp, i;

    Info(InfoCF,1,"Construct irreducible subgroups of ",GL(n,q),".");

    if not n=2 then
        Display("Only the subgroups of GL(2,q) are available");
        TryNextMethod();
    fi;

    if not IsPrimePowerInt(q) then 
        Error("q must be a prime power integer");
    fi;

    groups := [];
    p      := Collected(FactorsInt(q))[1][1];

    if p>= 5 then
        if q mod 4 = 3 then
            temp := Concatenation(cf_GL2_Th56(q),cf_GL2_Th55(q));
            for i in [1..Size(temp)] do
                Info(InfoCF,3,"      Rewrite Matrix groups over trace field.");
                if Size(FieldOfMatrixGroup(temp[i])) > q then
                    temp[i]:=RewriteAbsolutelyIrreducibleMatrixGroup(
                             temp[i]);
                fi;
            od;
            groups := Concatenation(cf_GL2_Th515(q),cf_GL2_Th51(q),
                                    cf_GL2_Th52(q), cf_GL2_Th53(q), temp);
        else
            groups := Concatenation(cf_GL2_Th515(q),cf_GL2_Th56(q),
                                    cf_GL2_Th55(q), cf_GL2_Th51(q), 
                                    cf_GL2_Th52(q),cf_GL2_Th53(q));
        fi;
        return(groups);
    else
        Display("A prime power of p>= 5 is needed.");
        TryNextMethod();
    fi;
end);
