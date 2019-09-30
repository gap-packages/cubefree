#############################################################################
##
#W  prelim.gi           Cubefree                               Heiko Dietrich
##
##
## This files contains some preliminary functions.

##############################################################################
##
#P  IsCubeFreeInt( n )
##
## return true if the integer n is cube-free
##
InstallMethod( IsCubeFreeInt,
    "for integers",  
    [ IsInt ], 0,
    n-> ForAll( Collected( FactorsInt( n ) ), x -> x[2] < 3 ) );


##############################################################################
##
#P  IsSquareFreeInt( n )
##
## returns true if the integer n is square-free
##
InstallMethod( IsSquareFreeInt,
    "for integers",
    [ IsInt ], 0,
    n-> ForAll(Collected( FactorsInt( n ) ) , x -> x[2] < 2 ) );


############################################################################# 
## 
#F  ConstructAllCFSimpleGroups( n ) 
## 
## returns all cube-free simple groups of order n up to isomorphism
##
InstallGlobalFunction( ConstructAllCFSimpleGroups, function ( size ) 
    local p; 
 
    # check
    if not IsPosInt( size ) or  not IsCubeFreeInt( size )  then
        Error("Argument has to be a positive cube-free integer.");
    fi;

    if size = 1 then 
        return []; 
    elif IsPrime( size) then
        return [CyclicGroup( size )];
    fi;

    p  := RootInt( size * 2 , 3 ) + 1; 
    if IsPrimeInt( p ) and p>3 and size = p * (p-1) * (p+1) / 2 then
        return [PSL( 2, p )];
    fi;

    return []; 
end ); 
 
############################################################################# 
## 
#F  ConstructAllCFNilpotentGroups( n ) 
## 
## returns all cube-free nilpotent groups of order n up to isomorphism
##
InstallGlobalFunction(ConstructAllCFNilpotentGroups, function ( size ) 
    local cl, p, G, temp, groups; 
 
    # check
    if not IsPosInt( size ) or  not IsCubeFreeInt( size )  then
        Error("Argument has to be a positive cube-free integer.");
    fi;
   
    if size = 1 then 
        return [TrivialGroup()]; 
    fi;

    cl     := Collected( FactorsInt( size ) );
    groups := [[]];
    
    for p in cl do
        temp := [];
        for G in groups do
            if p[2] = 1 then
                Add(temp, Concatenation( G, [p[1]] ) );
            else
                Add(temp, Concatenation( G, [p[1]^2] ) );
                Add(temp, Concatenation( G, [p[1] , p[1]] ) );
            fi;
        od;
        groups := ShallowCopy( temp );
    od;

    return List(groups, x -> AbelianGroupCons( IsPcGroup , x ) ); 
end );


############################################################################ 
## 
#F  cf_canUseSG( n ) 
## 
## checks if the order n is of the type p^2, p^2q, or squarefree.
## In this case the SmallGroups library should be used.
##
cf_canUseSG := function( n )
local tmp;

    if not IsList(n) then n := Collected(FactorsInt(n)); fi;
    tmp := Length(Filtered(n, x->x[2]=2));
    if tmp > 1 then 
        return false; 
    elif tmp = 1 then
        if Length(n)>2 then return false; fi;
    fi;
    return true;
 end;
    
        


##############################################################################
##
#F  CubefreeOrderInfo( arg )
##
## Returns information about how many socle extensions and how many Frattini-
## extensions are to compute
##
InstallGlobalFunction(CubefreeOrderInfo, function( arg )
    local nonAb, G, p, cl, A, nSize, solvFF, groups, nr, nrs, OrderOfSocles,
          OrderFFgroup, lv,tmp, n, disp, subs, nrsolv, erg;

    n    := arg[1];
    disp := true;
    if Length( arg ) = 2 then 
        disp := arg[2];
    fi;

    # check
    if not IsBool(disp) then Error("Second argument has to be a boolean.");fi;
    if not IsPosInt( n ) or not IsCubeFreeInt( n ) then
        Print("Argument has to be a positive cube-free integer.\n"); 
        return 0;
    fi;

    # catch the case of n = 1 
    if n = 1 then 
       return 1;
    fi; 

    ##########################
    OrderOfSocles := function(n)
    local SocOrders, temp, s, facS, ord, lv, facNS, possible; 
        SocOrders := Filtered(DivisorsInt(n),x->x>1);
        temp      := [];
        for s in SocOrders do
            facS  := Collected(FactorsInt(s));
            ord   := 1;
            for lv in facS do
                if lv[2]=1 then
                    ord := ord*(lv[1]-1);
                else
                    ord := ord*(lv[1]*(lv[1]-1)^2*(lv[1]+1));
                fi;
            od;
            if ord mod (n/s) = 0 then Add(temp,s); fi;
        od;
        SocOrders := temp;
        temp      := [];
        for s in SocOrders do
            possible := true;
            facS     := Collected(FactorsInt(s));
            facNS    := Collected(FactorsInt(n/s));
            ord      := 2;
            for lv in facS do
                if lv[2]=1 then 
                    ord := ord*(lv[1]-1); 
                else 
                   ord := ord*(lv[1]^2-1); 
                fi;
            od;
            for lv in facNS do
                if not ord mod lv[1] = 0 then possible := false; fi;
            od;
            if possible then Add(temp,s); fi;
        od;
        return temp;
     end;
    ###########################
    OrderFFgroup := function(n)
    local cl, primes;
        cl     := Collected( FactorsInt( n ) );  
        primes := Product(List(cl,x->x[1]));
        return Filtered(DivisorsInt(n),x-> x mod primes = 0);        
    end;
    ###########################


    # set up
    groups := [];
    cl     := Collected( Factors( n ) ); 
    nr     := [];
    nrs    := [];

    if disp and cf_canUseSG(n) then
        Print("#I This order is either squarefree or of the type p^2, p^2q.\n");
        Print("#I You can use 'AllSmallGroups' or 'NumberSmallGroups'\n");
        Print("#I of the SmallGroups library.\n");
    elif disp and n<50001 then
        Print("#I This order is less than 50000.\n");
        Print("#I You can use 'AllSmallGroups' or 'NumberSmallGroups'\n");
        Print("#I of the SmallGroups library.\n");
    fi;


    # determine the possible non-abelian factors PSL(2,p)
    nonAb:=[1];
    for p in cl do
        if (p[1]>3) and (n mod (p[1]*(p[1]-1)*(p[1]+1) / 2)=0) and
           IsCubeFreeInt(p[1]+1) and IsCubeFreeInt(p[1]-1) then
                Add(nonAb,p[1]*(p[1]-1)*(p[1]+1) / 2);
           
        fi;
    od;

    ## the orders of the solvable factor
    nr := List(nonAb,x->n/x);
    for A in nr do 
        for lv in OrderFFgroup(A) do
            Add(nrs,[A,[lv,OrderOfSocles(lv)]]); 
        od;
    od;
    tmp  := Sum(List(nrs, x-> Length(x[2][2])));
    erg  := tmp;
    subs := [];
    for A in nrs do
        subs := Concatenation(subs, List(A[2][2],
                              x->[A[2][1]/x,Collected(FactorsInt(x))]));
    od;
    subs := Filtered(subs, x-> not x[1]=1);
    if disp then
        Print("#####################################################################\n");
        Print("#I -- Information: Construction of the groups of order ",n," --\n");
        Print("#I What kind of socle complements are to construct:\n");
        Print(subs,"\n#I\n");
        Print("#I The above list has entries [n,[[p1,e1],..,[pl,el]]] with n>1\n");
        Print("#I and for such an entry one has to construct up to conj. all\n");
        Print("#I subgroups of order n of GL(p1,e1)x...xGL(pl,el) (socle complements).\n");
        tmp := Union(List(nrs, x-> x[2][2]{[1..Length(x[2][2])-1]}));
        tmp := Union(List(tmp, x->Collected(FactorsInt(x))));
        Print("#I The following GL(1,p)'s are to consider: p in  ",
                List(Filtered(tmp,x->x[2]=1),x->x[1]),"\n");
        Print("#I The following GL(2,p)'s are to consider: p in ",
                List(Filtered(tmp,x->x[2]=2),x->x[1]),"\n");
        Print("#I\n#I The possible solvable direct factors have orders ",nr,"\n");
        Print("#I The number of pairs\n");
        Print("#I      (order of solv. Fratt.-free group / order of its socle)\n");
        Print("#I for solvable Frattini-free groups which are to compute is ",erg,".\n");
        Print("#####################################################################\n");
    fi;
    return erg;
   
end);




##############################################################################
##
#F  cf_symmSDProducts ( q, n[,bool] )
##
## constructs subdirect products U of C_n with C_n such that 
## (x,y) in U if and only if (y,x) in U. We must have C_n \leq GF(q)^x
##
cf_symmSDProducts := function( arg )
local divn, m, auts, i, j, k, l, erg, gr, C, gen, D, a, sub, gnl,gnr,
      tmp, gens, exp,tmpgr, c, testAll, q, n;

    q := arg[1];
    n := arg[2];
    if Length(arg) = 3 then
        testAll  := arg[3];
    else
        testAll  := false;
    fi;

    if not (q-1) mod n = 0 then 
        Error("Input wrong");
    fi;

    if n = 1 then return Group([ DirectProductElement( [Z(q)^0,Z(q)^0  ] ) ] ); fi;

    # Construct C_n x C_n 
    C    := FromTheLeftCollector(2);
    SetRelativeOrder(C,1,n);
    SetRelativeOrder(C,2,n);
    C    := PcpGroupByCollector(C);
    gnl  := Pcp(C)!.gens;
    gnr  := gnl[2];
    gnl  := gnl[1];

    erg  := [C];
    divn := Filtered(DivisorsInt(n),x-> not x=n);
    for m in divn do
        auts := Filtered([1..n/m], x->  Gcd(n/m,x)=1 and (x^2) mod (n/m) = 1);
        for a in auts do
            gr := GroupByGenerators([gnl^(n/m),gnr^(n/m),gnl*gnr^a,gnl^a*gnr]);
            Add(erg,gr);       
        od;
    od;
  
    # rewrite groups
    tmp := [];
    c   := Z(q)^((q-1)/n);
    D   := Group(c);
    for gr in erg do
        gens  := GeneratorsOfGroup(gr);
        gens  := List(gens, x-> 
                 DirectProductElement( [ c^(Exponents(x)[1]),c^(Exponents(x)[2]) ]));
        tmpgr := Group(gens);
        SetSize(tmpgr,Size(gr));
        Add(tmp,tmpgr);
    od;
    erg := tmp;
 
    ## test
    if testAll then
        Display("test cf_symmSDProducts");
        for gr in erg do
            if not ForAll(gr, x-> DirectProductElement([x[2]  ,x[1]] ) in gr) then
                Error(" not symm ");
            fi;
            if not Group(List(gr,x->x[1]))=D then
                Error(" not subd ");
            fi;
        od;

        sub := SubdirectProducts(D,D);
        sub := Filtered(sub, gr -> ForAll(gr, x-> DirectProductElement([x[2]  ,x[1]] ) in gr));
        if not Difference(sub,erg)=[] or not Difference(erg,sub)=[] or
           not IsDuplicateFreeList(erg) then
            Error("sth wrong");
        fi;
    fi;

    return erg;
end;



##############################################################################
##
#F  cf_completelyReducibleSG ( p[,bool] )
##
## construct all  subgroups of C_(p-1) x C_(p-1) \cong diag(2,p) of cubefree
## order coprime to p up to conjugacy in GL(2,p), i.e. up to the conjugaction 
## action of [[0,1],[1,0]].
##
cf_completelyReducibleSG := function( arg )
local C, gnl, gnr, div, erg, tmp, gr, ms, ns, divneu, gens, divs1,
      tmpgr ,el, n, k, m, l, els, c, d, D, sub, K, orbits, act, g,
      ind,a, testAll, p, pcg;

    p        := arg[1];
    if Length(arg) = 2 then
        testAll  := arg[2];
    else
        testAll  := false;
    fi;

    el       := Z(p);
    n        := p-1;
    div      := Filtered(DivisorsInt(n),IsCubeFreeInt);

    # Construct C_n x C_n 
    C    := FromTheLeftCollector(2);
    SetRelativeOrder(C,1,n);
    SetRelativeOrder(C,2,n);
    C    := PcpGroupByCollector(C);
    gnl  := Pcp(C)!.gens;
    gnr  := gnl[2];
    gnl  := gnl[1];

    erg  := [];
    for ms in div do
        for ns in Filtered(div, x-> x>=ms and IsCubeFreeInt(x*ms)) do
            gens         := [gnl^(n/ms),gnr^(n/ns)];
            gens         := Filtered(gens, x-> not x=x^0);
            if gens = [] then gens := [gnl^0]; fi;
            gr           := GroupByGenerators(gens);
            Add(erg,gr);
            divs1 := Filtered(DivisorsInt(n/ms),x->IsCubeFreeInt(x*ms*ns));
            for l in divs1 do
                if  l>1  and n mod (ns*l)=0 then
                    els := List(Filtered([1..l-1],x->Gcd(x,l)=1),y -> (n*y)/(ns*l));
                    if  ns=ms then
                        ## do not add 'symmetric' groups
                        tmp := [];
                        for c in els do
                            ind := Filtered([1..l-1],i->(n*i/l-c*ns) mod n = 0);
                            if not ForAny(ind,i-> ForAny(tmp,b->(b*i*ns-n/l) mod n =0)) then
                                Add(tmp,c);
                            fi;
                        od;
                        els := tmp;
                    fi;
                    for k in els do
                        gens := [gnl^(n/(ms*l))*gnr^k,gnl^(n/ms),gnr^(n/ns)];
                        gens := Filtered(gens,x-> not x=x^0);
                        if not ns=ms then
                            gr!.isNormal := false;
                        fi;
                        gr   := GroupByGenerators( gens );
                        Add(erg,gr);
                    od;
                fi;
            od;
        od;
    od;
   
    # rewrite groups
    tmp := [];
    for gr in erg do
        gens  := GeneratorsOfGroup(gr);
        if not IsBound(gr!.isNormal) then
            gr!.isNormal := ForAll(gens,x->gnl^(Exponents(x)[2])*gnr^(Exponents(x)[1]) in  gr);
        fi;
        gens  := List(gens, x-> 
                 [ [el^(Exponents(x)[1]), 0*el],[0*el,el^(Exponents(x)[2])] ]);
        tmpgr := Group(gens);
        SetSize(tmpgr,Size(gr));
        tmpgr!.red := true;
        tmpgr!.isNormal := gr!.isNormal;
        Add(tmp,tmpgr);
    od;
    erg := tmp;

    # test
    if testAll then
        Display("test cf_completelyReducibleSG");
        C   := CyclicGroup(p-1);
        D   := DirectProduct(C,C);
        d   := Filtered(GeneratorsOfGroup(D),x->Order(x)=p-1);
        tmp := SubgroupsSolvableGroup(D);
        K   := CyclicGroup(2);
        k   := GeneratorsOfGroup(K);
        g   := [GroupHomomorphismByImages(D,D,d,[d[2],d[1]])];
        act := function(pt,elm) return Image(elm,pt);end;
        tmp := Filtered(Orbits(K,tmp,k,g,act),x->IsCubeFreeInt(Size(x[1]))); 
        if not Length(tmp)=Length(erg) then Error("not the same length"); fi;
        if not IsDuplicateFreeList(erg) then Error("duplicates"); fi;
        a   := [[0,1],[1,0]]*One(GF(p));
        if not ForAll(erg,x -> (x^a=x) = x!.isNormal) then 
            Error("not normal");
        fi;
        for gr in erg do
            if gr^a in Difference(erg,[gr]) then
                Error("there is conj in list");
            fi;
        od;
        if not ForAll(erg,x->IsCubeFreeInt(Size(x))) then Error("not cubefree"); fi;
    fi;

    return erg;
end;



