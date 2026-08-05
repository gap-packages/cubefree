#############################################################################
##
#W  frattFree.gi           Cubefree                            Heiko Dietrich
##                                                              
##

##
## These are the functions to construct all Frattini-free groups of
## a given cube-free order up to isomorphism.
##

##############################################################################
##
#F  cf_FrattFreeSolvGroups( n )
##
## Returns all solvable Frattini-free groups of order n up to isomorphism
##
cf_FrattFreeSolvGroups := function( n )
    local facN, facS, SocOrders, temp, lv, groups, s, ord, tempAutGr,
          subDP, i, facNS, possible;
   
    Info(InfoCF,1,"  Compute solvable Frattini-free groups of order ",n,".");

    groups := [];
    facN   := Collected(FactorsInt(n));

    # compute all socles s with n/|s| divides |Aut(s)|
    SocOrders := Filtered(DivisorsInt(n),x->x>1);
    temp      := [];
    for s in SocOrders do
        facS  := Collected(FactorsInt(s));
        # compute size of Aut(Soc)
        ord   := 1;
        for lv in facS do
            if lv[2]=1 then
                ord := ord*(lv[1]-1);
            else
                ord := ord*(lv[1]*(lv[1]-1)^2*(lv[1]+1));
            fi;
        od;
        if ord mod (n/s) = 0 then
            Add(temp,s);
        fi;
    od;
    SocOrders := temp;
    
    # if S is is a socle with size s and p|(n/s), then p has to divide the 
    # order of a projection of a direct factor of the socle complement
    # K\leq Aut(S). Hence extract all these possible socle orders:
    temp := [];
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
            if not ord mod lv[1] = 0 then
                possible := false;
            fi;
        od;
        if possible then
            Add(temp,s);
        fi;
    od;
    SocOrders := temp;

    # for every socle compute a list of complements 
    for s in SocOrders do
       
        tempAutGr := [];
        facS      := Collected(FactorsInt(s));

        if s < n then

            # the small models of the subgroups of the GL(d,p), see count.gi
            for lv in facS do 
                if lv[2]=1 then
                    Add(tempAutGr,cf_AutGroupsC(lv[1],n/s));
                else
                    Add(tempAutGr,cf_AutGroupsGL2(lv[1],n/s));
                fi;
            od;

            # compute all subdirect products of tempAutGr of order n/s
            Info(InfoCF,2,"    Compute socle complements (subdir. prod.'s) of order "
                               ,n/s,".");
            subDP := cf_SocleComplements(tempAutGr,n/s);
            Info(InfoCF,2,"    Compute extensions by socle of order ",n,".");
            for i in [1..Length( subDP )] do
                subDP[i] := ExtensionBySocle( subDP[i] );
            od;
            groups := Concatenation(groups,subDP);

        # the case s=size
        else 
            lv     := AbelianGroup(FactorsInt(s));
            groups := Concatenation(groups,[rec(code  := CodePcGroup(lv),
                                                order := s,
                                                isFrattiniFree := true,
                                                extdim         := [],
                                                isUnique       := true)]);
        fi;
    od;

    return groups;
end;


##############################################################################
##
#F  ConstructAllCFFrattiniFreeGroups( n )
##
## Returns all Frattini-free groups of order n up to isomorphism
##
InstallGlobalFunction( ConstructAllCFFrattiniFreeGroups, function( n )
    local nonAb, G, p, cl, A, nSize, solvFF, groups;

    Info(InfoCF,1,"Construct all Frattini-free groups of order ",n,".");

    # check
    if not IsPosInt( n ) or not IsCubeFreeInt( n ) then
        Error("Argument has to be a positive cube-free integer."); 
    fi;

    # catch the case of n = 1 
    if n = 1 then 
        return [TrivialGroup()]; 
    fi; 
 
    # set up
    groups := [];
    cl     := Collected( Factors( n ) ); 

    # determine the possible non-abelian factors PSL(2,p)
    nonAb:=[TrivialGroup()];
    for p in cl do
        if (p[1]>3) and (n mod (p[1]*(p[1]-1)*(p[1]+1) / 2)=0) and
           IsCubeFreeInt(p[1]+1) and IsCubeFreeInt(p[1]-1) then
            
            G := PSL(2,p[1]);
            if Size(G)=n then
                Add(groups,G);
            else
                Add(nonAb,G);
            fi;
        fi;
    od;
    
    # for every non-abelian A compute a solvable complement
    for A in nonAb do
        nSize   := n/Size(A);
        solvFF  := List(cf_FrattFreeSolvGroups(nSize),PcGroupCodeRec);
        groups  := Concatenation(groups, List(solvFF,x->DirectProduct(A,x)));
    od;
  
    return(groups);
end);
