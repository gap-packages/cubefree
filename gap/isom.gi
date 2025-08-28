## record to store new functions...
cf:=rec();

## in case one uses isom.gi as standalone, need this:
cf.IsCubeFreeInt :=   
function ( n )
    return ForAll( Collected( FactorsInt( n ) ), function ( x )
            return x[2] < 3;
        end );
end;


cf.resetRandomSource := function ( i )
    Reset( GlobalMersenneTwister, 231897 * i );
   ## the number 231897 has been determined with a random number generator
    return;
end;


#######################################################################
cf.getsinger := function(p)
local K, prEl, prElp, one,b,c;
 
   K      := GF(p^2);
   prEl   := PrimitiveElement( K );
   prElp  := prEl^(p+1);
   b      := [[0,1],[-(prEl^(p+1)),prEl+prEl^p]]*One(GF(p));
   c      := [[1,0],[prEl+prEl^p,-(prElp^0)]]*One(GF(p));
   return [b,c];
end;


#######################################################################
cf.makeStdNilpotent := function(G)
local ord, syl, a, b, S, id;
   if IsTrivial(G) then
      return rec(id:=[1,1], gens:=[One(G),One(G)]);
   fi;
   ord  := PrimeDivisors(Order(G));
   syl  := List(ord,p->SylowSubgroup(G,p));
   a    := One(G);
   b    := One(G);
   for S in syl do
      id := MinimalGeneratingSet(S);
      a := a * id[1];
      if Size(id)>1 then
         b := b * id[2];
      fi;
   od;
   return rec(id:=List([a,b],Order), gens:=[a,b]);
end;


#######################################################################
cf.IsomNilpotent := function(G,H)
   if not IsBound(G!.std) then G!.std := cf.makeStdNilpotent(G); fi;
   if not IsBound(H!.std) then H!.std := cf.makeStdNilpotent(H); fi;
   if G!.std.id <> H!.std.id then
      return fail;
   fi;
   return GroupHomomorphismByImagesNC(G,H,G!.std.gens,H!.std.gens);
end;



#######################################################################
cf.IsIsomNilpotent := function(G,H)
   if not IsBound(G!.std) then G!.std := cf.makeStdNilpotent(G); fi;
   if not IsBound(H!.std) then H!.std := cf.makeStdNilpotent(H); fi;
   return G!.std.id = H!.std.id;
end;


#######################################################################
#
# U,V are cubefree subgroups of GL(2,p), of order not divisible by p
# returns conjugating matrix t such that U^t=V, if exists
#
cf.isconjugate := function(U,V)
local p,iU,iV,eU,eV,UU,VV,m,r,ex,gU,gV,cm,cobV,cobU,e,el4U,el4V,cU,cV,
      HU,HV,nsU,nsV, nsUe, nsVe, i,j, t1,  mU, mV, oU, oV, ind, stds,stdc,
      cobStd,ns, fU, fV, get2part,o,s,w,isscalarmat, cs, ab, ims, S;

   if U=V then
      return Identity(U);
   fi;

   get2part := function(x)
   local o,v;
      o := Order(x);
      if IsOddInt(o) then
         return [x,1];
      fi;
      v := 2^PValuation(o,2);
      return [x^(o/v),v];
   end;

   isscalarmat := x-> IsDiagonalMat(x) and Size(Collected(List([1..Size(x)],i->
                      x[i][i])))=1;

   p  := Size(FieldOfMatrixGroup(U));
   if p=2 then
      return RepresentativeAction(GL(2,p),U,V);
   fi;

   iU := IsIrreducible(U);
   iV := IsIrreducible(V);
   if iU <> iV then
      return false;
   fi;
   if Order(U) <> Order(V) then
      return false;
   fi;

 ##both can be diagonalised
   if not iU then
     #Display("case reducible");
      eU := First(MinimalGeneratingSet(U),x-> not IsDiagonalMat(x));
      if eU=fail then eU := One(U); else  eU := Eigenvectors(GF(p),eU)^-1; fi;
      eV := First(MinimalGeneratingSet(V),x-> not IsDiagonalMat(x));
      if eV=fail then eV := One(V); else  eV := Eigenvectors(GF(p),eV)^-1; fi;
      UU := U^eU;
      VV := V^eV;
      m  := One(GF(p))*[[0,1],[1,0]];
      if UU=VV then
         return eU*eV^-1;
      elif UU=VV^m then
         return eU*m*eV^-1;
      else
        return false;
      fi;

 ##both not reducible
   else
      if IsAbelian(U) <> IsAbelian(V) then
         return false;
      fi;
     #if abelian then cyclic (D+OB Thm 5.1)
      if IsAbelian(U) then
        #Display("case irreducible and abelian");
         r  := Order(U);
         ex := Filtered([1..r],x-> Gcd(r,x)=1);
         gU := MinimalGeneratingSet(U)[1];
         gV := MinimalGeneratingSet(V)[1];
         cm := CharacteristicPolynomial(gU);
         r  := First(ex,x->CharacteristicPolynomial(gV^x)=cm);
         gV := gV^r;
         e  := [1,0]*One(GF(p));
         cobU := [e*gU,e]^-1;
         cobV := [e*gV,e];
         return cobU*cobV;

     ##now we have G_2\ltimes G_{2'} or <c,A> or <c', A'> 
      else
        
            r   := Filtered(PrimeDivisors(Order(U)),x->x>2);
            HU  := HallSubgroup(U,r);
            HV  := HallSubgroup(V,r);          
            if Size(HU) <> Size(HV) then
               return false;
            fi;
            fU  := List(RightCosets(U,HU),x->get2part(Representative(x)));
            fV  := List(RightCosets(V,HV),x->get2part(Representative(x)));
            ind := Size(fU);
            m   := Maximum(List(fU,x->x[2]));
            if m <> Maximum(List(fV,x->x[2])) then
               return false;
            fi;
            fU  := Filtered(fU,x->x[2]=m);
            fV  := Filtered(fV,x->x[2]=m);
            HU  := GeneratorsOfGroup(HU);
            HV  := GeneratorsOfGroup(HV);

          ##determine module structure of hall subgroup
            if ForAll(HU,isscalarmat) then
               nsU := 1; nsUe := First(HU,x->not x=x^0);
            else
               t1 := List(HU,x->[x,Length(Eigenspaces(GF(p),x))]);
               if ForAny(t1,x->x[2]=0) then
                  t1 := First(t1,x->x[2]=0);
                  nsU := 0; nsUe := t1[1];
               elif ForAny(t1,x->x[2]=2) then
                  t1 := First(t1,x->x[2]=2);
                  nsU := 2; nsUe := t1[1];
               else Error("shouldn't happen..nsU"); fi;
            fi;       
            if ForAll(HV,isscalarmat) then
               nsV := 1; nsVe := First(HV,x->not x=x^0);
            else
               t1 := List(HV,x->[x,Length(Eigenspaces(GF(p),x))]);
               if ForAny(t1,x->x[2]=0) then
                  t1 := First(t1,x->x[2]=0);
                  nsV := 0; nsVe := t1[1];
               elif ForAny(t1,x->x[2]=2) then
                  t1 := First(t1,x->x[2]=2);
                  nsV := 2; nsVe := t1[1];
               else Error("shouldn't happen..nsU"); fi;
            fi;       
            if not nsU = nsV then
               return false;
            fi;
        
           #Print("nsU is ",nsU,"\n");

          ##are in G_2\ltimes G_{2'} case
            if nsU=2 then
               #if ind = 2 then
                  #Display("case nonab irred and non-Singer");
                   eU := Eigenvectors(GF(p),nsUe)^-1;
                   eV := Eigenvectors(GF(p),nsVe)^-1;
                   UU := Group(nsUe)^eU;
                   VV := Group(nsVe)^eV;
                   m  := One(GF(p))*[[0,1],[1,0]];
                   if not UU=VV then
                      eV:=eV*m;
                   fi;
                   UU := U^eU;
                   VV := V^eV;
                   if UU=VV then
                      return eU*eV^-1;
                   fi;
                
                   if ind = 2 or fU[1][2]=2 then
                      s := One(GF(p));
                   else
                      s := -One(GF(p));
                   fi;

                   fU := List(fU,x->x[1]^eU);
                   fV := List(fV,x->x[1]^eV);
                   iU := First(fU,x-> x[1][1]=0*One(GF(p)));
                   iV := First(fV,x-> x[1][1]=0*One(GF(p)));
                  
                   mU := [[1,0],[0,s*iU[2][1]^-1]] * One(GF(p)); mU:=mU^-1;
                   mV := [[1,0],[0,s*iV[2][1]^-1]] * One(GF(p)); mV:=mV^-1;
                   eU := eU*mU;
                   eV := eV*mV;
                   UU := U^eU;
                   VV := V^eV;
                   if UU=VV then
                      return eU*eV^-1;
                   elif UU=VV^m then
                      return eU*m*eV^-1;
                   else
                      Error("ups");
                   fi;
               #fi;

              
          ##are in Singer case, with irreducible A
            elif nsU=0 then

              #Display("case nonab irred and singer with irred A");
               stds := cf.getsinger(p);
               ns   := Group(stds);
               stdc := stds[2]; 
               stds := stds[1];
               r    := Size(U)/2;
               stds := stds^(Order(stds)/r); 
               ex := Filtered([1..r],x-> Gcd(r,x)=1);
               gU := First(U,x->Order(x) = r);
               gV := First(V,x->Order(x) = r);
               cU := CharacteristicPolynomial(gU);
               cV := CharacteristicPolynomial(gV); 
               cs := CharacteristicPolynomial(stds);
               if not IsIrreducible(cU) or not IsIrreducible(cV) then
                  Display("shouldn't happen"); 
               fi;

               cm := CharacteristicPolynomial(gU);
               r  := First(ex,x->CharacteristicPolynomial(gV^x)=cm);
               gV := gV^r;
               r  := First(ex,x->CharacteristicPolynomial(stds^x)=cm);
               stds := stds^r;
               e  := [1,0]*One(GF(p));
               cobU := [e,e*gU]^-1;
               cobV := [e,e*gV]^-1;
               cobStd := [e,e*stds]^-1;
               UU   := U^cobU;
               VV   := V^cobV;
               ns   := ns^cobStd;
               m    := RepresentativeAction(ns,UU,VV);
               if m = fail then
                  Error("shouldn't happen...");
               fi;
               return cobU*m*cobV^-1;

 
           ##are in Singer case with scalar A
            elif nsU=1 then
              #Display("case nonab irred and singer with scalar A");
               fU := fU[1][1];
               fV := fV[1][1]; 
               eU := RepresentativeAction(GL(2,p),fU,fV);
               return eU;
            fi;            
        fi;
     fi;
end;




#######################################################################
#
# test function...
#
cf.testisconj:=function(p)
local clgl, U, o, V, t, tt,a,b,bb;

    Display("compute classes...");
    clgl := List(ConjugacyClassesSubgroups(GL(2,p)),Representative);
    clgl := Filtered(clgl,x-> not Order(x) mod p =0 and not Order(x)=1 
                             and cf.IsCubeFreeInt(Order(x)));
    Display("done... prepare copies");
    Display("now test noncj");
    for a in [1..Size(clgl)] do
       for b in [1..Size(clgl)] do
          Print("test ",[a,b],"\n");
          bb := clgl[b]^Random(GL(2,p));
          tt := cf.isconjugate(clgl[a],bb);
          if not a=b and not tt=false then  Error("should not be conj..."); fi;
          if a=b and tt=false then Error("should be conj!"); fi;
          if a=b and not clgl[a]^tt =bb then Error("mhmm.."); fi;
       od;
    od;
    return true;
end; 




#######################################################################
cf.getSocleAct := function(U,C)
local soc, ord, c, e, K, hom, cact, eact;
 
   soc  := Socle(U);
   ord  := Collected(FactorsInt(Size(soc)));
   c    := List(Filtered(ord,x->x[2]=1),p->Pcgs(SylowSubgroup(soc,p[1])));
   e    := List(Filtered(ord,x->x[2]=2),p->Pcgs(SylowSubgroup(soc,p[1])));
   cact := List(c, g->
                     List(C,k-> 
                     List(g,x->ExponentsOfPcElement(g,x^k)*One(GF(Order(x))))));
   eact := List(e, g->
                     List(C,k-> 
                     List(g,x->ExponentsOfPcElement(g,x^k))*One(GF(Order(g[1])))));
   return rec(onC := cact, onE := eact,groups:=[c,e]);
end;




#######################################################################
cf.IsomSolvableFrattFree := function(G,H)
local socG, socH, CG, CH, actG, actH, ord, type, sc, se, Af, A, aa,bb,GG,HH,conjm, eqIm,
      emb, proj, nr, phiG, phiH, psi, i, m, conj, U, V, mm, old, new,cg,ims, ttt, norm;

   
   socG := Socle(G);
   socH := Socle(H); 
   if Size(socG) <> Size(socH) then
      return fail;
   fi;

   if socG = G then
      return cf.IsomNilpotent(G,H);
   fi;

   CG   := ComplementClassesRepresentativesSolvableNC(G,socG)[1];
   CH   := ComplementClassesRepresentativesSolvableNC(H,socH)[1];
   
   actG := cf.getSocleAct(G,GeneratorsOfGroup(CG));
   actH := cf.getSocleAct(H,GeneratorsOfGroup(CH));
   if not List(actG.onC,x->Size(Group(x))) = List(actH.onC,x->Size(Group(x))) then 
      return fail; 
   fi;
   if not List(actG.onE,x->Size(Group(x))) = List(actH.onE,x->Size(Group(x))) then 
      return fail; 
   fi;
 
   ord  := Collected(FactorsInt(Size(socG)));
   type := Filtered(ord,x->x[2]=1); 
   sc   := Size(type);
   type := Concatenation(type,Filtered(ord,x->x[2]=2));
   se   := Size(type)-sc;
   Af   := List(type,x-> GL(x[2],x[1]));

  #overgroup Aut(B)\times\Aut(C) in which we embedd CH and CG
   A    := DirectProduct(Af);
   nr   := Size(type);
   emb  := List([1..nr],x->Embedding(A,x));
   proj := List([1..nr],x->Projection(A,x));

   phiG := GroupHomomorphismByImagesNC(CG,A,GeneratorsOfGroup(CG),
                List([1..Size(GeneratorsOfGroup(CG))], i->
                    Product(Concatenation(
                        List([1..sc], x->Image(emb[x],actG.onC[x][i])),
                        List([1..se], x->Image(emb[x+sc],actG.onE[x][i]))))));

   phiH := GroupHomomorphismByImagesNC(CH,A,GeneratorsOfGroup(CH),
                List([1..Size(GeneratorsOfGroup(CH))], i->
                    Product(Concatenation(
                        List([1..sc], x->Image(emb[x],actH.onC[x][i])),
                        List([1..se], x->Image(emb[x+sc],actH.onE[x][i]))))));

   eqIm := Image(phiG)=Image(phiH);
   if se = 0 and not eqIm then
      return fail;
   fi;

   if eqIm then  
      psi := GroupHomomorphismByImagesNC(G,H,
             Concatenation([Flat(actG.groups[1]),Flat(actG.groups[2]),GeneratorsOfGroup(CG)]),
             Concatenation([Flat(actH.groups[1]),Flat(actH.groups[2]),
                            List(GeneratorsOfGroup(CG),
                                 g->PreImagesRepresentative(phiH,Image(phiG,g)))]));
      return psi;
   fi;
 
   conj := [];
   for i in [1..Size(actG.onE)] do
      U := Group(actG.onE[i]);
      V := Group(actH.onE[i]);
      m := cf.isconjugate(U,V);
      if m = false or m=fail then
         return fail;
      fi;
     #if not U^m = V then Error("should be conj!"); fi;
      Add(conj,m);
   od;
   conj:=Product(List([1..se],x-> Image(emb[sc+x],conj[x])));


   norm := []; 
   for i in [1..sc] do Add(norm,Group(GeneratorsOfGroup(Af[i]))); od;
   for i in [1..Size(actG.onE)] do
      norm[sc+i] := Normaliser(Af[sc+i],Group(actH.onE[i]));
   od; 
   norm := Concatenation(List([1..Size(norm)],
            x -> List(GeneratorsOfGroup(norm[x]),g->Image(emb[x],g))));
   norm := Subgroup(A,norm);
   norm := RepresentativeAction(norm,Image(phiG)^conj,Image(phiH));   
    
   if norm=fail then
      return fail;
   fi;
   conj := conj*norm;


 ##first adjust image in E 
 ##this is v --> v*m, here use v as part of pcgs so do vector*matrix manually via exponents
   conjm := List([sc+1..nr],x->Image(proj[x],conj));
   mm    := List(conjm,m->List(m, x-> List(x, Int)));
   old   := List(actH.groups[2],x->List(x,t->t));   
   new   := [];
   for i in [1..Size(mm)] do
      Add(new, [old[i][1]^mm[i][1][1]*old[i][2]^mm[i][1][2], 
                old[i][1]^mm[i][2][1]*old[i][2]^mm[i][2][2]]);
   od;

   cg  := GeneratorsOfGroup(CG);
   ims := List(cg,x->Image(phiG,x)^conj);
   aa  := Concatenation( [Flat(actG.groups[1]),Flat(actG.groups[2]),cg]);  
   bb  := Concatenation( [Flat(actH.groups[1]),Flat(new),
                         List(ims,x->PreImagesRepresentative(phiH,x))
                         ]);

   return GroupHomomorphismByImagesNC(G,H,aa,bb); 
end; 




#######################################################################
cf.IsIsomSolvableFrattFree := function(G,H)
local socG, socH, CG, CH, actG, actH, ord, type, sc, se, Af, A, aa,bb,GG,HH,conjm, 
emb, proj, nr, phiG, phiH, psi, i, m, conj, U, V, mm, old, new,cg,ims, ttt, norm;


   socG := Socle(G);
   socH := Socle(H); 
   if Size(socG) <> Size(socH) then
      return false;
   fi;

   if socG = G then
      return cf.IsIsomNilpotent(G,H);
   fi;

 
   CG   := ComplementClassesRepresentativesSolvableNC(G,socG)[1];
   CH   := ComplementClassesRepresentativesSolvableNC(H,socH)[1];

 
   actG := cf.getSocleAct(G,GeneratorsOfGroup(CG));
   actH := cf.getSocleAct(H,GeneratorsOfGroup(CH));
   if not List(actG.onC,x->Size(Group(x))) = List(actH.onC,x->Size(Group(x))) then 
      return false; 
   fi;
   if not List(actG.onE,x->Size(Group(x))) = List(actH.onE,x->Size(Group(x))) then 
      return false; 
   fi;

   ord  := Collected(FactorsInt(Size(socG)));
   type := Filtered(ord,x->x[2]=1); 
   sc   := Size(type);
   type := Concatenation(type,Filtered(ord,x->x[2]=2));
   se   := Size(type)-sc;
   Af   := List(type,x-> GL(x[2],x[1]));

  #overgroup Aut(B)\times\Aut(C) in which we embedd CH and CG
   A    := DirectProduct(Af);
   nr   := Size(type);
   emb  := List([1..nr],x->Embedding(A,x));
   proj := List([1..nr],x->Projection(A,x));

   phiG := GroupHomomorphismByImagesNC(CG,A,GeneratorsOfGroup(CG),
                 List([1..Size(GeneratorsOfGroup(CG))], i->
                     Product(Concatenation(
                         List([1..sc], x->Image(emb[x],actG.onC[x][i])),
                         List([1..se], x->Image(emb[x+sc],actG.onE[x][i]))))));
   phiH := GroupHomomorphismByImagesNC(CH,A,GeneratorsOfGroup(CH),
                 List([1..Size(GeneratorsOfGroup(CH))], i->
                     Product(Concatenation(
                         List([1..sc], x->Image(emb[x],actH.onC[x][i])),
                         List([1..se], x->Image(emb[x+sc],actH.onE[x][i]))))));

   if se = 0 or Image(phiG)=Image(phiH) then
      if  not Image(phiG) = Image(phiH) then
         return false;
      else
         return true;
      fi;
   fi;

 
   conj := [];
   for i in [1..Size(actG.onE)] do
      U := Group(actG.onE[i]);
      V := Group(actH.onE[i]);
      m := cf.isconjugate(U,V);
      if m = false or m=fail then
         return false;
      fi;
      Add(conj,m);
   od;
   conj:=Product(List([1..se],x-> Image(emb[sc+x],conj[x])));

   norm := []; 
   for i in [1..sc] do Add(norm,Group(GeneratorsOfGroup(Af[i]))); od;
   for i in [1..Size(actG.onE)] do
      norm[sc+i] := Normaliser(Af[sc+i],Group(actH.onE[i]));
   od; 
   norm := Concatenation(List([1..Size(norm)],
             x->List(GeneratorsOfGroup(norm[x]),g->Image(emb[x],g))));
   norm := Subgroup(A,norm);
   norm := RepresentativeAction(norm,Image(phiG)^conj,Image(phiH));   

   return norm <> fail;

end;



#######################################################################
#
# get random pc series (refined, consistent) for pc group G 
#
cf.getrandomcopy := function(G)
local pcgs,H,ns,N,el,hom,Q,i,rel,els;

   if not IsPcGroup(G) then Error("need pc group as input"); fi;
   els  := [];
   H    := G;
   rel  := [];
   repeat
      ns  := Filtered(MaximalSubgroupClassReps(H),x-> IsNormal(H,x) and 
              Size(x)<Size(H) and IsPrimeInt(Size(H)/Size(x)));
      N   := Random(ns);
      hom := NaturalHomomorphismByNormalSubgroup(H,N);
      el  := MinimalGeneratingSet(Image(hom))[1];
      if not Order(el) mod Size(Image(hom))=0 then Error("mhmm"); fi;
      Add(els,PreImagesRepresentative(hom,el));
      Add(rel,Size(Image(hom)));
      H   := N; 
   until Size(H)=1;
   pcgs := PcgsByPcSequence(FamilyObj(els[1]),els);
   return GroupByPcgs(pcgs);
end;












################################################################################
##
## given: cubefree groups G, H with cyclic subgroups PG\cong PH\cong C_p of their Frattini subgroups
##        homG: G--> QG \cong G/PG    and  homH: H--> QH \cong H/PH
##        iso: QG --> QH and isomorpshism
## return: an isomorphism G --> H       
##
## SylowSystem: pairwise permutable sylow subgroups
##
cf.IsomSolvableCyclic := function(G,H,PG,PH,homG,homH,QG,QH,iso)
local ordp, p, H1, S1, N2,H2,H2im,homH2,gen,im,
      tmp, newiso,a,GG,SS;
    

   newiso := iso;
   ordp   := PrimeDivisors(Order(G));
   p      := Size(PG);
   if not IsPrime(p) then Error("need cyclic Frattini subgroups"); fi;
   SS     := SylowSystem(G);
   S1     := Filtered(SS,x->PrimePGroup(x)=p)[1];
   H1     := Subgroup(G,Union(List(Filtered(SS,x->not PrimePGroup(x)=p),MinimalGeneratingSet)));
   H2im   := Image(newiso,Image(homG,H1)); 
   N2     := PreImage(homH,H2im); 
   H2     := HallSubgroup(N2,Filtered(ordp,x-> not x=p));

   homH2 := GroupHomomorphismByImagesNC(H2,H2im,GeneratorsOfGroup(H2),
             List(GeneratorsOfGroup(H2),x->Image(homH,x)));

   gen := List(GeneratorsOfGroup(H1),x->x);
   im  := List(gen,x->PreImagesRepresentative(homH2,Image(newiso,Image(homG,x))));

   Add(gen,MinimalGeneratingSet(S1)[1]);
   Add(im, PreImagesRepresentative(homH,Image(newiso,Image(homG,gen[Size(gen)]))));
 
   newiso  := GroupHomomorphismByImagesNC(G,H,gen,im);
   return newiso;
    
end;   

 


###################################################################### 
RANDOMAUTS := false;

if RANDOMAUTS then Display("USE RANDOM AUTS IN CYCLIC LIFT..."); fi;

cf.IsomSolvable := function(GG,HH)
local PG, PH, G, H, isoG, isoH,iso, homH, homG, QG, QH, genG, genH, imH, imG, 
      new, cnt, tup, t, tmp,tmpiso, iii, genQG, pos, 
      mypf, g, tmpH, tmpG,gg, pcgs,pcgslift,pcgsliftall, pcgsliftone,
      genQH,ordp,frat,lift,homLiftNot,i,j,homLiftAll,im,
      S1,S2,H2,H1,N2,p,gen,newiso,H2im,homH2,newiso2,
      GGs, HHs, homGG, homHH, subGG, subHH, n,AAA;
   
   if not IsPcGroup(GG) then

      isoG := IsomorphismPcGroup(GG);
      G    := Image(isoG);
   else
      isoG := GroupHomomorphismByImagesNC(GG,GG,GeneratorsOfGroup(GG),
                                              GeneratorsOfGroup(GG));
      G    := GG;
   fi; 
   if not IsPcGroup(HH) then

      isoH := IsomorphismPcGroup(HH);
      H    := Image(isoH);
   else
      isoH := GroupHomomorphismByImagesNC(HH,HH,GeneratorsOfGroup(HH),
                                              GeneratorsOfGroup(HH));
      H    := HH;
   fi; 
 

   PG := FrattiniSubgroup(G);
   PH := FrattiniSubgroup(H);
   if Size(PG) <> Size(PH) then
      return fail;
   fi;

   if Size(PG)=1 then
   
      iso := cf.IsomSolvableFrattFree(G,H);
      if iso= fail then
         return fail;
      fi;

      PG := (isoG*iso)*InverseGeneralMapping(isoH);

      return PG;
   fi;


   ordp := PrimeDivisors(Order(PG));
   GGs   := [G];
   HHs   := [H];
   homGG := [];
   homHH := [];
   subGG := [];
   subHH := [];
   for i in [1..Size(ordp)] do
     Add(subGG,SylowSubgroup(PG,ordp[i]));;
     Add(subHH,SylowSubgroup(PH,ordp[i]));
     Add(homGG, NaturalHomomorphismByNormalSubgroup(GGs[i],subGG[i]));
     Add(homHH, NaturalHomomorphismByNormalSubgroup(HHs[i],subHH[i]));
     Add(GGs, Image(homGG[i]));
     Add(HHs, Image(homHH[i]));
     PG := FrattiniSubgroup(Image(homGG[i]));
     PH := FrattiniSubgroup(Image(homHH[i]));
   od;


   n    := Size(GGs);
   iso  := IsomorphismCubefreeGroups(GGs[n],HHs[n]);
   if iso = fail then
      return fail;
   fi;
 
   for i in [n,n-1..2] do

     ## for testing purposes
     if RANDOMAUTS then
        AAA := AutomorphismGroup(HHs[i]);
        iso := iso*Random(AAA);
     fi;
     
     ## iso between QG=G/PG and QH=H/PH, and PG,PH cyclic
     iso := cf.IsomSolvableCyclic(GGs[i-1],HHs[i-1],subGG[i-1],subHH[i-1],
                                     homGG[i-1],homHH[i-1],GGs[i],HHs[i],iso);
     if iso=fail then Error("sth wrong here..."); fi;
   od;

 return isoG*iso*InverseGeneralMapping(isoH);

end;   

    
############################################################################

cf.IsIsomSolvable := function(GG,HH)
local PG, PH, G, H, isoG, isoH,iso;
   
   if not IsPcGroup(GG) then

      isoG := IsomorphismPcGroup(GG);
      G    := Image(isoG);
   else
      isoG := GroupHomomorphismByImagesNC(GG,GG,GeneratorsOfGroup(GG),
                                              GeneratorsOfGroup(GG));
      G    := GG;
   fi; 
   if not IsPcGroup(GG) then

      isoH := IsomorphismPcGroup(HH);
      H    := Image(isoH);
   else
      isoH := GroupHomomorphismByImagesNC(HH,HH,GeneratorsOfGroup(HH),
                                              GeneratorsOfGroup(HH));
      H    := HH;
   fi; 

   PG := FrattiniSubgroup(G);
   PH := FrattiniSubgroup(H);
   if Size(PG) <> Size(PH) then
      return false;
   fi;

   return cf.IsIsomSolvableFrattFree(G/PG,H/PH);
   
end;





##############################################################################
##
#F  IsomorphismCubefreeGroups(G,H)
##
## G and H are cubefree groups;
## returns an isomorphism from G to H, and fail if such an iso doesn't exist
##
InstallGlobalFunction( IsomorphismCubefreeGroupsNC, function(G,H)
local Gpsl, Hpsl, Gsolv, Hsolv, genG, genH, phiA, isoSolv;

   if not ForAll([G,H],x-> IsPcGroup(x) or IsPermGroup(x)) then
      Error("input must be pc group of perm group");
   fi;
  
   if Order(G) <> Order(H) then
      return fail;
   fi;
   if not cf.IsCubeFreeInt(Order(G)) or not cf.IsCubeFreeInt(Order(H)) then
      Error("input must be two cubefree groups");
   fi;
   if IsAbelian(G) <> IsAbelian(H) then
      return fail;
   fi;
   if IsAbelian(G) and IsAbelian(H) then
      return cf.IsomNilpotent(G,H);
   fi; 
   if IsSolvableGroup(G) <> IsSolvableGroup(H) then
      return fail;
   fi;
   if IsSolvableGroup(G) and IsSolvableGroup(H) then
      return cf.IsomSolvable(G,H);
   fi;
 
   if DerivedLength(G) <> DerivedLength(H) then
     return fail;
   fi;
   Gpsl := DerivedSeries(G);
   Gpsl := Gpsl[Length(Gpsl)];
   Hpsl := DerivedSeries(H);
   Hpsl := Hpsl[Length(Hpsl)];
   if Size(Gpsl) <> Size(Hpsl) then
     return fail;
   fi;

Display("get isom between PSL");
Display("WARNING: need to use efficient iso....");
   phiA := IsomorphismGroups(Gpsl,Hpsl);
Display("done, now get PSL complement");

   # now both groups are PSL(2,p) \times solvable-cubefree, where p is
   # the largest prime divisor of the order of Gpsl resp. Hpsl

   Gsolv   := Centraliser(G,Gpsl);
   Hsolv   := Centraliser(H,Hpsl);

   isoSolv := cf.IsomSolvable(Gsolv,Hsolv);
   if isoSolv = fail then
      return fail;
   fi;

   genG := Concatenation(GeneratorsOfGroup(Gpsl),GeneratorsOfGroup(Gsolv));
   genH := Concatenation(List(GeneratorsOfGroup(Gpsl),x->Image(phiA,x)),
                 List(GeneratorsOfGroup(Gsolv),x->Image(isoSolv,x)));


   return GroupHomomorphismByImagesNC(G,H,genG,genH);
end); 
 


##############################################################################
##
#F  IsomorphismCubefreeGroups(G,H)
##
## G and H are cubefree groups;
## returns an isomorphism from G to H, and fail if such an iso doesn't exist
##
InstallGlobalFunction( IsomorphismCubefreeGroups, function(G,H)
local Gpsl, Hpsl, Gsolv, Hsolv, genG, genH, phiA, isoSolv, iso;

   if not ForAll([G,H],x-> IsPcGroup(x) or IsPermGroup(x)) then
      Error("input must be pc group of perm group");
   fi;
  
   if Order(G) <> Order(H) then
      return fail;
   fi;
   if not cf.IsCubeFreeInt(Order(G)) or not cf.IsCubeFreeInt(Order(H)) then
      Error("input must be two cubefree groups");
   fi;
   if IsAbelian(G) <> IsAbelian(H) then
      return fail;
   fi;
   if IsAbelian(G) and IsAbelian(H) then
      iso := cf.IsomNilpotent(G,H);
      if iso=fail then
         return fail;
      fi;
      genG := GeneratorsOfGroup(G);
      return GroupHomomorphismByImages(G,H,genG,List(genG,x->Image(iso,x)));
   fi; 
   if IsSolvableGroup(G) <> IsSolvableGroup(H) then
      return fail;
   fi;
   if IsSolvableGroup(G) and IsSolvableGroup(H) then
      iso := cf.IsomSolvable(G,H);
      if iso=fail then
         return fail;
      fi;
      genG := GeneratorsOfGroup(G);
      return GroupHomomorphismByImages(G,H,genG,List(genG,x->Image(iso,x)));
   fi;

   if DerivedLength(G) <> DerivedLength(H) then
     return fail;
   fi;
   Gpsl := DerivedSeries(G);
   Gpsl := Gpsl[Length(Gpsl)];
   Hpsl := DerivedSeries(H);
   Hpsl := Hpsl[Length(Hpsl)];
   if Size(Gpsl) <> Size(Hpsl) then
     return fail;
   fi;

Display("get isom between PSL");
Display("WARNING: need to use efficient iso....");
   phiA := IsomorphismGroups(Gpsl,Hpsl);
Display("done, now get PSL complement");

   # now both groups are PSL(2,p) \times solvable-cubefree, where p is
   # the largest prime divisor of the order of Gpsl resp. Hpsl

   Gsolv   := Centraliser(G,Gpsl);
   Hsolv   := Centraliser(H,Hpsl);
   Display("done... now call solvable code");
   isoSolv := cf.IsomSolvable(Gsolv,Hsolv);
   if isoSolv = fail then
      return fail;
   fi;

   genG := Concatenation(GeneratorsOfGroup(Gpsl),GeneratorsOfGroup(Gsolv));
   genH := Concatenation(List(GeneratorsOfGroup(Gpsl),x->Image(phiA,x)),
                 List(GeneratorsOfGroup(Gsolv),x->Image(isoSolv,x)));

Display("WARNING: iso between psl not efficient... ");
 
  return GroupHomomorphismByImages(G,H,genG,genH);
 
end); 




###############################################################################
##############################################################################
##
#F  IsIsomorphicCubefreeGroups(G,H)
## 
## G and H are cubefree groups;
## returns true iff G and H are isomorphic
##
InstallGlobalFunction( IsIsomorphicCubefreeGroups, function(G,H)
local Gpsl, Hpsl, Gsolv, Hsolv, genG, genH, phiA, isoSolv, len;

   if G = H then
      return true;
   fi;
  #if not ForAll([G,H],x-> IsPcGroup(x) or IsPermGroup(x)) then
  #   Error("input must be pc group of perm group");
  #fi;
  
   if Order(G) <> Order(H) then
      return false;
   fi;
   if not cf.IsCubeFreeInt(Order(G)) or not cf.IsCubeFreeInt(Order(H)) then
      Error("input must be two cubefree groups");
   fi;
   if IsAbelian(G) <> IsAbelian(H) then
      return false;
   fi;
   if IsAbelian(G) and IsAbelian(H) then
      if not IsPcGroup(G) then G := Image(IsomorphismPcGroup(G)); fi;
      if not IsPcGroup(H) then H := Image(IsomorphismPcGroup(H)); fi;
      return cf.IsIsomNilpotent(G,H);
   fi; 
   if IsSolvableGroup(G) <> IsSolvableGroup(H) then
      return false;
   fi;
   if IsSolvableGroup(G) and IsSolvableGroup(H) then
      if not IsPcGroup(G) then G := Image(IsomorphismPcGroup(G)); fi;
      if not IsPcGroup(H) then H := Image(IsomorphismPcGroup(H)); fi;
      return cf.IsIsomSolvable(G,H);
   fi; 

   if DerivedLength(G) <> DerivedLength(H) then
     return false;
   fi;
   Gpsl := DerivedSeries(G);
   Gpsl := Gpsl[Length(Gpsl)];
   Hpsl := DerivedSeries(H);
   Hpsl := Hpsl[Length(Hpsl)];
   if Size(Gpsl) <> Size(Hpsl) then
     return false;
   fi;
   # now both groups are PSL(2,p) \times solvable-cubefree, where p is
   # the largest prime divisor of the order of Gpsl resp. Hpsl

   Gsolv   := Centraliser(G,Gpsl);
   Hsolv   := Centraliser(H,Hpsl);
   if not IsPcGroup(Gsolv) then Gsolv := Image(IsomorphismPcGroup(Gsolv)); fi;
   if not IsPcGroup(Hsolv) then Hsolv := Image(IsomorphismPcGroup(Hsolv)); fi;
   return cf.IsIsomSolvable(Gsolv,Hsolv);
end); 




 



############################################################################################################
############################################################################################################
############################################################################################################



cf.testisom := function(G)
local H,K,t,iso;
   H := cf.getrandomcopy(G);
   K := cf.getrandomcopy(G);
   t := Runtime();
   iso := IsomorphismCubefreeGroups(K,H);
   t := Runtime()-t;
   if iso=fail or iso = false then
       Display("<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>");
       Error("ups");
   fi;
  #Print("got ",iso,"\n");
   if iso=true then
      return "todo";
   fi;
   return t;
end;




cf.testmtimes := function(from, to,m)
local cfi, n, j, k, G;
cfi:=Filtered([from..to],cf.IsCubeFreeInt);;
for n in cfi do
   Print("START GROUP ORDER ",n,"\n");
   for j in [1..NumberSmallGroups(n)] do
       for k in [1..m] do
          G:=SmallGroup(n,j); 
          if IsSolvable(G) then cf.testisom(G); fi;
       od;
   od;
od;
end;



## RandomIsomorphismTest with codes, just return runtime and size
cf.randomtest:= function(G,H)
  local t,cr,a;
  cr := List([G,H], x-> rec(order:=Order(x), code:=CodePcGroup(x)));;
  t:=Runtime();
  a:=RandomIsomorphismTest(cr,10);
  return [Runtime()-t,Size(a)];
end;

## extract RandomIsomorphismTest, don't reconstruct groups from code
cf.randomtest := function(G,H)
    local  list,n,codes, conds, code, found, i, j, k, l, rem, c;
    list:= [rec(),rec()];
    list[1].order:=Order(G);
    list[2].order:=Order(H);
    list[1].code := CodePcGroup(G);
    list[2].code := CodePcGroup(H);
    list[1].group:=G;
    list[2].group:=H;
    n := 10;
    codes := List( list, function ( x )
            return [ x.code ];
        end );
    conds := List( list, function ( x )
            return 0;
        end );
    rem := Length( list );
    c := 0;
    while Minimum( conds ) <= n and rem > 1  do
        for i  in [ 1 .. Length( list ) ]  do
            if Length( codes[i] ) > 0  then
                #Display("start spec");
                code := RandomSpecialPcgsCoded( list[i].group );
                #Display("done...");
                if code in codes[i]  then
                    conds[i] := conds[i] + 1;
                fi;
                found := false;
                j := 1;
                while not found and j <= Length( list )  do
                    if j <> i  then
                        if code in codes[j]  then
                            found := true;
                        else
                            j := j + 1;
                        fi;
                    else
                        j := j + 1;
                    fi;
                od;
                if found  then
                    k := Minimum( i, j );
                    l := Maximum( i, j );
                    codes[k] := Union( codes[k], codes[l] );
                    codes[l] := [  ];
                    conds[k] := 0;
                    conds[l] := n + 1;
                    rem := rem - 1;
                else
                    AddSet( codes[i], code );
                fi;
            fi;
        od;
#Display("now here...");
        c := c + 1;
        if c mod 10 = 0  then
            Info( InfoRandIso, 3, "     ", c, " loops, ", rem, " groups ",
             conds{Filtered( [ 1 .. Length( list ) ], function ( x )
                     return Length( codes[x] ) > 0;
                 end )}, " doubles ",
             List( codes{Filtered( [ 1 .. Length( list ) ], function ( x )
                       return Length( codes[x] ) > 0;
                   end )}, Length ), " presentations" );
        fi;
    od;
    for i  in [ 1 .. Length( list ) ]  do
        Unbind( list[i].group );
    od;
    return list{Filtered( [ 1 .. Length( codes ) ], function ( x )
             return Length( codes[x] ) > 0;
         end )};
end;




############################################################################################################
