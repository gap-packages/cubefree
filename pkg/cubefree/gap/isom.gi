
## record to store new functions...
cf:=rec();

## want to use NC version or not?
cf.TEST:=true;



cf.getsinger := function(p)
local K, prEl, prElp, one,b,c;
 
   K      := GF(p^2);
   prEl   := PrimitiveElement( K );
   prElp  := prEl^(p+1);
   b      := [[0,1],[-(prEl^(p+1)),prEl+prEl^p]]*One(GF(p));
   c      := [[1,0],[prEl+prEl^p,-(prElp^0)]]*One(GF(p));
   return [b,c];
end;



cf.makeStdNilpotent := function(G)
local ord, syl, a, b, S, id;
   ord  := Collected(FactorsInt(Order(G)));
   syl  := List(ord,p->SylowSubgroup(G,p[1]));
   a    := One(G);
   b    := One(G);
   for S in syl do
      id := MinimalGeneratingSet(S);
      if Size(id)=1 then 
         a := a * id[1]; 
      else
         a := a * id[1];
         b := b * id[2];
      fi;
   od;
   return rec(id:=List([a,b],Order), gens:=[a,b]);
end;




cf.IsomNilpotent := function(G,H)

   Display("start alg for nilpotent groups");

   if not IsBound(G!.std) then G!.std := cf.makeStdNilpotent(G); fi;
   if not IsBound(H!.std) then H!.std := cf.makeStdNilpotent(H); fi;
   if not G!.std.id = H!.std.id then return fail; fi;
   if cf.TEST then
      return GroupHomomorphismByImages(G,H,G!.std.gens,H!.std.gens);
   else
      return GroupHomomorphismByImagesNC(G,H,G!.std.gens,H!.std.gens);
   fi;
end;




cf.IsIsomNilpotent := function(G,H)
   if not IsBound(G!.std) then G!.std := cf.makeStdNilpotent(G); fi;
   if not IsBound(H!.std) then H!.std := cf.makeStdNilpotent(H); fi;
   if not G!.std.id = H!.std.id then return false; else return true; fi;
end;





#
# U,V are cubefree subgroups of GL(2,p), of order not divisible by p
# returns conjugating matrix t such that U^t=V, if exists
#
cf.isconjugate := function(U,V)
local p,iU,iV,eU,eV,UU,VV,m,r,ex,gU,gV,cm,cobV,cobU,e,el4U,el4V,cU,cV,
      HU,HV,nsU,nsV, nsUe, nsVe, i,j, t1,  mU, mV, oU, oV, ind, stds,stdc,
      cobStd,ns, fU, fV, get2part,o,s,w,isscalarmat, cs, ab, ims, S;

   if U=V then return U.1^0; fi;

   get2part := function(x)
   local o,c;
      o := Order(x);
      c := Collected(FactorsInt(o));
      if IsOddInt(o) then return [x,1]; fi;
      return [x^(o/(c[1][1]^c[1][2])),(c[1][1]^c[1][2])];
   end;

   isscalarmat := x-> IsDiagonalMat(x) and Size(Collected(List([1..Size(x)],i->
                      x[i][i])))=1;

   p  := Size(FieldOfMatrixGroup(U));
   if p=2 then return RepresentativeAction(GL(2,p),U,V); fi;

   iU := IsIrreducible(U);
   iV := IsIrreducible(V);
   if not iU = iV then return false; fi;
   if not Order(U)=Order(V) then return false; fi;

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
      if UU=VV then return eU*eV^-1; elif UU=VV^m then 
         return eU*m*eV^-1; 
      else return false; 
      fi;

 ##both not reducible
   else
      if not IsAbelian(U) = IsAbelian(V) then return false; fi;
     #if abelian then cyclic (D+OB Thm 5.1)
      if IsAbelian(U) then
        #Display("case irreducible and abelian");
         r  := Order(U);
         ex := Filtered([1..r],x-> Gcd(r,x)=1);
         gU := MinimalGeneratingSet(U)[1];
         gV := MinimalGeneratingSet(V)[1];
         cm := CompanionMat(CharacteristicPolynomial(gU));
         r  := First(ex,x->CompanionMat(CharacteristicPolynomial(gV^x))=cm);
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
            if not Size(HU)=Size(HV) then return false; fi;
            fU  := List(RightCosets(U,HU),x->get2part(Representative(x)));
            fV  := List(RightCosets(V,HV),x->get2part(Representative(x)));
            ind := Size(fU);
            m   := Maximum(List(fU,x->x[2]));
            if not m = Maximum(List(fV,x->x[2])) then return false; fi;
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
            if not nsU = nsV then return false; fi;
   
        
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
                   if not UU=VV then eV:=eV*m; fi;
                   UU := U^eU;
                   VV := V^eV;
                   if UU=VV then return eU*eV^-1; fi;
                
                   if ind = 2 or fU[1][2]=2 then s := One(GF(p)); 
                   else s := -One(GF(p)); fi;

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
                   else Error("ups"); fi;
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

               cm := CompanionMat(CharacteristicPolynomial(gU));
               r  := First(ex,x->CompanionMat(CharacteristicPolynomial(gV^x))=cm);
               gV := gV^r;
               r  := First(ex,x->CompanionMat(CharacteristicPolynomial(stds^x))=cm);
               stds := stds^r;
               e  := [1,0]*One(GF(p));
               cobU := [e,e*gU]^-1;
               cobV := [e,e*gV]^-1;
               cobStd := [e,e*stds]^-1;
               UU   := U^cobU;
               VV   := V^cobV;
               ns   := ns^cobStd;
               m    := RepresentativeAction(ns,UU,VV);
               if m = fail then Error("shouldn't happen...");fi;
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





cf.testisconj:=function(p,nr)
local clgl, U, o, V, t, tt,a,b;

  #Display("compute classes...");
   clgl := List(ConjugacyClassesSubgroups(GL(2,p)),Representative);
   clgl := Filtered(clgl,x-> not Order(x) mod p =0 and IsCubeFreeInt(Order(x)));
   #Display("done...");
   for U in clgl do 
      for o in [1..nr] do 
         repeat V:=Group(List([1..10],x->Random(U))); until Size(V)=Size(U);  
         t:=Random(GL(2,p)); 
         V:=V^t; 
         tt:=cf.isconjugate(U,V); 
         if not U^tt=V then Error("argg..."); fi; 
      od; 
    od;
    #Display("now test noncj");
    for a in [1..Size(clgl)] do
       for b in [1..Size(clgl)] do
          Print("test ",[a,b],"\n");
          tt := cf.isconjugate(clgl[a],clgl[b]);
          if not a=b and not tt=false then  Error("should not be conj..."); fi;
          if a=b and tt=false then Error("should be conj!"); fi;
          if a=b and not clgl[a]^tt =clgl[b] then Error("mhmm.."); fi;
       od;
    od;
    return true;
end; 





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






cf.makegroupdirectly:=function(A,nr,emb,proj,type,S)
local gen, V, act, M, tmp, new, i, gM,j,k,g,els, SS, auts, ab, ims, a;

   gen := List(GeneratorsOfGroup(S),g->List(proj,x->List(Image(x,g),x->List(x,Int))  ));
   ab  := Flat(List(type,x->List([1..x[2]],i->x[1])));
   M   := AbelianGroup(ab);
   gM  := GeneratorsOfGroup(M);
   tmp := List(type,x->x[2]);
   for i in [2..Length(tmp)] do tmp[i]:=tmp[i-1]+tmp[i]; od;
   new := [gM{[1..tmp[1]]}];
   for i in [2..Size(tmp)] do Add(new, gM{[tmp[i-1]+1..tmp[i]]}); od;
   ims := [];
   for g in gen do
      tmp:=[];
      for j in [1..Size(new)] do
         els := new[j];
         act := g[j];
         for a in act do Add(tmp,Product(List([1..Size(els)],x->els[x]^a[x]))); od;
      od;
      Add(ims,Flat(tmp));
   od;
   if cf.TEST then
      auts := Group(List(ims,x->GroupHomomorphismByImages(M,M,gM,x)));
   else
      auts := Group(List(ims,x->GroupHomomorphismByImagesNC(M,M,gM,x)));
   fi;
   SS   := SemidirectProduct(auts,M);
   return SS;
end;






cf.makeIsomdirectly:=function(A,nr,emb,proj,type,G,H,conj,phiG,phiH,aa,bb)
local gen, V, act, M, tmp, new, i, gM,j,k,g,els, SS, autsH,autsG,conjaut,g1,g2,i1,i2,iso,toGG,toH,S,ims,GG,HH,a, ab;

   S   := Image(phiG);
   gen := List(GeneratorsOfGroup(S),g->List(proj,x->List(Image(x,g),x->List(x,Int))  ));
   ab  := Flat(List(type,x->List([1..x[2]],i->x[1])));
   M   := AbelianGroup(ab);
   gM  := GeneratorsOfGroup(M);
   tmp := List(type,x->x[2]);
   for i in [2..Length(tmp)] do tmp[i]:=tmp[i-1]+tmp[i]; od;
   new := [gM{[1..tmp[1]]}];
   for i in [2..Size(tmp)] do Add(new, gM{[tmp[i-1]+1..tmp[i]]}); od;
   ims := [];
   for g in gen do
      tmp:=[];
      for j in [1..Size(new)] do
         els := new[j];
         act := g[j];
         for a in act do Add(tmp,Product(List([1..Size(els)],x->els[x]^a[x]))); od;
      od;
      Add(ims,Flat(tmp));
   od;
   autsG := Group(List(ims,x->GroupHomomorphismByImages(M,M,gM,x)));
   GG   := SemidirectProduct(autsG,M);

   S   := Image(phiH);
   gen := List(GeneratorsOfGroup(S),g->List(proj,x->List(Image(x,g),x->List(x,Int))  ));
   ims := [];
   for g in gen do
      tmp:=[];
      for j in [1..Size(new)] do
         els := new[j];
         act := g[j];
         for a in act do Add(tmp,Product(List([1..Size(els)],x->els[x]^a[x]))); od;
      od;
      Add(ims,Flat(tmp));
   od;
   autsH := Group(List(ims,x->GroupHomomorphismByImages(M,M,gM,x)));
   HH   := SemidirectProduct(autsH,M);

   gen := List([conj],g->List(proj,x->List(Image(x,g),x->List(x,Int))  ));
   ims := [];
   for g in gen do
      tmp:=[];
      for j in [1..Size(new)] do
         els := new[j];
         act := g[j];
         for a in act do Add(tmp,Product(List([1..Size(els)],x->els[x]^a[x]))); od;
      od;
      Add(ims,Flat(tmp));
   od;
   conjaut := GroupHomomorphismByImages(M,M,gM,Flat(tmp));
  
   if not autsG^conjaut = autsH then Error("conj aut doesn't work..."); fi;
   
   
#######
   toGG := GroupHomomorphismByImages(G,GG,aa,
             Concatenation(List(gM,x->Image(Embedding(GG,2),x)), 
                           List(GeneratorsOfGroup(autsG),a->Image(Embedding(GG,1),a))));
   toH := GroupHomomorphismByImages(H,HH,bb,
             Concatenation(List(gM,x->Image(Embedding(HH,2),x)), 
                           List(GeneratorsOfGroup(autsH),a->Image(Embedding(HH,1),a))));
   if toGG=fail or toH=fail then
      Error("GG or HH wrong");
   fi;
   toH := InverseGeneralMapping(toH);


  g1 := GeneratorsOfGroup(autsG);
  g2 := GeneratorsOfGroup(M);
  i1 := List(g1, x-> Image(Embedding(HH,1),x^conjaut));
  i2 := List(g2, x->Image(Embedding(HH,2),Image(conjaut,x)));
  g1 := List(g1, x->Image(Embedding(GG,1),x));
  g2 := List(g2,x-> Image(Embedding(GG,2),x));
  iso := GroupHomomorphismByImages(GG,HH,Flat(Concatenation(g1,g2)),Flat(Concatenation(i1,i2)));
  if iso = fail then Error("iso on mat groups fail!"); fi;


#   Print("start with aa\n",aa,"\n");
#   Print("old gens bb\n",bb,"\n");
#   Print("need these ims\n",List(aa,x->x^(toGG*iso*toH)),"\n");



end;









cf.IsomSolvableFrattFree := function(G,H)
local socG, socH, CG, CH, actG, actH, ord, type, sc, se, Af, A, aa,bb,GG,HH,conjm,
      emb, proj, nr, phiG, phiH, psi, i, m, conj, U, V, mm, old, new,cg,ims, ttt, norm;

   Display("start algorithm for solvable frattfree case");

   socG := Socle(G);
   socH := Socle(H); 
   if not Size(socG)=Size(socH) then return fail; fi;

   if socG = G then
      #Display("grp is nilpotent");
      return cf.IsomNilpotent(G,H);
   fi;

   CG   := ComplementClassesRepresentativesSolvableNC(G,socG);
   CH   := ComplementClassesRepresentativesSolvableNC(H,socH);
   if CG = [] then return cf.IsomNilpotent(socG,socH); fi;
   CG   := CG[1];
   CH   := CH[1];



   actG := cf.getSocleAct(G,GeneratorsOfGroup(CG));
   actH := cf.getSocleAct(H,GeneratorsOfGroup(CH));
   if not List(actG.onC,x->Size(Group(x))) = List(actH.onC,x->Size(Group(x))) then return fail; fi;
   if not Size(actG.onE)=Size(actH.onE) then return fail; fi;

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
      if se=0 then 
         #Display("no el-ab socle bit..."); 
      else 
         #Display("images already good..."); 
      fi;
      if  not Image(phiG) = Image(phiH) then
         return fail;
      else
         if cf.TEST then
            psi := GroupHomomorphismByImages(G,H,
                     Concatenation( [ Flat(List(actG.groups[1],y->List(y,r->r))),
                                   Flat(List(actG.groups[2],y->List(y,r->r))),
                                   GeneratorsOfGroup(CG)]),
                     Concatenation( [Flat(List(actH.groups[1],y->List(y,r->r))),
                                  Flat(List(actH.groups[2],y->List(y,r->r))),
                                  List(GeneratorsOfGroup(CG),
                                       g->PreImagesRepresentative(phiH,Image(phiG,g)))]));
         else
            psi := GroupHomomorphismByImagesNC(G,H,
                     Concatenation( [ Flat(List(actG.groups[1],y->List(y,r->r))),
                                   Flat(List(actG.groups[2],y->List(y,r->r))),
                                   GeneratorsOfGroup(CG)]),
                     Concatenation( [Flat(List(actH.groups[1],y->List(y,r->r))),
                                  Flat(List(actH.groups[2],y->List(y,r->r))),
                                  List(GeneratorsOfGroup(CG),
                                       g->PreImagesRepresentative(phiH,Image(phiG,g)))]));
         fi;
         return psi;
      fi;
   fi;

## now the tricky bit
   ttt := Runtime();
  #Display("start conj elt");
   conj := [];
   for i in [1..Size(actG.onE)] do
      U := Group(actG.onE[i]);
      V := Group(actH.onE[i]);
      m := cf.isconjugate(U,V);
      if m = false or m=fail then return fail; fi;
      if not U^m = V then Error("should be conj!"); fi;
      Add(conj,m);
   od;
   conj:=Product(List([1..se],x-> Image(emb[sc+x],conj[x])));
  #Display("start norm");
   norm := []; 
   for i in [1..sc] do Add(norm,Group(GeneratorsOfGroup(Af[i]))); od;
   for i in [1..Size(actG.onE)] do
      norm[sc+i] := Normaliser(Af[sc+i],Group(actH.onE[i]));
   od; 
  #Display("done, now compute action rep");
   norm   := Concatenation(List([1..Size(norm)],x->List(GeneratorsOfGroup(norm[x]),g->Image(emb[x],g))));
   norm := Subgroup(A,norm);
   norm := RepresentativeAction(norm,Image(phiG)^conj,Image(phiH));   
   if norm=fail then return fail; fi;
   conj := conj*norm;
  #Display("all good!");
   ttt := Runtime()-ttt;
   Print("runtime for finding conj elt: ",ttt,"\n");



#Display("do stuff directly in A");
#conj := RepresentativeAction(A,Image(phiG),Image(phiH));
#Display("done...");
#conj := List([sc+1..nr],x->Image(proj[x],conj));

  #for i in conj do Display(i); od;

 ##first adjust image in E -- actually not sure why transposed mat, but this is what worked...
   conjm := List([sc+1..nr],x->Image(proj[x],conj));
   mm    := List(conjm,m->TransposedMat(List(m, x-> List(x, Int))));
   old   := List(actH.groups[2],x->List(x,t->t));   
   new   := [];
   for i in [1..Size(mm)] do
      Add(new, [old[i][1]^mm[i][1][1]*old[i][2]^mm[i][2][1], 
                old[i][1]^mm[i][1][2]*old[i][2]^mm[i][2][2]]);
     #Add(new, [old[i][1]^mm[i][1][1]*old[i][2]^mm[i][1][2], 
     #          old[i][1]^mm[i][2][1]*old[i][2]^mm[i][2][2]]);
   od;
#Print("old,new,actwith\n",old,"\n",new,"\n",mm,"\n");

   

   cg  := GeneratorsOfGroup(CG);

## test whether conj gives the same group
   ims := List(cg,x->Image(phiG,x)^conj);
   if not Image(phiH) = Subgroup(A,ims) then
      return fail;
   fi;

   
   aa := Concatenation( [ Flat(List(actG.groups[1],y->List(y,r->r))),
                                   Flat(List(actG.groups[2],y->List(y,r->r))),
                                   cg]);
  
   bb := Concatenation( [Flat(List(actH.groups[1],y->List(y,r->r))),
                                  Flat(new),
                                  List(ims,x->PreImagesRepresentative(phiH,x))]);

   if cf.TEST then 
      return GroupHomomorphismByImages(G,H,aa,bb);  
   else
      return GroupHomomorphismByImagesNC(G,H,aa,bb); 
   fi;
end;






cf.IsIsomSolvableFrattFree := function(G,H)
local socG, socH, CG, CH, actG, actH, ord, type, sc, se, Af, A, aa,bb,GG,HH,conjm,
      emb, proj, nr, phiG, phiH, psi, i, m, conj, U, V, mm, old, new,cg,ims, ttt, norm;

   Display("start solvable frattfree case");
   socG := Socle(G);
   socH := Socle(H); 
   if not Size(socG)=Size(socH) then return false; fi;

   if socG = G then
      #Display("grp is nilpotent");
      return cf.IsIsomNilpotent(G,H);
   fi;

   Display("get complements");
   CG   := ComplementClassesRepresentativesSolvableNC(G,socG)[1];
   CH   := ComplementClassesRepresentativesSolvableNC(H,socH)[1];

   if not Size(socG)=Size(socH) then return false; fi;
   actG := cf.getSocleAct(G,GeneratorsOfGroup(CG));
   actH := cf.getSocleAct(H,GeneratorsOfGroup(CH));
   if not List(actG.onC,x->Size(Group(x))) = List(actH.onC,x->Size(Group(x))) then return false; fi;
   if not Size(actG.onE)=Size(actH.onE) then return false; fi;

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
      if se=0 then 
         #Display("no el-ab socle bit..."); 
      else 
         #Display("images already good..."); 
      fi;
      if  not Image(phiG) = Image(phiH) then
         return false;
      else
         return true;
      fi;
   fi;
## now the tricky bit
   ttt := Runtime();
   conj := [];
   for i in [1..Size(actG.onE)] do
      U := Group(actG.onE[i]);
      V := Group(actH.onE[i]);
      m := cf.isconjugate(U,V);
      if m = false or m=fail then return false; fi;
      if not U^m = V then Error("should be conj!"); fi;
      Add(conj,m);
   od;
   conj:=Product(List([1..se],x-> Image(emb[sc+x],conj[x])));
   norm := []; 
   for i in [1..sc] do Add(norm,Group(GeneratorsOfGroup(Af[i]))); od;
   for i in [1..Size(actG.onE)] do
      norm[sc+i] := Normaliser(Af[sc+i],Group(actH.onE[i]));
   od; 
   norm   := Concatenation(List([1..Size(norm)],x->List(GeneratorsOfGroup(norm[x]),g->Image(emb[x],g))));
   norm := Subgroup(A,norm);
   norm := RepresentativeAction(norm,Image(phiG)^conj,Image(phiH));   
   if norm=fail then return false; fi;
   conj := conj*norm;
   ttt := Runtime()-ttt;
   Print("runtime for finding conj elt: ",ttt,"\n");

## test whether conj gives the same group
   ims := List(cg,x->Image(phiG,x)^conj);
   if not Image(phiH) = Subgroup(A,ims) then
      return false;
   fi;
   return true;

end;







cf.getrandomcopy := function(G)
local pcgs,H,ns,N,el,hom,Q,i,rel,els;

   if not IsPcGroup(G) then Error("need pc group as input"); fi;
   els  := [];
   H    := G;
   rel  := [];
   repeat
      ns  := Filtered(NormalSubgroups(H),x->
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

## test to see what's going wrong...
   if not Group(els) = G then Error("sth wrong with pcgs"); fi;
   Q := List([1..Size(els)],x->Subgroup(G,els{[x..Size(els)]}));
   for i in [2..Size(Q)] do
      if not IsNormal(Q[i-1],Q[i]) 
         or not IsCyclic(Q[i-1]/Q[i])
         or not Size(Q[i-1]/Q[i]) = rel[i-1] then
         Error("sth wrong");
      fi;
   od;
   if not IsCyclic(Q[Size(Q)]) or not Size(Q[Size(Q)]) = rel[Size(Q)] then
      Error("sth wrong");
   fi;
   if not Product(RelativeOrders(pcgs))=Size(G) then
      Print("order of els ",List(els,Order),"\n");
      Print("official rel ords ",RelativeOrders(pcgs),"\n");
      Print("rel ords wrong -- will get error...");
   fi;

   return GroupByPcgs(pcgs);
end;









cf.IsomSolvable := function(GG,HH)
local PG, PH, G, H, isoG, isoH,iso, homH, homG, QG, QH, genG, genH, imH, imG, 
      new, cnt, tup, t;

   
   Display("start algorithm for solvable case");

   if not IsPcGroup(GG) then
      Display("make isom pc group");
      isoG := IsomorphismPcGroup(GG);
      G    := Image(isoG);
   else
      isoG := GroupHomomorphismByImagesNC(GG,GG,GeneratorsOfGroup(GG),
                                              GeneratorsOfGroup(GG));
      G    := GG;
   fi; 
   if not IsPcGroup(HH) then
      Display("make isom pc group");
      isoH := IsomorphismPcGroup(HH);
      H    := Image(isoH);
   else
      isoH := GroupHomomorphismByImagesNC(HH,HH,GeneratorsOfGroup(HH),
                                              GeneratorsOfGroup(HH));
      H    := HH;
   fi; 

   Display("start fratt");
   PG := FrattiniSubgroup(G);
   PH := FrattiniSubgroup(H);
   if not Size(PG)=Size(PH) then return fail; fi;
   Display("done");

   if Size(PG)=1 then 
   
      iso := cf.IsomSolvableFrattFree(G,H);
      if iso= fail then return fail; fi;
      Display("combine isos");
      PG := (isoG*iso)*InverseGeneralMapping(isoH);
      Display("done");
      return PG;
   fi;

   Display("Frattini subgroups not trivial, take quotient");
   homG := NaturalHomomorphismByNormalSubgroup(G,PG);
   homH := NaturalHomomorphismByNormalSubgroup(H,PH);
   QG   := Image(homG); 
   QH   := Image(homH);
   
   iso := IsomorphismCubefreeGroups(QG,QH);
   if iso = fail then return fail; fi;
  
   Display("now lift this thing...");
  

 ##genG := MinimalGeneratingSet(G);
   genG := List(MinimalGeneratingSet(QG),x->PreImagesRepresentative(homG,x));

 

   imH  := List(genG, x-> 
               PreImagesRepresentative(homH,Image(iso,Image(homG,x))));
 ##check if works without modifications
   iso  := GroupHomomorphismByImages(G,H,genG,imH);
   if not iso=fail then
      Display("worked without mods!");
      return isoG*iso*InverseGeneralMapping(isoH);
   fi;
   Error("mhm... lift didn't work this time?!?");
 
end;







cf.IsIsomSolvable := function(GG,HH)
local PG, PH, G, H, isoG, isoH,iso;
   
   if not IsPcGroup(GG) then
      Display("make isom pc group");
      isoG := IsomorphismPcGroup(GG);
      G    := Image(isoG);
   else
      isoG := GroupHomomorphismByImagesNC(GG,GG,GeneratorsOfGroup(GG),
                                              GeneratorsOfGroup(GG));
      G    := GG;
   fi; 
   if not IsPcGroup(GG) then
      Display("make isom pc group");
      isoH := IsomorphismPcGroup(HH);
      H    := Image(isoH);
   else
      isoH := GroupHomomorphismByImagesNC(HH,HH,GeneratorsOfGroup(HH),
                                              GeneratorsOfGroup(HH));
      H    := HH;
   fi; 

   PG := FrattiniSubgroup(G);
   PH := FrattiniSubgroup(H);
   if not Size(PG)=Size(PH) then return false; fi;

   return cf.IsIsomSolvableFrattFree(G/PG,H/PH);
   
end;





##############################################################################
##
#F  IsomorphismCubefreeGroups(G,H)
##
## G and H are cubefree groups;
## returns an isomorphism from G to H, and fail if such an iso doesn't exist
##
InstallGlobalFunction( IsomorphismCubefreeGroups, function(G,H)
local Gpsl, Hpsl, idpslG, idpslH,Gsolv, Hsolv, genG, genH, phiA, isoSolv;

   if not ForAll([G,H],x-> IsPcGroup(x) or IsPermGroup(x)) then
      Error("input must be pc group of perm group");
   fi;
  
   if not Order(G)=Order(H) then return fail; fi;
   if not IsCubeFreeInt(Order(G)) or not IsCubeFreeInt(Order(H)) then
      Error("input must be two cubefree groups");
   fi;
   if not IsAbelian(G) = IsAbelian(H) then return fail; fi;
   if IsAbelian(G) and IsAbelian(H) then
      return cf.IsomNilpotent(G,H);
   fi; 
   if not IsSolvableGroup(G) = IsSolvableGroup(H) then return fail; fi;
   if IsSolvableGroup(G) and IsSolvableGroup(H) then
      return cf.IsomSolvable(G,H);
   fi;

   Display("get PSL");
   Gpsl := DerivedSeries(G);
   Gpsl := Gpsl[Length(Gpsl)];
   Hpsl := DerivedSeries(H);
   Hpsl := Hpsl[Length(Hpsl)];
   idpslG := Reversed(Collected(FactorsInt(Size(Gpsl))))[1][1];
   idpslH := Reversed(Collected(FactorsInt(Size(Hpsl))))[1][1];
   if not idpslG=idpslH then return fail; fi;

   Display("get isom between PSL");
   Display("WARNING: need to use efficient iso....");
   phiA := IsomorphismGroups(Gpsl,Hpsl);
   Display("done, now get PSL complement");

  #now both groups are PSL(2,idpslG) \times solvable-cubefree

   Gsolv   := Centraliser(G,Gpsl);
   Hsolv   := Centraliser(H,Hpsl);
   Display("done... now call solvable code");
   isoSolv := cf.IsomSolvable(Gsolv,Hsolv);
   if isoSolv = fail then return fail; fi;

   Display("now I'm here");

   genG := Concatenation(GeneratorsOfGroup(Gpsl),GeneratorsOfGroup(Gsolv));
   genH := Concatenation(List(GeneratorsOfGroup(Gpsl),x->Image(phiA,x)),
                 List(GeneratorsOfGroup(Gsolv),x->Image(isoSolv,x)));
   Display("now return end result");
   Display("WARNING: iso between psl not efficient... atm use NC");
 
  #if cf.TEST then
  #  return GroupHomomorphismByImages(G,H,genG,genH);
  #else
      return GroupHomomorphismByImagesNC(G,H,genG,genH);
  #fi;

end); 






IsIsomorphicCubefreeGroups := function(G,H)
local Gpsl, Hpsl, idpslG, idpslH,Gsolv, Hsolv, genG, genH, phiA, isoSolv;

   if G = H then return true; fi;
  #if not ForAll([G,H],x-> IsPcGroup(x) or IsPermGroup(x)) then
  #   Error("input must be pc group of perm group");
  #fi;
  
   if not Order(G)=Order(H) then return false; fi;
   if not IsCubeFreeInt(Order(G)) or not IsCubeFreeInt(Order(H)) then
      Error("input must be two cubefree groups");
   fi;
   if not IsAbelian(G) = IsAbelian(H) then return false; fi;
   if IsAbelian(G) and IsAbelian(H) then
      if not IsPcGroup(G) then G := Image(IsomorphismPcGroup(G)); fi;
      if not IsPcGroup(H) then H := Image(IsomorphismPcGroup(H)); fi;
      return cf.IsIsomNilpotent(G,H);
   fi; 
   if not IsSolvableGroup(G) = IsSolvableGroup(H) then return false; fi;
   if IsSolvableGroup(G) and IsSolvableGroup(H) then
      if not IsPcGroup(G) then G := Image(IsomorphismPcGroup(G)); fi;
      if not IsPcGroup(H) then H := Image(IsomorphismPcGroup(H)); fi;
      return cf.IsIsomSolvable(G,H);
   fi; 

   Display("get PSL");
   Gpsl := DerivedSeries(G);
   Gpsl := Gpsl[Length(Gpsl)];
   Hpsl := DerivedSeries(H);
   Hpsl := Hpsl[Length(Hpsl)];
   idpslG := Reversed(Collected(FactorsInt(Size(Gpsl))))[1][1];
   idpslH := Reversed(Collected(FactorsInt(Size(Hpsl))))[1][1];
   if not idpslG=idpslH then return false; fi;
  #now both groups are PSL(2,idpslG) \times solvable-cubefree

   Gsolv   := Centraliser(G,Gpsl);
   Hsolv   := Centraliser(H,Hpsl);
   if not IsPcGroup(Gsolv) then Gsolv := Image(IsomorphismPcGroup(Gsolv)); fi;
   if not IsPcGroup(Hsolv) then Hsolv := Image(IsomorphismPcGroup(Hsolv)); fi;
   Display("done... now call solvable code");
   return cf.IsIsomSolvable(Gsolv,Hsolv);
end; 






cf.testisom := function(G)
local H,K,t,iso;
   Display("get copy of group...");
  #H := Image(IsomorphismPermGroup(G));
  #repeat H := Group(List([1..30],x->Random(H))); until Size(H)=Size(G);
  #H := Image(IsomorphismPcGroup(H));
   H := cf.getrandomcopy(G);
   K := cf.getrandomcopy(H);
   Display("done");
   t := Runtime();
   iso := IsomorphismCubefreeGroups(K,H);
   t := Runtime()-t;
   if iso=fail or iso = false then 
       Display("<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>{}<>");
   fi;
  #Print("got ",iso,"\n");
   if iso=true then return "todo"; fi;
   return t;
end;



cf.testaut := function(G)
local F, A, a, AF, hom,gen;

   if Size(FrattiniSubgroup(G))=1 then return true; fi;
   hom := NaturalHomomorphismByNormalSubgroup(G,FrattiniSubgroup(G));
   F   := Image(hom);
   Print("size G,F ",List([G,F],Size),"\n");
   Display("get aut groups");
   A  := AutomorphismGroup(G);
   AF := AutomorphismGroup(F);
   Display("done");
   gen := List(GeneratorsOfGroup(A), a->
               GroupHomomorphismByImages(F,F,
                 MinimalGeneratingSet(F), 
                 List(MinimalGeneratingSet(F),e->
                     Image(hom,Image(a,PreImagesRepresentative(hom,e))))));
   Display("got gens, now check");
   if not Size(Group(gen))=Size(AF) then Error("mhm"); fi;
   return true;
end;


cf.testcentral := function(G)
local F, A, a, AF, hom,gen;

   hom := NaturalHomomorphismByNormalSubgroup(G,FrattiniSubgroup(G));
   F   := Image(hom);
   if IsAbelian(F) then
      if not IsAbelian(G) then Error("not ab"); fi;
      if not ForAll(GeneratorsOfGroup(FrattiniSubgroup(G)),x->x in Center(G)) then Error("upsi"); fi;
   fi;
   return true;
end;



cf.testgroups := function(G)
local F,hom,L,S,t,Y;

   F   := FrattiniSubgroup(G);
   hom := NaturalHomomorphismByNormalSubgroup(G,F);
   L   := Image(hom);
   S   := Socle(L);
   Y   := Subgroup(G, Concatenation(GeneratorsOfGroup(F),
                 List(GeneratorsOfGroup(S),x->PreImagesRepresentative(hom,x))));
   if not Size(S) mod Size(F) = 0 then return "nothing"; fi;
   t   := Filtered(FactorsInt(Size(S)),x-> not Size(F) mod x = 0);
   t   := Concatenation(List(PrimeDivisors(Size(F)),x->x^2),t);
   if IsomorphismGroups(AbelianGroup(t),Y) = fail then 
      Error("not of this form..");
   fi;  
   return true;
end;

## for n in Filtered([44100..50000],IsCubeFreeInt) do for i in [1..NumberSmallGroups(n)] do Print("<><><><><><><><><<<<<><>< start ",n,"\n"); G:=SmallGroup(n,i); if IsSolvable(G) and not Size(FrattiniSubgroup(G))=1 then testgroups(G); fi; od; od;


cf.testgl2 := function(p)
local grps, q, ab,nonab,G,S;

   grps  := cf_AutGroupsGL2(p);
   grps  := Filtered(grps,x->Size(x)>1);
   ab    := Filtered(grps,IsAbelian);
   nonab := Filtered(grps, x->not IsAbelian(x));

   for G in ab do
      for q in PrimeDivisors(Size(G)) do
         S := SylowSubgroup(G,q);
         if not IsCharacteristicSubgroup(G,S) then
            Print("size / prime ",[Size(G),q]);
            Error("ab not char!");
         fi;
      od;
   od;
   for G in nonab do
      for q in PrimeDivisors(Size(G)) do
         S := SylowSubgroup(G,q);
         if Size(S)=q and not IsCharacteristicSubgroup(G,S) then
            Print("size / prime ",[Size(G),q]);
            Display("NONab not char!");
         
         fi;
      od;
   od;
   return true;
end;

G := PcGroupCode(6881188796437515087738221448294468417208650874579305659374726796228752260125245833015590139, 5336100);

