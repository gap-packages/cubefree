gap> START_TEST("isom.tst");

#
gap> testOrder := function(n)
>   local allSG, allCF;
>   Print("testing order ", n, "\n");
>   allSG:=AllSmallGroups(n);;
>   Assert(0, Length(allSG) = NrSmallGroups(n));
>   allCF:=ConstructAllCFGroups(n);;
>   Assert(0, Length(allCF) = NrSmallGroups(n));
>   # IsomorphismCubefreeGroups currently only supports pc and perm groups,
>   # but ConstructAllCFGroups sometimes produces direct product group, so
>   # first convert all generated groups as needed
>   Apply(allCF, function(g)
>       if IsPcGroup(g) or IsPermGroup(g) then return g; fi;
>       if IsSolvableGroup(g) then
>         return Image(IsomorphismPcGroup(g));
>       fi;
>       return Image(IsomorphismPermGroup(g));
>   end);
>   Assert(0, ForAll(allSG, g -> Number(allCF, h -> IsomorphismCubefreeGroupsNC(g, h) <> fail) = 1));
>   Assert(0, ForAll(allSG, g -> Number(allCF, h -> IsomorphismCubefreeGroups(g, h) <> fail) = 1));
>   Assert(0, ForAll(allSG, g -> Number(allCF, h -> IsIsomorphicCubefreeGroups(g, h)) = 1));
> end;;

# test some small cases, corner cases
gap> Perform(Filtered([1..20], IsCubeFreeInt), testOrder);
testing order 1
testing order 2
testing order 3
testing order 4
testing order 5
testing order 6
testing order 7
testing order 9
testing order 10
testing order 11
testing order 12
testing order 13
testing order 14
testing order 15
testing order 17
testing order 18
testing order 19
testing order 20

# orders where all groups are solvable
gap> testOrder((3*5)^2);
testing order 225
gap> testOrder(2*(3*5)^2);
testing order 450
gap> testOrder((3*5)^2*7);
testing order 1575

# orders with some non-solvable groups
gap> testOrder(60);
testing order 60
get isom between PSL
WARNING: need to use efficient iso....
done, now get PSL complement
get isom between PSL
WARNING: need to use efficient iso....
done, now get PSL complement
done... now call solvable code
WARNING: iso between psl not efficient... 
gap> testOrder(180);
testing order 180
get isom between PSL
WARNING: need to use efficient iso....
done, now get PSL complement
get isom between PSL
WARNING: need to use efficient iso....
done, now get PSL complement
done... now call solvable code
WARNING: iso between psl not efficient... 

#
gap> STOP_TEST( "isom.tst", 1);
