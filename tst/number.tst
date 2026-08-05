gap> START_TEST("number.tst");

#
# NumberCFGroups / NumberCFSolvableGroups must agree with the SmallGroups
# library.  The second argument suppresses the shortcut which just reads the
# answer off the library, so that the counting algorithm is really exercised.
#
gap> ForAll( Filtered([2..2000], IsCubeFreeInt),
>            n -> NumberCFGroups(n,false) = NumberSmallGroups(n) );
true

#
# orders beyond the SmallGroups library
#
gap> List( [50020, 55076, 100188, 147620, 161700, 198380], NumberCFGroups );
[ 141, 50, 429, 670, 2756, 123 ]
gap> List( [50020, 55076, 100188, 147620, 161700, 198380],
>          NumberCFSolvableGroups );
[ 141, 50, 429, 670, 2750, 123 ]

#
# the order below 50000 with the most groups
#
gap> NumberCFGroups(44100,false);
3093
gap> NumberCFSolvableGroups(44100,false);
3087

#
# large orders
#
gap> NumberCFGroups(101^2*97*37^2*29^2);
8
gap> NumberCFGroups(19^2*23^2*29*37*73^2*107^2);
24
gap> NumberCFSolvableGroups(2*2*3*11*17*67);
144
gap> NumberCFGroups(2*2*3*11*17*67);
145

#
# CountAllCFGroupsUpTo agrees with NumberCFGroups
#
gap> l := CountAllCFGroupsUpTo(500);;
gap> ForAll( Filtered([1..500], IsCubeFreeInt), n -> l[n] = NumberCFGroups(n) );
true
gap> ForAll( Filtered([1..500], n -> not IsCubeFreeInt(n)), n -> not IsBound(l[n]) );
true

#
# argument checking
#
gap> NumberCFGroups(8);
Error, First argument has to be a cube-free integer.
gap> NumberCFGroups(0);
Error, First argument has to be a positive integer.
gap> NumberCFGroups(12,1);
Error, Second argument has to be Boolean.
gap> NumberCFGroups(12,true,true);
Error, Wrong input format: Either arg='size' or arg='size,bool'.
gap> NumberCFGroups(1);
1
gap> NumberCFSolvableGroups(1);
1

#
gap> STOP_TEST("number.tst");
