gap> START_TEST("manual.tst");

#
gap> n:=5^2*7*13^2*67^2*97*107;
1377938614325
gap> CubefreeOrderInfo(n,false);
12
gap> Length(ConstructAllCFGroups(n));
12

# gap> n:=19^2*23^2*29*37*73^2*107^2;
# 12501895704027377
# gap> CubefreeOrderInfo(n,false);
# 24
# gap> NumberCFGroups(n);
# 24
# gap> Length(ConstructAllCFGroups(n));
# 24

#
gap> n:=5^2*13*23^2*43^2*191;
60716861075
gap> CubefreeOrderInfo(n,false);
16
gap> Length(ConstructAllCFGroups(n));
16

#
gap>  n:=2*2*3*11*17*67;
150348
gap> CubefreeOrderInfo(n,false);
20
gap> NumberCFGroups(n);
145
gap> Length(ConstructAllCFGroups(n));
145
gap> NumberCFSolvableGroups(n);
144
gap> Length(ConstructAllCFSolvableGroups(n));
144
gap> Length(ConstructAllCFFrattiniFreeGroups(n));
109
gap> Length(ConstructAllCFNilpotentGroups(n));
2
gap> Length(ConstructAllCFSimpleGroups(n));
1
gap> SetInfoLevel(InfoCF,1);
gap> ConstructAllCFGroups(4620);;
#I  Construct all groups of order 4620.
#I    Compute solvable Frattini-free groups of order 2310.
#I    Compute solvable Frattini-free groups of order 4620.
#I  Construct 138 Frattini extensions.
#I    Compute solvable Frattini-free groups of order 77.
#I  Construct 1 Frattini extensions.
#I    Compute solvable Frattini-free groups of order 7.
#I  Construct 1 Frattini extensions.

#
gap> n:=101^2*97*37^2*29^2;
1139236591513
gap> CubefreeOrderInfo(n,false);
8
gap> SetInfoLevel(InfoCF,0);
gap> NumberCFGroups(n);
8
gap> SetInfoLevel(InfoCF,1);
gap> ConstructAllCFGroups(n);
#I  Construct all groups of order 1139236591513.
#I    Compute solvable Frattini-free groups of order 10512181.
#I    Compute solvable Frattini-free groups of order 304853249.
#I    Compute solvable Frattini-free groups of order 388950697.
#I    Compute solvable Frattini-free groups of order 1061730281.
#I    Compute solvable Frattini-free groups of order 11279570213.
#I    Compute solvable Frattini-free groups of order 30790178149.
#I    Compute solvable Frattini-free groups of order 39284020397.
#I    Compute solvable Frattini-free groups of order 1139236591513.
#I  Construct 8 Frattini extensions.
[ <pc group of size 1139236591513 with 7 generators>, 
  <pc group of size 1139236591513 with 7 generators>, 
  <pc group of size 1139236591513 with 7 generators>, 
  <pc group of size 1139236591513 with 7 generators>, 
  <pc group of size 1139236591513 with 7 generators>, 
  <pc group of size 1139236591513 with 7 generators>, 
  <pc group of size 1139236591513 with 7 generators>, 
  <pc group of size 1139236591513 with 7 generators> ]

#
gap> n:=2*2*3*3*5*5*7*7;
44100
gap> CubefreeOrderInfo(n,false);
100
gap> Length(ConstructAllCFNilpotentGroups(n));
16

#
gap> SetInfoLevel(InfoCF,0);
gap> STOP_TEST( "manual.tst", 1);
