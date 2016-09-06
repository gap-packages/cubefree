gap> n:=5^2*7*13^2*67^2*97*107;
1377938614325
gap> CubefreeOrderInfo(n,false);
12
gap> Length(ConstructAllCFGroups(n));time;
12
53111
gap> n:=19^2*23^2*29*37*73^2*107^2;
12501895704027377
gap> CubefreeOrderInfo(n,false);
24
gap> NumberCFGroups(n);time;
24
190536
gap> Length(ConstructAllCFGroups(n));time;
24
948319
gap> n:=5^2*13*23^2*43^2*191;
60716861075
gap> CubefreeOrderInfo(n,false);
16
gap> Length(ConstructAllCFGroups(n)); time;
16
29146
gap>  n:=2*2*3*11*17*67;
150348
gap> CubefreeOrderInfo(n,false);
20
gap> NumberCFGroups(n);time;
145
12073
gap> Length(ConstructAllCFGroups(n)); time;
145
20757
gap> NumberCFSolvableGroups(n);time;
144
11925
gap> Length(ConstructAllCFSolvableGroups(n)); time;
144
18893
gap> Length(ConstructAllCFFrattiniFreeGroups(n)); time;
109
14421
gap> Length(ConstructAllCFNilpotentGroups(n));time;
2
12
gap> Length(ConstructAllCFSimpleGroups(n));time;
1
8
gap> SetInfoLevel(InfoCF,1);
gap> ConstructAllCFGroups(4620);;time;
#I  Construct all groups of order 4620.
#I    Compute solvable Frattini-free groups of order 2310.
#I    Compute solvable Frattini-free groups of order 4620.
#I  Construct 138 Frattini extensions.
#I    Compute solvable Frattini-free groups of order 77.
#I  Construct 1 Frattini extensions.
#I    Compute solvable Frattini-free groups of order 7.
#I  Construct 1 Frattini extensions.
15501
gap> n:=101^2*97*37^2*29^2;
1139236591513
gap> CubefreeOrderInfo(n,false);
8
gap> NumberCFGroups(n);time;
8
36
gap> SetInfoLevel(InfoCF,1);
gap> ConstructAllCFGroups(n);time;
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
1848
gap> n:=2*2*3*3*5*5*7*7;
44100
gap> CubefreeOrderInfo(n,false);
100
gap> NumberCFSolvableGroups(n,false);time;
3087
572639
gap> Length(ConstructAllCFSolvableGroups(n)); time;
3087
843085
gap> NumberCFGroups(n,false);time;
3093
719245
gap> Length(ConstructAllCFGroups(n)); time;
3093
1016763
gap> Length(ConstructAllCFFrattiniFreeGroups(n)); time;
1305
504451
gap> Length(ConstructAllCFNilpotentGroups(n));time;
16
180
