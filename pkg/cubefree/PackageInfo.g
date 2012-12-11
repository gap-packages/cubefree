#############################################################################
##  
#W  PackageInfo.g          GAP 4 package 'Cubefree'            Heiko Dietrich
##  
##
##  Based on Frank Luebeck's template for PackageInfo.g.
##  

SetPackageInfo( rec(

PackageName := "Cubefree",
Subtitle := "Constructing the Groups of a Given Cubefree Order",
Version := "1.13",
Date := "10/12/2012",
ArchiveURL := "http://users.monash.edu.au/~heikod/cubefree/cubefree1.13",
ArchiveFormats := ".tar.gz",

Persons := [

 rec(
      LastName      := "Dietrich",
      FirstNames    := "Heiko",
      IsAuthor      := true,
      IsMaintainer  := true,
      Email         := "heiko.dietrich@monash.edu",
      WWWHome       := "http://users.monash.edu.au/~heikod/",
      PostalAddress := Concatenation( [
            "School of Mathematical Sciences",
            "Monash University\n",
            "VIC 3800\n Melbourne, Australia" ] ),
      Place         := "Melbourne",
      Institution   := "Monash University"),

],

Status := "accepted",
CommunicatedBy := "David Joyner (Annapolis)",
AcceptDate := "10/2007",

README_URL := "http://users.monash.edu.au/~heikod/cubefree/README",
PackageInfoURL := "http://users.monash.edu.au/~heikod/cubefree/PackageInfo.g",

AbstractHTML := 
"The <span class=\"pkgname\">Cubefree</span> package contains methods to construct up to isomorphism the groups of a given (reasonable) cubefree order. The main function ConstructAllCFGroups(n) constructs all groups of a given cubefree order n. The function NumberCFGroups(n) counts all groups of a cubefree order n. Furthermore, IrreducibleSubgroupsOfGL(2,q) constructs the irreducible subgroups of GL(2,q), q=p^r, p>=5 prime, up to conjugacy and RewriteAbsolutelyIrreducibleMatrixGroup(G) rewrites the absolutely irreducible matrix group G (over a finite field) over a minimal subfield.",

PackageWWWHome := "http://users.monash.edu.au/~heikod/cubefree.html",
               
PackageDoc := rec(
  BookName  := "Cubefree",
  ArchiveURLSubset := ["doc", "htm"],
  HTMLStart := "htm/chapters.htm",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Constructing the groups of a given cubefree order",
  Autoload  := true),

Dependencies := rec(
  GAP := ">=4.3",
  NeededOtherPackages := [["GrpConst","1.0"],["Polycyclic","1.0"]],
  SuggestedOtherPackages := [],
  ExternalConditions := [] ),

AvailabilityTest := ReturnTrue,
BannerString := "Loading Cubefree 1.13 ... \n",
Autoload := false,
Keywords := ["cubefree","construction of groups","irreducible matrix subgroups of degree 2"]

));


