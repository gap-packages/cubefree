#############################################################################
##  
##  PackageInfo.g for the package `Cubefree` Heiko Dietrich

SetPackageInfo( rec(

PackageName := "Cubefree",
Subtitle := "Constructing the Groups of a Given Cubefree Order",
Version := "1.01",
Date := "11/02/2006",
ArchiveURL := "http://www.icm.tu-bs.de/ag_algebra/software/dietrich/Cubefree/cubefree1.01",
ArchiveFormats := ".tar.gz",

Persons := [

 rec(
      LastName      := "Dietrich",
      FirstNames    := "Heiko",
      IsAuthor      := true,
      IsMaintainer  := true,
      Email         := "h.dietrich@tu-bs.de",
      WWWHome       := "http://www.tu-bs.de/~y0015665/ ",
      PostalAddress := Concatenation( [
            "Institute Computational Mathematics",
            "TU Braunschweig\n",
            "Pockelsstr. 14\n D-38106 Braunschweig\n Germany" ] ),
      Place         := "Braunschweig",
      Institution   := "TU Braunschweig"),

],

Status := "deposited",

README_URL := "http://www.icm.tu-bs.de/ag_algebra/software/dietrich/Cubefree/readme",
PackageInfoURL := "http://www.icm.tu-bs.de/ag_algebra/software/dietrich/Cubefree/PackageInfo.g ",

AbstractHTML := 
"The <span class=\"pkgname\">Cubefree</span> package contains methods to construct up to isomorphism the groups of a given cubefree order. The main function ConstructAllCFGroups(n) constructs all groups of a given cubefree order n. The function NumberCFGroups(n) counts all groups of a cubefree order n. Furthermore, IrreducibleSubgroupsOfGL(2,q) constructs the irreducible subgroups of GL(2,q), q=p^r, p>=5 prime, up to conjugacy and RewriteAbsolutelyIrreducibleMatrixGroup(G) rewrites the absolutely irreducible matrix group G (over a finite field) over a minimal subfield.",

PackageWWWHome := "http://www.icm.tu-bs.de/ag_algebra/software/dietrich/Cubefree",
               
PackageDoc := rec(
  BookName  := "Cubefree",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "htm/chapters.htm",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Constructing the groups of a given cubefree order",
  Autoload  := true),

Dependencies := rec(
  GAP := ">=4.3",
  NeededOtherPackages := [["GrpConst","1,0"]],
  SuggestedOtherPackages := [],
  ExternalConditions := [] ),

AvailabilityTest := ReturnTrue,
BannerString := "Loading Cubefree 1.01 ... \n",
Autoload := false,
Keywords := ["constructing small groups of cubefree order"]

));


