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
Version := "1.21",
Date := "29/08/2025", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

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

PackageWWWHome  := "https://gap-packages.github.io/cubefree/",
README_URL      := Concatenation( ~.PackageWWWHome, "README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/gap-packages/cubefree",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/cubefree-", ~.Version ),
ArchiveFormats := ".tar.gz",
                                 
AbstractHTML := 
"The <span class=\"pkgname\">Cubefree</span> package contains methods to construct up to isomorphism the groups of a given (reasonable) cubefree order. The main function ConstructAllCFGroups(n) constructs all groups of a given cubefree order n. The function NumberCFGroups(n) counts all groups of a cubefree order n. Furthermore, IrreducibleSubgroupsOfGL(2,q) constructs the irreducible subgroups of GL(2,q), q=p^r, p>=5 prime, up to conjugacy and RewriteAbsolutelyIrreducibleMatrixGroup(G) rewrites the absolutely irreducible matrix group G (over a finite field) over a minimal subfield.",

PackageDoc := rec(
  BookName  := "Cubefree",
  ArchiveURLSubset := ["doc", "htm"],
  HTMLStart := "htm/chapters.htm",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Constructing the groups of a given cubefree order",
),

Dependencies := rec(
  GAP := ">=4.9",
  NeededOtherPackages := [
    ["GrpConst","2.5"],
    ["Polycyclic","2.11"],
    ["smallgrp","1.3"],
    ],
  SuggestedOtherPackages := [],
  ExternalConditions := [] ),

AvailabilityTest := ReturnTrue,
TestFile := "tst/testall.g",
Keywords := ["cubefree","construction of groups","irreducible matrix subgroups of degree 2"]

));
