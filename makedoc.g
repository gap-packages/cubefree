#############################################################################
##
##  makedoc.g
##
##  Builds the package documentation with AutoDoc/GAPDoc.
##
#############################################################################

LoadPackage("AutoDoc");

# Run this from the package's root directory: gap makedoc.g
AutoDoc(rec(
    autodoc := rec(scan_dirs := []),
    gapdoc := rec(main := "main", files := []),
    extract_examples := true,
    scaffold := rec(
        includes := [
            "intro.xml",
            "cubefree.xml",
            "install.xml"
        ],
        entities := rec(
            Cubefree := "<Package>Cubefree</Package>",
            GrpConst := "<Package>GrpConst</Package>",
            IrredSol := "<Package>IrredSol</Package>",
            SmallGroups := "<Package>SmallGroups</Package>",
        ),
        bib := "manual.bib",
    ),
));

QuitGap();
