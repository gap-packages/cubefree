#############################################################################
##
#W    init.g               share package 'Cubefree'            Heiko Dietrich
##                                                             

#############################################################################
##
#D Declare the package
##
DeclarePackage( "Cubefree", "1.03",
    function()

    if VERSION{[1,2,3]} <> "4.1" then
        return true;
    else
        Print("The versions of Cubefree and GAP4 you are using\n");
        Print("are not compatible. You need at least version 4.2 of\n");
        Print("GAP to work with this version of Cubefree.\n");
        return false;
    fi;

    end );
DeclarePackageDocumentation( "Cubefree", "doc" );


#############################################################################
##
#D Read .gd files
##

ReadPackage("cubefree/gap/cubefree.gd");

