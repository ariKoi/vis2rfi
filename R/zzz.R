# As per "When you do need side-effects" section in
# http://r-pkgs.had.co.nz/r.html
.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    cat(
        cat("\n"),
        cat(crayon::white$bgBlue$bold('VV') %+% "     " %+% crayon::white$bgBlue$bold('VV') # row1
            %+% "   " %+% crayon::white$bgBlue$bold('II')
            %+% "      " %+% crayon::white$bgBlue$bold('SSSS')
            %+% "   " %+% crayon::white$bgBlue$bold('2222')
            %+% "   " %+% crayon::white$bgBlue$bold('RRRR')
            %+% "   " %+% crayon::white$bgBlue$bold('FFF')
            %+% "   " %+% crayon::white$bgBlue$bold('II\n')),
        cat(" " %+% crayon::white$bgBlue$bold('VV') %+% "   " %+% crayon::white$bgBlue$bold('VV') # row2
            %+% "    " %+% crayon::white$bgBlue$bold('II')
            %+% "    " %+% crayon::white$bgBlue$bold('SSS')
            %+% "        " %+% crayon::white$bgBlue$bold('22')
            %+% "   " %+% crayon::white$bgBlue$bold('R') %+% "  " %+% crayon::white$bgBlue$bold('R')
            %+% "   " %+% crayon::white$bgBlue$bold('F')
            %+% "     " %+% crayon::white$bgBlue$bold('II\n')),
        cat("  " %+% crayon::white$bgBlue$bold('VV') %+% " " %+% crayon::white$bgBlue$bold('VV') # row3
            %+% "     " %+% crayon::white$bgBlue$bold('II')
            %+% "      " %+% crayon::white$bgBlue$bold('SSS')
            %+% "    " %+% crayon::white$bgBlue$bold('22')
            %+% "     " %+% crayon::white$bgBlue$bold('RRR')
            %+% "    " %+% crayon::white$bgBlue$bold('FFF')
            %+% "   " %+% crayon::white$bgBlue$bold('II\n')),
        cat("   " %+% crayon::white$bgBlue$bold('VVV') # row4
            %+% "      " %+% crayon::white$bgBlue$bold('II')
            %+% "    " %+% crayon::white$bgBlue$bold('SSSS')
            %+% "     " %+% crayon::white$bgBlue$bold('2222')
            %+% "   " %+% crayon::white$bgBlue$bold('R') %+% "  " %+% crayon::white$bgBlue$bold('R')
            %+% "   " %+% crayon::white$bgBlue$bold('F')
            %+% "     " %+% crayon::white$bgBlue$bold('II\n')),
        cat("\n"),
    cat(font_style2("vis2rfi : Visualising 2Way Random Forest Interactions\n")),
    cat(font_style("A main theme of this package is to repeatedly explore the interaction effect of two variables (i.e. 2 way) after running the")), cat(font_styleRF(" Random Forests ")), cat(font_style("algorithm in supervised mode (currently binary classification is only tested). Experiment with the different subsampling approaches depending on the size of your data. As a reminder, make sure you have run function")), cat(font_style3(" forestFloor ")), cat(font_style("from the")), cat(font_style3(" forestFloor ")), cat(font_style("package on your data, as")), cat(font_style2(" vis2rfi ")), cat(font_style("is currently using the feature contributions to the prediction from this package."))))
}
