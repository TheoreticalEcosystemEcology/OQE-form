ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

## Install packages
pkgs = c('shiny','shinyjs','leaflet','leaflet.extra','rmarkdown','shinythemes')
ipak(pkgs)
