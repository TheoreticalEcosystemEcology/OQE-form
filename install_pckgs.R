ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, repos="http://cran.rstudio.com/", dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

## Install packages
pkgs = c('devtools','shiny','shinyjs','rmarkdown','shinythemes')
ipak(pkgs)

## Install from GitHub
devtools::install_github('rstudio/leaflet')
devtools::install_github('bhaskarvk/leaflet.extras')

