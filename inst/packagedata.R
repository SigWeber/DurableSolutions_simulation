# Package building function
#
# This function pulls data from HDX and then tidy them a bit to be ready for R

# Check that the usethis package is also installed. If not:
#install.packages("usethis")
devtools::document()
devtools::check(document = FALSE)

#install.packages("pkgdown")

#library("pkgdown")
pkgdown::build_site()

## a few other exploration of the package
devtools::release()
# devtools::build_win(version = c("R-release", "R-devel"))

#install.packages("sinew")
#devtools::install_github("mdlincoln/docthis")
#library(readr)
#library(sinew)
#library(docthis)


hargeisa <- readxl::read_xlsx("data-raw/Hargeisa.xlsx", sheet = 3)
sinew::makeOxygen(hargeisa, add_fields = "source")
save(hargeisa, file =  "data/hargeisa.RData")

nigeria <- readr::read_csv("data-raw/Nigeria.csv")
sinew::makeOxygen(nigeria, add_fields = "source")
save(nigeria, file =  "data/nigeria.RData")


attachment::att_to_description()
rhub::check_for_cran()
rhub::check()


