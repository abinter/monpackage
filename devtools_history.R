devtools::use_package('magrittr')
devtools::use_package('survival')
devtools::use_package('stats')
devtools::use_package('RColorBrewer')
devtools::use_package('corrplot')
devtools::use_package('grid')
devtools::use_package('e1071')
devtools::use_vignettes()
devtools::build_vignettes()
usethis::use_build_ignore("devtools_history.R")
