install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("parent_directory")
create("yarrr")

document()


install_github('yarrr_package', 'ndphillips')

setwd("..")
install("yarrr_package")
