### ------------------------------------------------------------------------ ###
### use renv  ####
### ------------------------------------------------------------------------ ###
### replicate exact R package environment

### clone repository from GitHub and run:

### prepare renv package
install.packages("renv")
renv::activate()

### install required packages from lock file
renv::restore()

### ------------------------------------------------------------------------ ###
### manual installation of R packages ####
### ------------------------------------------------------------------------ ###
### use only if renv does not work


install.packages("remotes")
install.packages("ggplot2")
install.packages("doParallel")

remotes::install_github("fishfollower/SAM/stockassessment", 
                        INSTALL_opts = "--no-multiarch")

remotes::install_github("flr/FLCore", INSTALL_opts = "--no-multiarch")
remotes::install_github("flr/FLash", INSTALL_opts = "--no-multiarch")
remotes::install_github("flr/ggplotFL", INSTALL_opts = "--no-multiarch")
remotes::install_github("shfischer/FLfse/FLfse", INSTALL_opts = "--no-multiarch")

