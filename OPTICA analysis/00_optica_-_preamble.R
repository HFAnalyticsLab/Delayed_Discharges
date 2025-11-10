###########################################
##                                       ##
## Analysis of delayed discharge         ##
## statistics across acute NHS trusts    ##
## in England by OPTICA deployment       ##
##                                       ##
## ===================================== ##
##                                       ##
## Developer: Stefano Conti              ##
##            (e. stefano.conti@nhs.net) ##
##                                       ##
###########################################


##############
## Preamble ##
##############

rm(list = ls())  # Clear work-space

graphics.off()  # Shut all graphical devices

needed_package_names.vec <- c("abind")  # Set character vector of required R libraries

installed_package_log.vec <- sapply(needed_package_names.vec, 
                                    FUN = require, character.only = TRUE, quietly = TRUE
                                    )  # Derive logical vector of required installed R libraries

if(! all(installed_package_log.vec))
  {
  needed.missing_package_names.vec <- needed_package_names.vec[! installed_package_log.vec]  # Derive character vector of required missing R libraries
  
  install.packages(needed.missing_package_names.vec)  # Install required missing R libraries
  }

sapply(needed_package_names.vec, FUN = require, character.only = TRUE)  # Load required R libraries


old_par <- par(no.readonly = TRUE)  # Set list of graphical parameters

root.dir <- file.path("~/A/Non-IAU/Projects/Live/OPTICA")  # Set project root directory

data.dir <- file.path(root.dir, "Data")  # Set project data directory

output.dir <- file.path(root.dir, "R/Output")  # Set project output directory

dir.create(file.path(output.dir, 
                     "Graphs/Report"
                     ), 
           showWarnings = FALSE, recursive = TRUE
           )  # Set project report folder

lapply(file.path(output.dir, "Graphs", 
                 outer(c("QA", "Non-QA"), 
                       outer(c("Descriptive", "Inference"), 
                             outer(paste("Undischarged out of", 
                                         c("beds", "dischargeable"), 
                                         sep = " "
                                         ), 
                                   outer(c("Weekly", "Monthly"), 
                                         c(paste0(c("Non-", rep("", times = 2)), 
                                                  "OPTICA trusts", 
                                                  c("", paste0(" (", c("calendar", "lapsed"), " time)"))
                                                  ), 
                                           "Pooled trusts"
                                           ), 
                                         FUN = file.path
                                         ), 
                                   FUN = file.path
                                   ), 
                             FUN = file.path
                             ), 
                       FUN = file.path
                       )
                 ), 
       FUN = dir.create, showWarnings = FALSE, recursive = TRUE
       )  # Set project folder structure


# rate_scale <- 1e2  # Set multiplicative factor for rate calculations

res_scale <- 3e2  # Set resolution parameter for .png plots


setwd(root.dir)  # Set working directory