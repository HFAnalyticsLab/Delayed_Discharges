
#install.packages("ggrepel")

#load packages

library(ggrepel)

library(tibble) # for results table data manipulation

library(tidyverse) # for data manipulation and plots 

library(dplyr) # for data manipulation

library(here) # used to get easier filepath references for objects not in Git

library(tidyr) # for data manipulation

library(janitor) # for data manipulation

library(abind) # to join arrays

#library(bcp) # for Barry and Hartigan change-point model

library(forecast) # time-series forecasting

library(lme4) # for GLMM

library(MASS) # for "Modern Applied Statistics with S" utilities

library(multcomp) # for multiple inferences

library(readxl) # for importing MS Excel spreadsheets

library(gsynth) # to use synthetic control methods

library(panelView) # to produce gsynth plots

library(parallel) # to check number of cores

library(forcats) # to relevel rows for forest plot

pre.post_key.vec.ls <- list(pre=rev(-seq.int(24)), 
                            post=seq.int(24)
)  # Set list by study period of key intervention times