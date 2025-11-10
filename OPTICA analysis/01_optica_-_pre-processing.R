###########################################
##                                       ##
## Analysis of OPTICA delayed discharges ##
## per occupied beds rates for Analysis  ##
##                                       ##
## Pre-process, QA and inspect raw data  ##
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

source("~/A/Non-IAU/Projects/Live/OPTICA/R/scripts/00_optica_-_preamble.r")  # Source analysis preamble script


#########################
## Pre-processing data ##
#########################

discharge.bed_d.dat <- read.table(file.path(data.dir, "discharge_sitrep_table2withdenominator.csv"), 
                                  header = TRUE, sep = ",", quote = "\"", na.strings = c("NA", "NULL", "-"), 
                                  row.names = NULL, 
                                  col.names = c("trust_code", "status", "freq_discharge", "test1", "date", 
                                                paste0("test", 2:3), "freq_bed", paste0("test", 4:6)
                                                ), 
                                  colClasses = c("character", "factor", "integer", "NULL", "character", 
                                                 rep("NULL", times = 2), "integer", rep("NULL", times = 3)
                                                 )  # Load daily delayed discharge and monthly bed occupancy sit-rep data-frame
                                  )  # Load daily delayed discharge and monthly bed occupancy sit-rep data-frame

discharge.bed_d.dat <- within(discharge.bed_d.dat, 
                              expr = 
                                {
                                  levels(status) <- c(paste0(c("", "un"), "discharged"), "dischargeable")  # Rename "status" factor levels
                                  
                                  status <- relevel(status, ref = "dischargeable")  # Re-set baseline "status" factor level
                                  
                                  date <- as.Date(date, format = "%d/%m/%Y")  # Format "date" character variable as date
                                  
                                  date_wkyr <- format(date, format = "%W-%y")  # Format "date" date as 'wk-yr' character variable
                                  
                                  date_moyr <- format(date, format = "%b %y")  # Format "date" date as 'mo yr' character variable
                                }
                              )  # Pre-process daily delayed discharge and monthly bed occupancy sit-rep data-frame


discharge.bed_wk.dat <- aggregate(cbind(freq_discharge, freq_bed) ~ date_wkyr + trust_code + status, 
                                  data = discharge.bed_d.dat, FUN = sum
                                  )  # Derive weekly delayed discharge and monthly bed occupancy sit-rep and monthly bed occupancy data-frame


discharge.bed_mo.dat <- aggregate(cbind(freq_discharge, freq_bed) ~ date_moyr + trust_code + status, 
                                  data = discharge.bed_d.dat, FUN = sum
                                  )  # Derive monthly delayed discharge and monthly bed occupancy sit-rep data-frame


optica_status.dat <- read.table(file.path(data.dir, "OPTICA_trusts.csv"), 
                                header = TRUE, sep = ",", quote = "\"", na.strings = c("NA", "NULL", "-"), 
                                row.names = NULL, col.names = c("trust_name", "start"), 
                                colClasses = rep("character", times = 2)
                                )  # Load OPTICA status data-frame

optica_status.dat <- within(optica_status.dat, 
                            expr = 
                              {
                                trust_code <- c("RGN", "R1F", "RD1", "RWG", "RH5", "RJL", "RCB", "RXF", "RGP", 
                                                "RCD", "RCX", "RM1", "RTD", "RFS", "RTG", "RWA", "RXP", "R0B", 
                                                "RJR", "R1K", "RAS", "REM", "RYJ", "RQM", "RVW"
                                                )  # Set "trust_code" character variable
                                
                                start <- as.Date(start, format = "%d/%m/%Y")  # Format "start" character variable as date
                                
                                start_wkyr <- format(start, format = "%W-%y")  # Format "start" date as 'wk-yr' character variable
                                
                                start_moyr <- format(start, format = "%b %y")  # Format "start" date as 'mo yr' character variable
                                
                                rm(trust_name)  # Discard redundant variables
                                }
                            )  # Pre-process OPTICA status data-frame

save(optica_status.dat, 
     file = file.path(data.dir, "optica_status.RData")
     )  # Save OPTICA status data-frame in .RData format


discharge.bed.optica_wk.dat <- merge(discharge.bed_wk.dat, optica_status.dat, 
                                     by = "trust_code", all.x = TRUE
                                     )  # Merge weekly delayed discharge and monthly bed occupancy sit-rep and OPTICA status data-frames

discharge.bed.optica_wk.dat <- within(discharge.bed.optica_wk.dat, 
                                      expr = 
                                        {
                                          comparator <- ifelse(is.na(start_wkyr), "Without OPTICA", "With OPTICA")  # Derive "comparator" character variable
                                          
                                          date <- as.Date(paste("1", date_wkyr, sep = "-"), 
                                                          format = "%u-%W-%y"
                                          )  # Derive "date" date variable
                                        }
                                      )  # Pre-process weekly delayed discharge and monthly bed occupancy sit-rep augmented data-frame

discharge.bed.optica_wk.dat <- discharge.bed.optica_wk.dat[order(discharge.bed.optica_wk.dat$date), ]  # Sort by "date" weekly delayed discharge and monthly bed occupancy sit-rep augmented data-frame

discharge.bed.optica_wk.dat <- rbind(discharge.bed.optica_wk.dat[c("date_wkyr", "trust_code", "status", "comparator", 
                                                                   paste("freq", c("discharge", "bed"), sep = "_"))], 
                                     data.frame(trust_code = "pooled", 
                                                aggregate(cbind(freq_discharge, freq_bed) ~ date_wkyr + status + comparator, 
                                                          data = discharge.bed.optica_wk.dat, 
                                                          FUN = sum
                                                          )
                                                )  # Derive 'Pooled' "trust_code" weekly delayed discharge and monthly bed occupancy sit-rep augmented data-frame
                                     )  # Augment with 'Pooled' "icb_code", 'Pooled' "trust_code" weekly delayed discharge and monthly bed occupancy sit-rep augmented data-frame


discharge.bed.optica_mo.dat <- merge(discharge.bed_mo.dat, optica_status.dat, 
                                     by = "trust_code", all.x = TRUE
                                     )  # Merge monthly delayed discharge and monthly bed occupancy sit-rep and OPTICA status data-frames

discharge.bed.optica_mo.dat <- within(discharge.bed.optica_mo.dat, 
                                      expr = 
                                        {
                                          comparator <- ifelse(is.na(start_moyr), "Without OPTICA", "With OPTICA")  # Derive "comparator" character variable
                                          
                                          date <- as.Date(paste("01", date_moyr, sep = " "), 
                                                          format = "%d %b %y"
                                                          )  # Derive "date" date variable
                                          }
                                      )  # Pre-process monthly delayed discharge and monthly bed occupancy sit-rep augmented data-frame

discharge.bed.optica_mo.dat <- discharge.bed.optica_mo.dat[order(discharge.bed.optica_mo.dat$date), ]  # Sort by "date" monthly delayed discharge and monthly bed occupancy sit-rep augmented data-frame

discharge.bed.optica_mo.dat <- rbind(discharge.bed.optica_mo.dat[c("date_moyr", "trust_code", "status", "comparator", 
                                                                   paste("freq", c("discharge", "bed"), sep = "_"))], 
                                     data.frame(trust_code = "pooled", 
                                                aggregate(cbind(freq_discharge, freq_bed) ~ date_moyr + status + comparator, 
                                                          data = discharge.bed.optica_mo.dat, 
                                                          FUN = sum
                                                          )
                                                )  # Derive 'Pooled' "trust_code" monthly delayed discharge and monthly bed occupancy sit-rep augmented data-frame
                                     )  # Augment with 'Pooled' "icb_code", 'Pooled' "trust_code" monthly delayed discharge and monthly bed occupancy sit-rep augmented data-frame


discharge.bed.optica.dat.ls <- list(wk = discharge.bed.optica_wk.dat, 
                                    mo = discharge.bed.optica_mo.dat
                                    )  # Set list by "lag" of delayed discharge augmented sit-rep data-frames


freq_noqa.arr.ls <- sapply(names(discharge.bed.optica.dat.ls), 
                           FUN = function(lg)
                           {
                             discharge.bed_freq.frm <- as.formula(paste("cbind(freq_discharge, freq_bed)", 
                                                                        paste(c(paste0("date_", lg, "yr"), 
                                                                                "trust_code", "status", "comparator"
                                                                                ), 
                                                                              collapse = " + "
                                                                              ), 
                                                                        sep = " ~ "
                                                                        )
                                                                  )  # Set formula to derive delayed discharge contingency table
                             
                             discharge.bed_freq.arr <- xtabs(discharge.bed_freq.frm, 
                                                             data = discharge.bed.optica.dat.ls[[lg]]
                                                             )  # Format delayed discharge contingency table as 5d-array by "date_lgyr", "trust_code", "status", "comparator", "statistic"
                             
                             names(dimnames(discharge.bed_freq.arr))[names(dimnames(discharge.bed_freq.arr)) == ""] <- "statistic"  # Rename blank "statistic" margin of delayed discharge frequencies 5d-array
                             
                             discharge.bed_freq.arr <- discharge.bed_freq.arr[unique(discharge.bed.optica.dat.ls[[lg]][[paste0("date_", lg, "yr")]]), 
                                                                              , , , ]  # Sort delayed discharge frequencies 5d-array by "date_lgyr"
                             
                             return(discharge.bed_freq.arr)  # Return as output delayed discharge 5d-array by "date_lgyr", "trust_code", "status", "comparator", "statistic"
                             }, 
                           simplify = FALSE
                           )  # Derive list by "lag" of delayed discharge 5d-arrays by "date_lgyr", "trust_code", "status", "comparator", "statistic"


freq_noqa.dat.ls <- sapply(names(freq_noqa.arr.ls), 
                           FUN = function(lg) 
                             as.data.frame.table(freq_noqa.arr.ls[[lg]], 
                                                 responseName = "freq"
                                                 ),   # Format delayed discharge frequencies 5d-array as data-frame
                           simplify = FALSE
                           )  # Derive list by "lag" of delayed discharge frequencies data-frames


out_noqa.arr.ls <- sapply(names(freq_noqa.arr.ls), 
                          FUN = function(lg)
                            {
                            out_noqa.arr <- 1e2 * abind(prop_bed = freq_noqa.arr.ls[[lg]][, , "undischarged", , "freq_discharge"] / 
                                                          freq_noqa.arr.ls[[lg]][, , "dischargeable", , "freq_bed"],   # Derive beds undischarged proportions (%) 3d-array by "date_lgyr", "trust_code", "comparator"
                                                        prop_discharge = freq_noqa.arr.ls[[lg]][, , "undischarged", , "freq_discharge"] / 
                                                          freq_noqa.arr.ls[[lg]][, , "dischargeable", , "freq_discharge"],   # Derive dischargeable undischarged proportions (%) 3d-array by "date_lgyr", "trust_code", "comparator"
                                                        along = 4, 
                                                        use.dnns = TRUE
                                                        )  # Derive 4d-array by "date_lgyr", "trust_code", "comparator", "statistic" of delayed discharge outcomes
                            
                            names(dimnames(out_noqa.arr))[names(dimnames(out_noqa.arr)) == ""] <- "statistic"  # Rename blank "statistic" margin of delayed discharge outcomes 4d-array
                            
                            return(out_noqa.arr)  # Return as output delayed discharge outcomes 4d-array by "date_lgyr", "trust_code", "comparator", "statistic"
                            }, 
                          simplify = FALSE
                          )  # Derive list by "lag" of delayed discharge outcomes 4d-arrays by "date_lgyr", "trust_code", "comparator", "statistic"


out_noqa.dat.ls <- sapply(names(out_noqa.arr.ls), 
                          FUN = function(lg) 
                            as.data.frame.table(out_noqa.arr.ls[[lg]], 
                                                responseName = "prop"
                                                ),   # Format delayed discharge outcomes 4d-array as data-frame
                          simplify = FALSE
                          )  # Derive list by "lag" of delayed discharge outcomes data-frames


save(list = outer(c("freq", "out"), 
                  c("arr", "dat"), 
                  FUN = function(typ, ext) 
                    paste0(typ, "_noqa.", ext, ".ls")
                  ), 
     file = file.path(data.dir, 
                      "freq.out_noqa.RData"
                      )
     )  # Save delayed discharge frequencies and outcomes arrays and data-frames lists in .RData format


#######################
## Produce QAed      ##
## frequency and     ##
## outcome data-sets ##
#######################

## Details on trust data explorations are outlined on tHF's SharePoint 
## in 11-CAT --> 1. Work programme --> Discharge delays --> OPTICA --> 
## Data --> trust mergers acquisitions.txt

trust_merger.dat <- data.frame(acquiring = c("RH5", "RAL", "RBN"), 
                               acquired = c("RA4", "RAP", "RVY")
                               )  # Set data-frame of trust mergers and acquisitions


freq_qa.arr.ls <- sapply(names(freq_noqa.arr.ls), 
                         FUN = function(lg)
                           {
                           freq_qa.arr <- freq_noqa.arr.ls[[lg]]  # Initialise QAed delayed discharge frequencies 5d-array by "date_lgyr", "trust_code", "status", "comparator", "statistic"
                           
                           for(merger in seq_along(trust_merger.dat$acquiring)) 
                             freq_qa.arr[, trust_merger.dat$acquiring[merger], , , ] <- freq_qa.arr[, trust_merger.dat$acquiring[merger], , , ] + 
                               ifelse(is.na(freq_qa.arr[, trust_merger.dat$acquired[merger], , , ]), 
                                      0, freq_qa.arr[, trust_merger.dat$acquired[merger], , , ]
                                      )  # Add in QAed discharge frequencies 5d-array 'acquired' "trust_code" to 'acquiring' "trust_code" frequencies from trust mergers and acquisitions data-frame
                           
                           freq_qa.arr <- freq_qa.arr[, setdiff(dimnames(freq_qa.arr)$trust_code, 
                                                                y = c("pooled", "RA2", "RJN", "RKB", "RPA", 
                                                                      "RWD", "RXR", trust_merger.dat$acquired
                                                                      )
                                                                ), , , ]  # Discard poor quality data and acquired trusts from delayed discharge frequencies 5d-array
                           
                           freq_qa.arr <- abind(freq_qa.arr, 
                                                pooled = apply(freq_qa.arr, 
                                                               MARGIN = c(1, 3:5), 
                                                               FUN = sum, na.rm = TRUE
                                                               ), 
                                                along = 2, 
                                                use.dnns = TRUE
                                                )  # Derive 'Pooled' "trust_code' margin to QAed delayed discharge frequencies 5d-array by "date_lgyr", "trust_code", "status", "comparator", "statistic"
                           
                           return(freq_qa.arr)  # Return as output QAed delayed discharge frequencies 5d-array by "date_lgyr", "trust_code", "status", "comparator", "statistic"
                           }, 
                         simplify = FALSE
                         )  # Derive list by "lag" of QAed delayed discharge frequencies 5d-arrays by "date_lgyr", "trust_code", "status", "comparator", "statistic"


freq_qa.dat.ls <- sapply(names(freq_qa.arr.ls), 
                         FUN = function(lg) 
                           as.data.frame.table(freq_qa.arr.ls[[lg]], 
                                               responseName = "freq"
                                               ), 
                         simplify = FALSE
                         )  # Derive list by "lag" of QAed delayed discharge frequencies data-frames


out_qa.arr.ls <- sapply(names(freq_qa.arr.ls), 
                        FUN = function(lg)
                          {
                          out_qa.arr <- 1e2 * abind(prop_bed = freq_qa.arr.ls[[lg]][, , "undischarged", , "freq_discharge"] / 
                                                      freq_qa.arr.ls[[lg]][, , "dischargeable", , "freq_bed"],   # Derive QAed beds undischarged proportions (%) 3d-array by "date_lgyr", "trust_code", "comparator"
                                                    prop_discharge = freq_qa.arr.ls[[lg]][, , "undischarged", , "freq_discharge"] / 
                                                      freq_qa.arr.ls[[lg]][, , "dischargeable", , "freq_discharge"],   # Derive QAed dischargeable undischarged proportions (%) 3d-array by "date_lgyr", "trust_code", "comparator"
                                                    along = 4, 
                                                    use.dnns = TRUE
                                                    )  # Derive 4d-array by "date_lgyr", "trust_code", "comparator", "statistic" of QAed delayed discharge outcomes
                          
                          names(dimnames(out_qa.arr))[names(dimnames(out_qa.arr)) == ""] <- "statistic"  # Rename blank "statistic" margin of QAed delayed discharge outcomes 4d-array
                          
                          return(out_qa.arr)  # Return as output QAed delayed discharge outcomes 4d-array by "date_lgyr", "trust_code", "comparator", "statistic"
                          }, 
                        simplify = FALSE
                        )  # Derive list by "lag" of QAed delayed discharge outcomes 4d-arrays by "date_lgyr", "trust_code", "comparator", "statistic"


out_qa.dat.ls <- sapply(names(out_qa.arr.ls), 
                        FUN = function(lg) 
                          as.data.frame.table(out_qa.arr.ls[[lg]], 
                                              responseName = "prop"
                                              ),   # Format QAed delayed discharge outcomes 4d-array as data-frame
                        simplify = FALSE
                        )  # Derive list by "lag" of QAed delayed discharge outcomes data-frames


save(list = outer(c("freq", "out"), 
                  c("arr", "dat"), 
                  FUN = function(typ, ext) 
                    paste0(typ, "_qa.", ext, ".ls")
                  ), 
     file = file.path(data.dir, "freq.out_qa.RData")
     )  # Save QAed delayed discharge frequencies and outcomes arrays and data-frames lists in .RData format


#########################################
## Stagger delayed discharge outcomes  ##
## relative to lapsed time from OPTICA ##
## across OPTICA NHS trusts            ##
#########################################

optica_time.vec.ls2 <- sapply(names(out_qa.arr.ls), 
                              FUN = function(lg) 
                                list(start = sapply(setNames(optica_status.dat[[paste0("start_", lg, "yr")]], 
                                                             nm = optica_status.dat$trust_code
                                                             ), 
                                                    FUN = function(lgyr) 
                                                      which(lgyr == dimnames(out_qa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]])
                                                    ),   # Derive vector by "trust_code" of intervention start points
                                
                                total = length(dimnames(out_qa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]])
                                ),   # Append to intervention start week-points vector study length
                              simplify = FALSE
                              )  # Derive list by "lag" of lists by "timing" of study design


freq_pre_stg_qa.arr.ls2 <- sapply(names(freq_qa.arr.ls), 
                                  FUN = function(lg) 
                                    sapply(names(optica_time.vec.ls2[[lg]][["start"]]), 
                                           FUN = function(trust)
                                             {
                                             freq_pre_stg_qa.arr <- with(optica_time.vec.ls2[[lg]], 
                                                                         expr = 
                                                                           if(start[trust] == 1) 
                                                                             array(NA, 
                                                                                   dim = c(1, 3, 2), 
                                                                                   dimnames = setNames(list(NULL, 
                                                                                                            dimnames(freq_qa.arr.ls[[lg]])$status, 
                                                                                                            dimnames(freq_qa.arr.ls[[lg]])$statistic
                                                                                                            ), 
                                                                                                       nm = c(paste("start", lg, sep = "_"), 
                                                                                                              "status", "statistic")
                                                                                                       )
                                                                                   ) else 
                                                                                     apply(freq_qa.arr.ls[[lg]][, trust, , "With OPTICA", ], 
                                                                                           MARGIN = 2:3, 
                                                                                           FUN = function(vec) 
                                                                                             rev(vec[seq.int(start[trust] - 1)])
                                                                                           )
                                                                         )  # Derive 3d-array by "start_lg", "status" , "statistic" of QAed staggered pre-intervention delayed discharge frequencies
                                             
                                             names(dimnames(freq_pre_stg_qa.arr))[names(dimnames(freq_pre_stg_qa.arr)) == ""] <- paste("start", lg, sep = "_")  # Rename blank "start_lg" margin of QAed staggered pre-intervention delayed discharge frequencies 3d-array
                                             
                                             return(freq_pre_stg_qa.arr)  # Return as output staggered pre-intervention delayed discharge frequencies 3d-array by "start_lg", "status", "statistic"
                                             }, 
                                           simplify = FALSE
                                           ), 
                                  simplify = FALSE
                                  )  # Derive list by "lag" of lists by "trust_code" of 3d-arrays by "start_lg", "status", "statistic" of QAed staggered pre-intervention delayed discharge frequencies


freq_pre_stg_qa.arr.ls <- sapply(names(freq_pre_stg_qa.arr.ls2), 
                                 FUN = function(lg)
                                   {
                                   freq_pre_stg_qa.arr <- sapply(names(freq_pre_stg_qa.arr.ls2[[lg]]), 
                                                                 FUN = function(trust) 
                                                                   apply(freq_pre_stg_qa.arr.ls2[[lg]][[trust]], 
                                                                         MARGIN = 2:3, 
                                                                         FUN = "[", 
                                                                         i = seq.int(max(optica_time.vec.ls2[[lg]]$start) - 1)
                                                                         ), 
                                                                 simplify = "array"
                                                                 )  # Derive 4d-array by "start_lg", "status", "statistic", "trust_code" of QAed staggered pre-intervention delayed discharge frequencies
                                   
                                   freq_pre_stg_qa.arr <- aperm(freq_pre_stg_qa.arr, 
                                                                perm = c(1, 4, 2:3)
                                                                )  # Permute margins of QAed staggered pre-intervention delayed discharge frequencies 4d-array by "start_lg", "trust_code", "status", "statistic"
                                   
                                   names(dimnames(freq_pre_stg_qa.arr))[names(dimnames(freq_pre_stg_qa.arr)) == ""] <- "trust_code"  # Rename blank "start_lg" margin of QAed staggered pre-intervention delayed discharge frequencies 4d-array
                                   
                                   dimnames(freq_pre_stg_qa.arr)[[paste("start", lg, sep = "_")]] <- - seq.int(max(optica_time.vec.ls2[[lg]]$start) - 1)  # Rename "start_lgyr" labels of QAed staggered pre-intervention delayed discharge frequencies 4d-array
                                   
                                   return(freq_pre_stg_qa.arr)  # Return as output 4d-array by "start_lg", "trust_code", "status", "statistic" of QAed staggered pre-intervention delayed discharge frequencies
                                   }, 
                                 simplify = FALSE
                                 )  # Derive list by "lag" of 4d-arrays by "start_lg", "trust_code", "status", "statistic" of QAed staggered pre-intervention delayed discharge frequencies


freq_post_stg_qa.arr.ls2 <- sapply(names(freq_qa.arr.ls), 
                                   FUN = function(lg) 
                                     sapply(names(optica_time.vec.ls2[[lg]][["start"]]), 
                                            FUN = function(trust)
                                              {
                                              freq_post_stg_qa.arr <- with(optica_time.vec.ls2[[lg]], 
                                                                           expr = 
                                                                             # if(start[trust] == 1) 
                                                                             #   array(NA, 
                                                                             #         dim = c(1, 3, 2), 
                                                                             #         dimnames = setNames(list(NULL, 
                                                                             #                                  dimnames(freq_qa.arr.ls[[lg]])$status, 
                                                                             #                                  dimnames(freq_qa.arr.ls[[lg]])$statistic
                                                                             #                                  ), 
                                                                             #                             nm = c(paste("start", lg, sep = "_"), 
                                                                             #                                    "status", "statistic")
                                                                             #                             )
                                                                             #         ) else 
                                                                             apply(freq_qa.arr.ls[[lg]][, trust, , "With OPTICA", ], 
                                                                                   MARGIN = 2:3, 
                                                                                   FUN = "[", i = start[trust]:total
                                                                                   )   # Derive 3d-array by "start_lg", "status", "statistic" of QAed staggered post-intervention delayed discharge frequencies
                                                                           )
                                              
                                              names(dimnames(freq_post_stg_qa.arr))[names(dimnames(freq_post_stg_qa.arr)) == ""] <- paste("start", lg, sep = "_")  # Rename blank "start_lg" margin of QAed staggered post-intervention delayed discharge frequencies 3d-array
                                              
                                              return(freq_post_stg_qa.arr)  # Return as output staggered post-intervention delayed discharge frequencies 3d-array by "start_lg", "status", "statistic"
                                              }, 
                                            simplify = FALSE
                                            ), 
                                   simplify = FALSE
                                   )  # Derive list by "lag" of lists by "trust_code" of 3d-arrays by "start_lg", "status", "statistic" of QAed staggered post-intervention delayed discharge frequencies


freq_post_stg_qa.arr.ls <- sapply(names(freq_post_stg_qa.arr.ls2), 
                                  FUN = function(lg)
                                    {
                                    freq_post_stg_qa.arr <- sapply(names(freq_post_stg_qa.arr.ls2[[lg]]), 
                                                                   FUN = function(trust) 
                                                                     apply(freq_post_stg_qa.arr.ls2[[lg]][[trust]], 
                                                                           MARGIN = 2:3, 
                                                                           FUN = "[", 
                                                                           i = with(optica_time.vec.ls2[[lg]], 
                                                                                    expr = seq.int(total - min(start) + 1)
                                                                                    )
                                                                           ), 
                                                                   simplify = "array"
                                                                   )  # Derive 4d-array by "start_lg", "status", "statistic", "trust_code" of QAed staggered post-intervention delayed discharge frequencies
                                    
                                    freq_post_stg_qa.arr <- aperm(freq_post_stg_qa.arr, 
                                                                  perm = c(1, 4, 2:3)
                                                                  )  # Permute margins of QAed staggered post-intervention delayed discharge frequencies 4d-array by "start_lg", "trust_code", "status", "statistic"
                                    
                                    names(dimnames(freq_post_stg_qa.arr))[names(dimnames(freq_post_stg_qa.arr)) == ""] <- c(paste("start", lg, sep = "_"), 
                                                                                                                            "trust_code"
                                                                                                                            )  # Rename blank "start_lg", "trust_code" margins of QAed staggered post-intervention delayed discharge frequencies 4d-array
                                    
                                    dimnames(freq_post_stg_qa.arr)[[paste("start", lg, sep = "_")]] <- with(optica_time.vec.ls2[[lg]], 
                                                                                                            expr = seq_along(min(start):total)
                                                                                                            )  # Rename "start_lg" labels of QAed staggered post-intervention delayed discharge frequencies 4d-array
                                    
                                    return(freq_post_stg_qa.arr)  # Return as output 4d-array by "start_lg", "trust_code", "status", "statistic" of QAed staggered post-intervention delayed discharge frequencies
                                    }, 
                                  simplify = FALSE
                                  )  # Derive list by "lag" of 4d-arrays by "start_lg", "trust_code", "status", "statistic" of QAed staggered post-intervention delayed discharge frequencies


freq_stg_qa.arr.ls <- sapply(names(freq_qa.arr.ls), 
                             FUN = function(lg)
                               {
                               freq_qa.arr <- abind(freq_pre_stg_qa.arr.ls[[lg]][nrow(freq_pre_stg_qa.arr.ls[[lg]]):1, , , ], 
                                                    freq_post_stg_qa.arr.ls[[lg]], 
                                                    along = 1, 
                                                    use.dnns = TRUE
                                                    )  # Bind by row across "period" 4d-arrays by "start_lg", "trust_code", "status", "statistic" of QAed staggered delayed discharge frequencies
                               
                               freq_qa.arr <- abind(freq_qa.arr, 
                                                    pooled = apply(freq_qa.arr, 
                                                                   MARGIN = c(1, 3:4), 
                                                                   FUN = sum, na.rm = TRUE
                                                                   ), 
                                                    along = 2, 
                                                    use.dnns = TRUE
                                                    )  # Append 'Pooled' "trust_code' margin to QAed staggered delayed discharge frequencies 4d-array by "start_lg", "trust_code", "status", "statistic"
                               
                               return(freq_qa.arr)  # Return as output QAed staggered delayed discharge frequencies 4d-array by "start_lg", "trust_code", "status", "statistic"
                               }, 
                             simplify = FALSE
                             )  # Derive list by "lag" of 4d-arrays by "date_lg", "trust_code", "status", "statistic" of QAed staggered delayed discharge frequencies


freq_stg_qa.dat.ls <- sapply(names(freq_stg_qa.arr.ls), 
                             FUN = function(lg) 
                               as.data.frame.table(freq_stg_qa.arr.ls[[lg]], 
                                                   responseName = "prop"
                                                   ),   # Format QAed staggered delayed discharge frequencies 4d-array as data-frame
                             simplify = FALSE
                             )  # Derive list by "lag" of QAed staggered delayed discharge frequencies data-frames


out_stg_qa.arr.ls <- sapply(names(freq_stg_qa.arr.ls), 
                            FUN = function(lg)
                              {
                              out_stg_qa.arr <- 1e2 * abind(prop_bed = freq_stg_qa.arr.ls[[lg]][, , "undischarged", "freq_discharge"] / 
                                                              freq_stg_qa.arr.ls[[lg]][, , "dischargeable", "freq_bed"],   # Derive QAed staggered beds undischarged proportions (%) 2d-array by "date_lgyr", "trust_code"
                                                            prop_discharge = freq_stg_qa.arr.ls[[lg]][, , "undischarged", "freq_discharge"] / 
                                                              freq_stg_qa.arr.ls[[lg]][, , "dischargeable", "freq_discharge"],   # Derive QAed staggered dischargeable undischarged proportions (%) 2d-array by "date_lgyr", "trust_code"
                                                            along = 3, 
                                                            use.dnns = TRUE
                                                            )  # Derive 3d-array by "date_lgyr", "trust_code", "statistic" of QAed staggered delayed discharge frequencies
                              
                              names(dimnames(out_stg_qa.arr))[names(dimnames(out_stg_qa.arr)) == ""] <- "statistic"  # Rename blank "statistic" margin of QAed staggered delayed discharge outcomes 3d-array
                              
                              return(out_stg_qa.arr)  # Return as output QAed staggered delayed discharge outcomes 3d-array by "date_lgyr", "trust_code", "statistic"
                              }, 
                            simplify = FALSE
                            )  # Derive list by "lag" of QAed staggered delayed discharge outcomes 3d-arrays by "date_lgyr", "trust_code", "statistic"


out_stg_qa.dat.ls <- sapply(names(out_stg_qa.arr.ls), 
                            FUN = function(lg) 
                              as.data.frame.table(out_stg_qa.arr.ls[[lg]], 
                                                  responseName = "prop"
                                                  ),   # Format QAed staggered delayed discharge outcomes 3d-array as data-frame
                            simplify = FALSE
                            )  # Derive list by "lag" of QAed staggered delayed discharge outcomes data-frames


save(list = outer(c("freq", "out"), 
                  c("arr", "dat"), 
                  FUN = function(typ, ext) 
                    paste0(typ, "_stg_qa.", ext, ".ls")
                  ), 
     file = file.path(data.dir, 
                      "freq.out_stg_qa.RData"
                      )
     )  # Save QAed staggered delayed discharge frequencies and outcomes arrays and data-frames lists in .RData format


## Stagger delayed discharge outcomes 
## pooled across NHS trusts with 1yr
## pre- and post-OPTICA deployment data

freq_1yrprepost_stg_qa.arr.ls <- sapply(names(freq_stg_qa.arr.ls), 
                                        FUN = function(lg)
                                          {
                                          freq_1yrprepost_stg_qa.arr <- freq_stg_qa.arr.ls[[lg]][, subset(optica_status.dat, 
                                                                                                          subset = (start >= "2023-04-01") & 
                                                                                                            (start <= "2024-08-01"), 
                                                                                                          select = "trust_code", 
                                                                                                          drop = TRUE
                                                                                                          ), , ]  # Subset QAed staggered delayed discharge frequencies 4d-array to 'With OPTICA' "trust_code" with 1yr pre- and post-OPTICA roll-out data
                                          
                                          freq_1yrprepost_stg_qa.arr <- abind(freq_1yrprepost_stg_qa.arr, 
                                                                              pooled = apply(freq_1yrprepost_stg_qa.arr, 
                                                                                             MARGIN = c(1, 3:4), 
                                                                                             FUN = sum, na.rm = TRUE
                                                                                             ), 
                                                                              along = 2, 
                                                                              use.dnns = TRUE
                                                                              )  # Append 'Pooled' "trust_code' margin to QAed staggered restricted delayed discharge frequencies 4d-array by "date_lgyr", "trust_code", "status", "statistic"
                                          
                                          return(freq_1yrprepost_stg_qa.arr)  # Return as output QAed staggered restricted delayed discharge frequencies 4d-array by "date_lgyr", "trust_code", "status", "statistic"
                                          }, 
                                        simplify = FALSE
                                        )  # Derive list by "lag" of QAed staggered delayed discharge frequencies 4d-arrays by "date_lgyr", "trust_code", "status", "statistic" subset to 'With OPTICA' "trust_code" with 1yr pre- and post-OPTICA roll-out data


freq_1yrprepost_stg_qa.dat.ls <- sapply(names(freq_1yrprepost_stg_qa.arr.ls), 
                                        FUN = function(lg) 
                                          as.data.frame.table(freq_1yrprepost_stg_qa.arr.ls[[lg]], 
                                                              responseName = "freq"
                                                              ), 
                                        simplify = FALSE
                                        )  # Derive list by "lag" of QAed staggered restricted delayed discharge frequencies data-frames

out_1yrprepost_stg_qa.arr.ls <- sapply(names(freq_1yrprepost_stg_qa.arr.ls), 
                                       FUN = function(lg)
                                         {
                                         out_1yrprepost_stg_qa.arr <- 1e2 * abind(prop_bed = freq_1yrprepost_stg_qa.arr.ls[[lg]][, , "undischarged", "freq_discharge"] / 
                                                                                    freq_1yrprepost_stg_qa.arr.ls[[lg]][, , "dischargeable", "freq_bed"],   # Derive QAed beds undischarged proportions (%) 3d-array by "date_lgyr", "trust_code", "comparator"
                                                                                  prop_discharge = freq_1yrprepost_stg_qa.arr.ls[[lg]][, , "undischarged", "freq_discharge"] / 
                                                                                    freq_1yrprepost_stg_qa.arr.ls[[lg]][, , "dischargeable", "freq_discharge"],   # Derive QAed dischargeable undischarged proportions (%) 3d-array by "date_lgyr", "trust_code", "comparator"
                                                                                  along = 3, 
                                                                                  use.dnns = TRUE
                                                                                  )  # Derive 3d-array by "date_lgyr", "trust_code", "statistic" of QAed staggered restricted delayed discharge outcomes
                                         
                                         names(dimnames(out_1yrprepost_stg_qa.arr))[names(dimnames(out_1yrprepost_stg_qa.arr)) == ""] <- "statistic"  # Rename blank "statistic" margin of QAed staggered restricted delayed discharge outcomes 3d-array
                                         
                                         return(out_1yrprepost_stg_qa.arr)  # Return as output QAed staggered restricted delayed discharge outcomes 3d-array by "date_lgyr", "trust_code", "statistic"
                                         }, 
                                       simplify = FALSE
                                       )  # Derive list by "lag" of QAed staggered restricted delayed discharge outcomes 3d-arrays by "date_lgyr", "trust_code", "statistic"


out_1yrprepost_stg_qa.dat.ls <- sapply(names(out_1yrprepost_stg_qa.arr.ls), 
                                       FUN = function(lg) 
                                         as.data.frame.table(out_1yrprepost_stg_qa.arr.ls[[lg]], 
                                                             responseName = "prop"
                                                             ),   # Format QAed staggered restricted delayed discharge outcomes 3d-array as data-frame
                                       simplify = FALSE
                                       )  # Derive list by "lag" of QAed staggered restricted delayed discharge outcomes data-frames


save(list = outer(c("freq", "out"), 
                  c("arr", "dat"), 
                  FUN = function(typ, ext) 
                    paste0(typ, "_1yrprepost_stg_qa.", ext, ".ls")
                  ), 
     file = file.path(data.dir, 
                      "freq.out_1yrprepost_stg_qa.RData"
                      )
     )  # Save QAed staggered restricted delayed discharge frequencies and outcomes arrays and data-frames lists in .RData format