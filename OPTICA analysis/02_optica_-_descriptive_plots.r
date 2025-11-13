###########################################
##                                       ##
## Analysis of OPTICA delayed discharges ##
## per occupied beds rates for Analysis  ##
##                                       ##
## Display longitudinal outcome plots    ##
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

load(file.path(data.dir, "optica_status.RData")
     )  # Load OPTICA status data-frame

load(file.path(data.dir, "freq.out_noqa.RData")
     )  # Load non-QAed delayed discharge frequencies and outcomes arrays and data-frames lists

load(file.path(data.dir, "freq.out_qa.RData")
     )  # Load QAed delayed discharge frequencies and outcomes arrays and data-frames lists

load(file.path(data.dir, "freq.out_stg_qa.RData")
     )  # Load QAed staggered delayed discharge frequencies and outcomes arrays and data-frames lists

load(file.path(data.dir, "freq.out_1yrprepost_stg_qa.RData")
     )  # Load QAed staggered restricted delayed discharge frequencies and outcomes arrays and data-frames lists


#######################
## Descriptive plots ##
#######################

## Plot of QAed staggered introduction 
## of OPTICA across NHS trusts

study_period.ls <- with(optica_status.dat, 
                        expr = list(start_moyr = format(start, format = "%b %y"),  # Format "start" date as 'mo yr' character variable
                                    study = format(seq.Date(min(start), max(start), by = "month"), 
                                                   format = "%b %y"
                                                   )  # Derive sequence by month spanning study period
                                    )
                        )  # Derive list by "type" of vectors spanning monthly OPTICA roll-out and study period


rollout_plot.arr <- with(study_period.ls, 
                         expr = outer(start_moyr, study, FUN = "==")
                         )  # Derive logical 2d-array by "start_moyr", "study" of OPTICA roll-out

dimnames(rollout_plot.arr) <- with(study_period.ls, 
                                   expr = setNames(list(start_moyr, study), 
                                                   nm = names(study_period.ls)
                                                   )
                                   )  # Set OPTICAL roll-out 2d-array margin names and labels


rollout_plot.vec <- cumsum(colSums(rollout_plot.arr))  # Derive vector of cumulative OPTICAL roll-out frequencies


barplot(rollout_plot.vec, 
        names.arg = "", axes = FALSE, 
        main = "OPTICA Roll-out\nacross Acute NHS Trusts in England", 
        xlab = "Month", ylab = "No. trusts", 
        xlim = range(barplot(rollout_plot.vec, plot = FALSE)), 
        ylim = range(pretty(rollout_plot.vec, n = 8))
        )  # Time-series barplot of cumulative OPTICA roll-out frequencies

axis(1, 
     at = barplot(rollout_plot.vec, plot = FALSE), 
     labels = ifelse(seq_along(rollout_plot.vec) %% 2 == 1, 
                     names(rollout_plot.vec), NA
                     ), 
     cex.axis = .8, las = 2
     )  # Overlay X-axis labels onto time-series barplot

axis(2, 
     at = pretty(rollout_plot.vec, n = 8), 
     cex.axis = .8, las = 2
     )  # Overlay Y-axis labels onto time-series barplot

abline(h = pretty(rollout_plot.vec, n = 8), col = 8, lty = 3)  # Overlay horizontal grid lines for readability

dev.print(pdf, 
          file = file.path(output.dir, 
                           "Graphs/QA/Descriptive/rollout.pdf"
                           ), 
          width = 1024 / 72, height = 768 / 72
          )  # Export time-series plot in .pdf format

# dev.print(png, 
#           file = file.path(output.dir, 
#                            "Graphs/QA/Descriptive/rollout.png"
#                            ), 
#           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
#           # width = 14, height = 7, units = "in", res = 3e2
#           # width = 1024, height = 768
#           )  # Export time-series plot in .png format

# unlink(file.path(output.dir, 
#                  file.path(output.dir, 
#                            "Graphs/QA/Descriptive/rollout.*"
#                            )
#                  )
#        )  # Remove time-series plot files


## Time-series plots of 
## delayed discharge outcomes 
## vs calendar time 
## across NHS trusts

lapply(names(out_noqa.arr.ls), 
       FUN = function(lg) 
         lapply(dimnames(out_noqa.arr.ls[[lg]])$statistic, 
                FUN = function(out)
                  {
                  lapply(split(optica_status.dat$trust_code, 
                               f = gl(length(optica_status.dat$trust_code) %/% 5, 
                                      k = 5
                                      )
                               ), 
                         FUN = function(trust.vec)
                           {
                           matplot(out_noqa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                   type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                                   col = hcl.colors(length(trust.vec), 
                                                    palette = hcl.pals("qualitative")[3]
                                                    ), 
                                   lty = 1, pch = 16, 
                                   main = paste("Proportions of", 
                                                switch(match(out, table = dimnames(out_noqa.arr.ls[[lg]])$statistic), 
                                                       "Beds Occupied by", "Undischarged"
                                                       ), 
                                                "Patients Medically Fit for Discharge\nfrom Acute NHS Trusts in England", 
                                                sep = " "
                                                ), 
                                   sub = "OPTICA trusts", 
                                   xlab = switch(match(lg, table = names(out_noqa.arr.ls)), 
                                                 "Week", "Month"
                                                 ), 
                                   ylab = "Proportion of undischarged patients (%)", 
                                   ylim = range(pretty(out_noqa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                                       n = 8
                                                       )
                                                )
                                   )  # Time-series plots of 'With OPTICA' "comparator" delayed discharges outcomes
                           
                           matlines(sapply(trust.vec, 
                                           FUN = function(trust) 
                                             predict(loess(prop ~ I(seq_along(get(paste0("date_", lg, "yr")))), 
                                                           data = out_noqa.dat.ls[[lg]], 
                                                           subset = trust_code == trust & 
                                                             comparator == "With OPTICA" & 
                                                             statistic == out, 
                                                           na.action = na.exclude
                                                           )
                                                     ), 
                                           simplify = "array"
                                           ), 
                                    col = hcl.colors(length(trust.vec), 
                                                     palette = hcl.pals("qualitative")[3]
                                                     ), 
                                    lty = 2, lwd = 1
                                    )  # Overlay LOESS point predictions over time-series plot
                           
                           with(dimnames(out_noqa.arr.ls[[lg]]), 
                                expr = axis(1, 
                                            at = seq_along(get(paste0("date_", lg, "yr"))), 
                                            labels = ifelse(seq_along(get(paste0("date_", lg, "yr"))) %% 2 == 1, 
                                                            get(paste0("date_", lg, "yr")), NA
                                                            ), 
                                            cex.axis = .8, las = 2
                                            )
                                )  # Overlay X-axis labels onto time-series plot
                           
                           axis(2, 
                                at = pretty(out_noqa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                            n = 8
                                            ), 
                                labels = pretty(out_noqa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                                n = 8
                                                ), 
                                cex.axis = .8, las = 2
                                )  # Overlay Y-axis labels onto time-series plot
                           
                           abline(v = match(with(optica_status.dat, 
                                                 expr = 
                                                   get(paste0("start_", lg, "yr"))[match(trust.vec, 
                                                                                         table = trust_code
                                                                                         )]
                                                 ), 
                                            table = dimnames(out_noqa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]
                                            ), 
                                  col = hcl.colors(length(trust.vec), 
                                                   palette = hcl.pals("qualitative")[3]
                                                   ), 
                                  lty = 2, lwd = 2
                                  )  # Overlay vertical line at trust's OPTICA start date
                           
                           text(match(with(optica_status.dat, 
                                           expr = 
                                             get(paste0("start_", lg, "yr"))[match(trust.vec, 
                                                                                   table = trust_code
                                                                                   )]
                                           ), 
                                      table = dimnames(out_noqa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]
                                      ), 
                                max(pretty(out_noqa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                           n = 8
                                           )
                                    ), 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 4#, adj = c(.5, -.1), cex = .8, col = 8
                                )  # Overlay OPTICA start date onto time-series plot
                           
                           
                           
                           with(dimnames(out_noqa.arr.ls[[lg]]), 
                                expr = abline(v = seq_along(get(paste0("date_", lg, "yr"))
                                                            )[grepl(switch(match(lg, 
                                                                                 table = names(out_noqa.arr.ls)
                                                                                 ), 
                                                                           "^52", "^Dec"
                                                                           ), 
                                                                    x = get(paste0("date_", lg, "yr"))
                                                                    )] + .5, 
                                              col = 8, lty = 3
                                              )
                                )  # Overlay vertical grid year delimiters for readability
                           
                           text(1, 
                                out_noqa.arr.ls[[lg]][1, trust.vec, "With OPTICA", out], 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 2, cex = .75
                                )  # Overlay "trust_code" onto time-series plot
                           
                           text(length(dimnames(out_noqa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]), 
                                out_noqa.arr.ls[[lg]][length(dimnames(out_noqa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]), 
                                                      trust.vec, "With OPTICA", out], 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 4, cex = .75
                                )  # Overlay "trust_code" onto time-series plot
                           
                           legend("topleft", 
                                  legend = c("Observed", "Smoothed"), 
                                  bty = "n", cex = 1, lty = seq.int(2), pch = c(16, NA)#, x.intersp = 1, y.intersp = .5
                                  )  # Overlay legend with "statistic" keys onto time-series plot
                           
                           dev.print(pdf, 
                                     file = file.path(output.dir, 
                                                      "Graphs/Non-QA/Descriptive", 
                                                      switch(match(out, 
                                                                   table = dimnames(out_noqa.arr.ls[[lg]])$statistic
                                                                   ), 
                                                             "Undischarged out of beds", 
                                                             "Undischarged out of dischargeable"
                                                             ), 
                                                      switch(match(lg, table = names(out_noqa.arr.ls)), 
                                                             "Weekly", "Monthly"
                                                             ), 
                                                      "OPTICA trusts (calendar time)", 
                                                      paste(out, lg, 
                                                            paste(trust.vec, collapse = "."), 
                                                            "noqa_opt.pdf", 
                                                            sep = "_"
                                                            )
                                                      ), 
                                     width = 1024 / 72, height = 768 / 72
                                     )  # Export time-series plot in .pdf format
                           
                           # dev.print(png, 
                           #           file = file.path(output.dir, 
                           #                            "Graphs/Non-QA/Descriptive", 
                           #                            switch(match(out, 
                           #                                         table = dimnames(out_noqa.arr.ls[[lg]])$statistic
                           #                                         ), 
                           #                                   "Undischarged out of beds", 
                           #                                   "Undischarged out of dischargeable"
                           #                                   ), 
                           #                            switch(match(lg, table = names(out_noqa.arr.ls)), 
                           #                                   "Weekly", "Monthly"
                           #                                   ), 
                           #                            "OPTICA trusts (calendar time)", 
                           #                            paste(out, lg, 
                           #                                  paste(trust.vec, collapse = "."), 
                           #                                  "noqa_opt.png", 
                           #                                  sep = "_"
                           #                                  )
                           #                            ), 
                           #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                           #           # width = 14, height = 7, units = "in", res = 3e2
                           #           # width = 1024, height = 768
                           #           )  # Export time-series plot in .png format
                           
                           # unlink(file.path(output.dir, 
                           #                  "Graphs/Non-QA/Descriptive", 
                           #                  switch(match(out, 
                           #                               table = dimnames(out_noqa.arr.ls[[lg]])$statistic
                           #                               ), 
                           #                         "Undischarged out of beds", 
                           #                         "Undischarged out of dischargeable"
                           #                         ), 
                           #                  switch(match(lg, table = names(out_noqa.arr.ls)), 
                           #                         "Weekly", "Monthly"
                           #                         ), 
                           #                  "OPTICA trusts (calendar time)", 
                           #                  paste(out, lg, 
                           #                        paste(trust.vec, collapse = "."), 
                           #                        "noqa_opt.*", 
                           #                        sep = "_"
                           #                        )
                           #                  )
                           #        )  # Remove time-series plot files
                           }
                         )
                  
                  
                  lapply(split(setdiff(dimnames(out_noqa.arr.ls[[lg]])$trust_code, 
                                       y = c("pooled", optica_status.dat$trust_code)
                                       ), 
                               f = gl(length(setdiff(dimnames(out_noqa.arr.ls[[lg]])$trust_code, 
                                                     y = c("pooled", optica_status.dat$trust_code)
                                                     )
                                             ) %/% 6, 
                                      k = 6
                                      )
                               ), 
                         FUN = function(trust.vec)
                           {
                           matplot(out_noqa.arr.ls[[lg]][, trust.vec, "Without OPTICA", out], 
                                   type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                                   col = hcl.colors(length(trust.vec), 
                                                    palette = hcl.pals("qualitative")[3]
                                                    ), 
                                   lty = 1, pch = 16, 
                                   main = paste("Proportions of", 
                                                switch(match(out, table = dimnames(out_noqa.arr.ls[[lg]])$statistic), 
                                                       "Beds Occupied by", "Undischarged"
                                                       ), 
                                                "Patients Medically Fit for Discharge\nfrom Acute NHS Trusts in England", 
                                                sep = " "
                                                ), 
                                   sub = "Non-OPTICA trusts", 
                                   xlab = switch(match(lg, table = names(out_noqa.arr.ls)), 
                                                 "Week", "Month"
                                                 ), 
                                   ylab = "Proportion of undischarged patients (%)", 
                                   ylim = range(pretty(out_noqa.arr.ls[[lg]][, trust.vec, "Without OPTICA", out], 
                                                       n = 8
                                                       )
                                                )
                                   )  # Time-series plots of 'Without OPTICA' "comparator" delayed discharges outcomes
                           
                           matlines(sapply(trust.vec, 
                                           FUN = function(trust) 
                                             predict(loess(prop ~ I(seq_along(get(paste0("date_", lg, "yr")))), 
                                                           data = out_noqa.dat.ls[[lg]], 
                                                           subset = trust_code == trust & 
                                                             comparator == "Without OPTICA" & 
                                                             statistic == out, 
                                                           na.action = na.exclude
                                                           )
                                                     ), 
                                           simplify = "array"
                                           ), 
                                    col = hcl.colors(length(trust.vec), 
                                                     palette = hcl.pals("qualitative")[3]
                                                     ), 
                                    lty = 2, lwd = 1
                                    )  # Overlay LOESS point predictions over time-series plot
                           
                           with(dimnames(out_noqa.arr.ls[[lg]]), 
                                expr = axis(1, 
                                            at = seq_along(get(paste0("date_", lg, "yr"))), 
                                            labels = ifelse(seq_along(get(paste0("date_", lg, "yr"))) %% 2 == 1, 
                                                            get(paste0("date_", lg, "yr")), NA
                                                            ), 
                                            cex.axis = .8, las = 2
                                            )
                                )  # Overlay X-axis labels onto time-series plot
                           
                           axis(2, 
                                at = pretty(out_noqa.arr.ls[[lg]][, trust.vec, "Without OPTICA", out], 
                                            n = 8
                                            ), 
                                labels = pretty(out_noqa.arr.ls[[lg]][, trust.vec, "Without OPTICA", out], 
                                                n = 8
                                                ), 
                                cex.axis = .8, las = 2
                                )  # Overlay Y-axis labels onto time-series plot
                           
                           with(dimnames(out_noqa.arr.ls[[lg]]), 
                                expr = abline(v = seq_along(get(paste0("date_", lg, "yr"))
                                                            )[grepl(switch(match(lg, 
                                                                                 table = names(out_noqa.arr.ls)
                                                                                 ), 
                                                                           "^52", "^Dec"
                                                                           ), 
                                                                    x = get(paste0("date_", lg, "yr"))
                                                                    )] + .5, 
                                              col = 8, lty = 3
                                              )
                                )  # Overlay vertical grid year delimiters for readability
                           
                           text(1, 
                                out_noqa.arr.ls[[lg]][1, trust.vec, "Without OPTICA", out], 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 2, cex = .75
                                )  # Overlay "trust_code" onto time-series plot
                           
                           text(length(dimnames(out_noqa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]), 
                                out_noqa.arr.ls[[lg]][length(dimnames(out_noqa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]), 
                                                      trust.vec, "Without OPTICA", out
                                                      ], 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 4, cex = .75
                                )  # Overlay "trust_code" onto time-series plot
                           
                           legend("topleft", 
                                  legend = c("Observed", "Smoothed"), 
                                  bty = "n", cex = 1, lty = seq.int(2), pch = c(16, NA)#, x.intersp = 1, y.intersp = .5
                                  )  # Overlay legend with "statistic" keys onto time-series plot
                           
                           dev.print(pdf, 
                                     file = file.path(output.dir, 
                                                      "Graphs/Non-QA/Descriptive", 
                                                      switch(match(out, 
                                                                   table = dimnames(out_noqa.arr.ls[[lg]])$statistic
                                                                   ), 
                                                             "Undischarged out of beds", 
                                                             "Undischarged out of dischargeable"
                                                             ), 
                                                      switch(match(lg, table = names(out_noqa.arr.ls)), 
                                                             "Weekly", "Monthly"
                                                             ), 
                                                      "Non-OPTICA trusts", 
                                                      paste(out, lg, 
                                                            paste(trust.vec, collapse = "."), 
                                                            "noqa_noopt.pdf", 
                                                            sep = "_"
                                                            )
                                                      ), 
                                     width = 1024 / 72, height = 768 / 72
                                     )  # Export time-series plot in .pdf format
                           
                           # dev.print(png, 
                           #           file = file.path(output.dir, 
                           #                            "Graphs/Non-QA/Descriptive", 
                           #                            switch(match(out, 
                           #                                         table = dimnames(out_noqa.arr.ls[[lg]])$statistic
                           #                                         ), 
                           #                                   "Undischarged out of beds", 
                           #                                   "Undischarged out of dischargeable"
                           #                                   ), 
                           #                            switch(match(lg, table = names(out_noqa.arr.ls)), 
                           #                                   "Weekly", "Monthly"
                           #                                   ), 
                           #                            "Non-OPTICA trusts", 
                           #                            paste(out, lg, 
                           #                                  paste(trust.vec, collapse = "."), 
                           #                                  "noqa_noopt.png", 
                           #                                  sep = "_"
                           #                                  )
                           #                            ), 
                           #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                           #           # width = 14, height = 7, units = "in", res = 3e2
                           #           # width = 1024, height = 768
                           #           )  # Export time-series plot in .png format
                           
                           # unlink(file.path(output.dir, 
                           #                  "Graphs/Non-QA/Descriptive", 
                           #                  switch(match(out, 
                           #                               table = dimnames(out_noqa.arr.ls[[lg]])$statistic
                           #                              ), 
                           #                         "Undischarged out of beds", 
                           #                         "Undischarged out of dischargeable"
                           #                         ), 
                           #                  switch(match(lg, table = names(out_noqa.arr.ls)), 
                           #                         "Weekly", "Monthly"
                           #                         ), 
                           #                  "Non-OPTICA trusts", 
                           #                  paste(out, lg, 
                           #                        paste(trust.vec, collapse = "."), 
                           #                        "noqa_noopt.*", 
                           #                        sep = "_"
                           #                        )
                           #                  )
                           #        )  # Remove time-series plot files
                           }
                         )
                  
                  
                  matplot(out_noqa.arr.ls[[lg]][, "pooled", , out], 
                          type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                          col = c(4, 2), lty = 1, pch = 16, 
                          main = paste("Proportions of", 
                                       switch(match(out, table = dimnames(out_noqa.arr.ls[[lg]])$statistic), 
                                              "Beds Occupied by", "Undischarged"
                                              ), 
                                       "Patients Medically Fit for Discharge\nfrom Acute NHS Trusts in England", 
                                       sep = " "
                                       ), 
                          sub = "Pooled trusts", 
                          xlab = switch(match(lg, table = names(out_noqa.arr.ls)), 
                                        "Week", "Month"
                                        ), 
                          ylab = "Proportion of undischarged patients (%)", 
                          ylim = range(pretty(out_noqa.arr.ls[[lg]][, "pooled", , out], 
                                              n = 8
                                              )
                                       )
                          )  # Time-series plots of key delayed discharge outcomes
                  
                  matlines(sapply(dimnames(out_noqa.arr.ls[[lg]])$comparator, 
                                  FUN = function(grp) 
                                    predict(loess(prop ~ I(seq_along(get(paste0("date_", lg, "yr")))), 
                                                  data = out_noqa.dat.ls[[lg]], 
                                                  subset = trust_code == "pooled" & 
                                                    comparator == grp & 
                                                    statistic == out, 
                                                  na.action = na.exclude
                                                  )
                                            ), 
                                  simplify = "array"
                                  ), 
                           col = c(4, 2), lty = 2, lwd = 1
                           )  # Overlay LOESS point predictions by "comparator" over time-series plot
                  
                  with(dimnames(out_noqa.arr.ls[[lg]]), 
                       expr = axis(1, 
                                   at = seq_along(get(paste0("date_", lg, "yr"))), 
                                   labels = ifelse(seq_along(get(paste0("date_", lg, "yr"))) %% 2 == 1, 
                                                   get(paste0("date_", lg, "yr")), NA
                                                   ), 
                                   cex.axis = .8, las = 2
                                   )
                       )  # Overlay X-axis labels onto time-series plot
                  
                  axis(2, 
                       at = pretty(out_noqa.arr.ls[[lg]][, "pooled", , out], 
                                   n = 8
                                   ), 
                       labels = pretty(out_noqa.arr.ls[[lg]][, "pooled", , out], 
                                       n = 8
                                       ), 
                       cex.axis = .8, las = 2
                       )  # Overlay Y-axis labels onto time-series plot
                  
                  with(dimnames(out_noqa.arr.ls[[lg]]), 
                       expr = abline(v = seq_along(get(paste0("date_", lg, "yr"))
                                                   )[grepl(switch(match(lg, 
                                                                        table = names(out_noqa.arr.ls)
                                                                        ), 
                                                                  "^52", "^Dec"
                                                                  ), 
                                                           x = get(paste0("date_", lg, "yr"))
                                                           )] + .5, 
                                     col = 8, lty = 3
                                     )
                       )  # Overlay vertical grid lines for readability
                  
                  legend("topleft", 
                         legend = c("Observed", "Smoothed"), 
                         bty = "n", cex = 1, lty = seq.int(2), x.intersp = 1, y.intersp = .5
                         )  # Overlay legend with "comparator" keys onto time-series plot
                  
                  legend("topright", 
                         legend = dimnames(out_noqa.arr.ls[[lg]])$comparator, 
                         bty = "n", cex = 1, fill = c(4, 2)#, x.intersp = 1, y.intersp = .8, inset = -.05, 
                         )  # Overlay legend with "comparator" keys onto time-series plot
                  
                  dev.print(pdf, 
                            file = file.path(output.dir, 
                                             "Graphs/Non-QA/Descriptive", 
                                             switch(match(out, 
                                                          table = dimnames(out_noqa.arr.ls[[lg]])$statistic
                                                          ), 
                                                    "Undischarged out of beds", 
                                                    "Undischarged out of dischargeable"
                                                    ), 
                                             switch(match(lg, table = names(out_noqa.arr.ls)), 
                                                    "Weekly", "Monthly"
                                                    ), 
                                             "Pooled trusts", 
                                             paste(out, lg, "pooled_noqa.pdf", sep = "_")
                                             ), 
                            width = 1024 / 72, height = 768 / 72
                            )  # Export time-series plot in .pdf format
                  
                  # dev.print(png, 
                  #           file = file.path(output.dir, 
                  #                            "Graphs/Non-QA/Descriptive", 
                  #                            switch(match(out, 
                  #                                         table = dimnames(out_noqa.arr.ls[[lg]])$statistic
                  #                                         ), 
                  #                                   "Undischarged out of beds", 
                  #                                   "Undischarged out of dischargeable"
                  #                                   ), 
                  #                            switch(match(lg, table = names(out_noqa.arr.ls)), 
                  #                                   "Weekly", "Monthly"
                  #                                  ), 
                  #                            "Pooled trusts", 
                  #                            paste(out, lg, "pooled_noqa.png", sep = "_")
                  #                            ), 
                  #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                  #           # width = 14, height = 7, units = "in", res = 3e2
                  #           # width = 1024, height = 768
                  #           )  # Export time-series plot in .png format
                  
                  # unlink(file.path(output.dir, 
                  #                  "Graphs/Non-QA/Descriptive", 
                  #                  switch(match(out, 
                  #                               table = dimnames(out_noqa.arr.ls[[lg]])$statistic
                  #                               ), 
                  #                         "Undischarged out of beds", 
                  #                         "Undischarged out of dischargeable"
                  #                         ), 
                  #                  "Pooled trusts", 
                  #                  paste(out, lg, "pooled_noqa.*", sep = "_")
                  #                  )
                  #        )  # Remove time-series plot files
                  }
                )
       )


#################################
## QA plots from pre-processed ##
## analysis data-sets          ##
#################################

out_noqa_plot.arr.ls <- sapply(names(out_noqa.arr.ls), 
                               FUN = function(lg) 
                                 abind(out_noqa.arr.ls[[lg]][, optica_status.dat$trust_code, "With OPTICA", 
                                                             ],   # Subset to 'With OPTICA' "comparator" delayed discharge outcomes 4d-array)
                                       out_noqa.arr.ls[[lg]][, setdiff(dimnames(out_noqa.arr.ls[[lg]])$trust_code, 
                                                                       y = c("pooled", optica_status.dat$trust_code)
                                                                       ), "Without OPTICA", 
                                                             ],   # Subset to 'Without OPTICA' "comparator" delayed discharge outcomes 4d-array
                                       along = 2, 
                                       use.dnns = TRUE
                                       ), 
                               simplify = FALSE
                               )  # List by "lag" of 3d-arrays by "date_lgyr", "trust_code", "statistic" of delayed discharge outcomes 3d-arrays


out_noqa_plot.dat.ls <- sapply(names(out_noqa_plot.arr.ls), 
                               FUN = function(lg) 
                                 as.data.frame.table(out_noqa_plot.arr.ls[[lg]], 
                                                     responseName = "prop"
                                                     ), 
                               simplify = FALSE
                               )  # Derive list by "lag" of QAed staggered delayed discharge outcomes 3d-arrays formatted as data-frames


## Time-series plots of 
## delayed discharge outcomes 
## vs calendar time 
## across NHS trusts

lapply(names(out_noqa_plot.arr.ls), 
       FUN = function(lg) 
         lapply(dimnames(out_noqa_plot.arr.ls[[lg]])$statistic, 
                FUN = function(out) 
                  lapply(dimnames(out_noqa_plot.arr.ls[[lg]])$trust_code, 
                         FUN = function(trust)
                           {
                           
                           # lg <- names(out_noqa_plot.arr.ls)[1]; lg
                           # out <- dimnames(out_noqa_plot.arr.ls[[lg]])$statistic[2]; out
                           # trust <- dimnames(out_noqa_plot.arr.ls[[lg]])$trust_code[55]; trust
                           # rm(lg, out, trust)
                           
                           plot(out_noqa_plot.arr.ls[[lg]][, trust, out], 
                                type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                                col = 1, lty = 1, pch = 16, 
                                main = paste("Proportions of", 
                                             switch(match(out, 
                                                          table = dimnames(out_noqa_plot.arr.ls[[lg]])$statistic
                                                          ), 
                                                    "Beds Occupied by Patients Fit for Discharge\n", 
                                                    "Undischarged Patients Fit for Discharge\n"
                                                    ), 
                                             "from the", trust, "Acute NHS Trust in England", 
                                             sep = " "
                                             ), 
                                sub = paste(trust, "Trust", sep = " "), 
                                xlab = switch(match(lg, table = names(out_noqa_plot.arr.ls)), 
                                              "Week", "Month"
                                              ), 
                                ylab = "Proportion of undischarged patients (%)", 
                                ylim = range(pretty(out_noqa_plot.arr.ls[[lg]][, trust, out], 
                                                    n = 8
                                                    )
                                             )
                                )  # Time-series plots of delayed discharge outcomes
                           
                           lines(predict(loess(prop ~ I(seq_along(get(paste0("date_", lg, "yr")))), 
                                               data = out_noqa_plot.dat.ls[[lg]], 
                                               subset = trust_code == trust & statistic == out, 
                                               na.action = na.exclude
                                               )
                                         ), 
                                 col = 1, lty = 2, lwd = 1
                                 )  # Overlay LOESS point predictions over time-series plot
                           
                           with(dimnames(out_noqa_plot.arr.ls[[lg]]), 
                                expr = axis(1, 
                                            at = seq_along(get(paste0("date_", lg, "yr"))), 
                                            labels = ifelse(seq_along(get(paste0("date_", lg, "yr"))) %% 2 == 1, 
                                                            get(paste0("date_", lg, "yr")), 
                                                            NA
                                                            ), 
                                            cex.axis = .8, las = 2
                                            )
                                )  # Overlay X-axis labels onto time-series plot
                           
                           axis(2, 
                                at = pretty(out_noqa_plot.arr.ls[[lg]][, trust, out], 
                                            n = 8
                                            ), 
                                labels = pretty(out_noqa_plot.arr.ls[[lg]][, trust, out], 
                                                n = 8
                                                ), 
                                cex.axis = .8, las = 2
                                )  # Overlay Y-axis labels onto time-series plot
                           
                           if(trust %in% optica_status.dat$trust_code)
                             {
                             abline(v = match(with(optica_status.dat, 
                                                   expr = 
                                                     get(paste0("start_", lg, "yr"))[match(trust, 
                                                                                           table = trust_code
                                                                                           )]
                                                   ), 
                                              table = dimnames(out_noqa_plot.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]
                                              ), 
                                    col = 8, lty = 2, lwd = 2
                                    )  # Overlay vertical line at trust's OPTICA start date
                             
                             text(match(with(optica_status.dat, 
                                             expr = 
                                               get(paste0("start_", lg, "yr"))[match(trust, 
                                                                                     table = trust_code
                                                                                     )]
                                             ), 
                                        table = dimnames(out_noqa_plot.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]
                                        ), 
                                  max(pretty(out_noqa_plot.arr.ls[[lg]][, trust, out], 
                                             n = 8
                                             )
                                      ), 
                                  labels = "OPTICA\ndeployed", 
                                  cex = 1, col = 8, pos = 4, adj = c(.5, -.1)
                                  )  # Overlay OPTICA start date onto time-series plot
                             }
                           
                           with(dimnames(out_noqa_plot.arr.ls[[lg]]), 
                                expr = abline(v = seq_along(get(paste0("date_", lg, "yr"))
                                                            )[grepl(switch(match(lg, 
                                                                                 table = names(out_noqa_plot.arr.ls)
                                                                                 ), 
                                                                           "^52", "^Dec"
                                                                           ), 
                                                                    x = get(paste0("date_", lg, "yr"))
                                                                    )] + .5, 
                                              col = 8, lty = 3
                                              )
                                )  # Overlay vertical grid year delimiters for readability
                           
                           legend("topleft", 
                                  legend = c("Observed", "Smoothed"), 
                                  bty = "n", cex = 1, lty = seq.int(2), pch = c(16, NA)#, x.intersp = 1, y.intersp = .5
                                  )  # Overlay legend with "statistic" keys onto time-series plot
                           
                           dev.print(pdf, 
                                     file = file.path(output.dir, 
                                                      "Graphs/Non-QA/Descriptive", 
                                                      switch(match(out, 
                                                                   table = dimnames(out_noqa_plot.arr.ls[[lg]])$statistic
                                                                   ), 
                                                             "Undischarged out of beds", 
                                                             "Undischarged out of dischargeable"
                                                             ), 
                                                      switch(match(lg, table = names(out_noqa_plot.arr.ls)), 
                                                             "Weekly", "Monthly"
                                                             ), 
                                                      ifelse(trust %in% optica_status.dat$trust_code, 
                                                             "OPTICA trusts (calendar time)", 
                                                             "Non-OPTICA trusts"
                                                             ), 
                                                      paste(paste(out, lg, trust, "noqa", 
                                                                  ifelse(trust %in% optica_status.dat$trust_code, 
                                                                         "opt", "noopt"
                                                                         ), 
                                                                  sep = "_"
                                                                  ), 
                                                            "pdf", 
                                                            sep = "."
                                                            )
                                                      ), 
                                     width = 1024 / 72, height = 768 / 72
                                     )  # Export time-series plot in .pdf format
                           
                           # dev.print(png, 
                           #           file = file.path(output.dir, 
                           #                            "Graphs/Non-QA/Descriptive", 
                           #                            switch(match(out, 
                           #                                         table = dimnames(out_noqa_plot.arr.ls[[lg]])$statistic
                           #                                         ), 
                           #                                   "Undischarged out of beds", 
                           #                                   "Undischarged out of dischargeable"
                           #                                   ), 
                           #                            switch(match(lg, table = names(out_noqa_plot.arr.ls)), 
                           #                                   "Weekly", "Monthly"
                           #                                   ), 
                           #                            ifelse(trust %in% optica_status.dat$trust_code, 
                           #                                   "OPTICA trusts (calendar time)", 
                           #                                   "Non-OPTICA trusts"
                           #                                   ), 
                           #                            paste(paste(out, lg, trust, "noqa", 
                           #                                        ifelse(trust %in% optica_status.dat$trust_code, 
                           #                                               "opt", "noopt"
                           #                                               ), 
                           #                                        sep = "_"
                           #                                        ), 
                           #                                  "png", 
                           #                                  sep = "."
                           #                                  )
                           #                            ), 
                           #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                           #           # width = 14, height = 7, units = "in", res = 3e2
                           #           # width = 1024, height = 768
                           #           )  # Export time-series plot in .png format
                           
                           # unlink(file.path(output.dir, 
                           #                  "Graphs/Non-QA/Descriptive", 
                           #                  switch(match(out, 
                           #                               table = dimnames(out_noqa_plot.arr.ls[[lg]])$statistic
                           #                               ), 
                           #                         "Undischarged out of beds", 
                           #                         "Undischarged out of dischargeable"
                           #                         ), 
                           #                  switch(match(lg, table = names(out_noqa_plot.arr.ls)), 
                           #                         "Weekly", "Monthly"
                           #                         ), 
                           #                  ifelse(trust %in% optica_status.dat$trust_code, 
                           #                         "OPTICA trusts (calendar time)", 
                           #                         "Non-OPTICA trusts"
                           #                         ), 
                           #                  paste(paste(out, lg, trust, "noqa", 
                           #                              ifelse(trust %in% optica_status.dat$trust_code, 
                           #                                     "opt", "noopt"
                           #                                     ), 
                           #                              sep = "_"
                           #                              ), 
                           #                        "*", 
                           #                        sep = "."
                           #                        )
                           #                  )
                           #        )  # Remove time-series plot files
                           }
                         )
                )
       )


############################
## Time-series plots from ##
## QAed analysis data-set ##
############################

out_qa_plot.arr.ls2 <- sapply(names(out_qa.arr.ls), 
                              FUN = function(lg)
                                {
                                out_qa_nopool_opt.arr <- out_qa.arr.ls[[lg]][, optica_status.dat$trust_code, 
                                                                             "With OPTICA", ]  # Subset to 'With OPTICA' "comparator" QAed delayed discharge outcomes for plotting 3d-array
                                
                                
                                out_qa_pool_opt.arr <- apply(out_qa_nopool_opt.arr, 
                                                             MARGIN = c(1, 3), 
                                                             FUN = quantile, 
                                                             probs = c(.25, .5, .75), na.rm = TRUE
                                                             )  # Derive 3d-array by "quantile", "date_lgyr", "statistic" of 'With OPTICA' "comparator" QAed delayed discharge outcomes quartiles across "trust_code" for plotting
                                
                                out_qa_pool_opt.arr <- abind(pooled = out_qa.arr.ls[[lg]][, "pooled", "With OPTICA", ], 
                                                             out_qa_pool_opt.arr, 
                                                             along = 1, 
                                                             use.dnns = TRUE
                                                             )  # Augment 'With OPTICA' "comparator" QAed delayed discharge outcome quartiles for plotting 3d-array with 'Pooled' "trust_code"
                                
                                names(dimnames(out_qa_pool_opt.arr))[names(dimnames(out_qa_pool_opt.arr)) == ""] <- "quantile"  # Rename blank "quantile" margin of 'With OPTICA' "comparator" QAed delayed discharge outcome quartiles for plotting 3d-array
                                
                                
                                out_qa_nopool_noopt.arr <- out_qa.arr.ls[[lg]][, setdiff(dimnames(out_qa.arr.ls[[lg]])$trust_code, 
                                                                                         y = c("pooled", optica_status.dat$trust_code)
                                                                                         ), "Without OPTICA", ]  # Subset to 'Without OPTICA' "comparator" QAed delayed discharge outcomes for plotting 3d-array
                                
                                
                                out_qa_pool_noopt.arr <- apply(out_qa_nopool_noopt.arr, 
                                                               MARGIN = c(1, 3), 
                                                               FUN = quantile, 
                                                               probs = c(.25, .5, .75), na.rm = TRUE
                                                               )  # Derive 3d-array by "quantile", "date_lgyr", "statistic" of 'Without OPTICA' "comparator" QAed delayed discharge outcomes quartiles across "trust_code" for plotting
                                
                                out_qa_pool_noopt.arr <- abind(pooled = out_qa.arr.ls[[lg]][, "pooled", "Without OPTICA", ], 
                                                               out_qa_pool_noopt.arr, 
                                                               along = 1, 
                                                               use.dnns = TRUE
                                                               )  # Augment 'Without OPTICA' "comparator" QAed delayed discharge outcome quartiles for plotting 3d-array with 'Pooled' "trust_code"
                                
                                names(dimnames(out_qa_pool_noopt.arr))[names(dimnames(out_qa_pool_noopt.arr)) == ""] <- "quantile"  # Rename blank "quantile" margin of 'Without OPTICA' "comparator" QAed delayed discharge outcome quartiles for plotting 3d-array
                                
                                
                                out_qa_nopool.arr <- abind(out_qa_nopool_opt.arr, 
                                                           out_qa_nopool_noopt.arr, 
                                                           along = 2, 
                                                           use.dnns = TRUE
                                                           )  # Derive QAed delayed discharge outcomes for plotting 3d-array by "date_lgyr", "trust_code", "statistic"
                                
                                
                                out_qa_pool.arr <- abind("With OPTICA" = out_qa_pool_opt.arr, 
                                                         "Without OPTICA" = out_qa_pool_noopt.arr, 
                                                         along = 4, 
                                                         use.dnns = TRUE
                                                         )  # Derive Qaed delayed discharge outcomes quartiles for plotting 4d-array by "quantile", "date_lgyr", "statistic", "comparison"
                                
                                names(dimnames(out_qa_pool.arr))[names(dimnames(out_qa_pool.arr)) == ""] <- "comparison"  # Rename blank "comparison" margin of QAed delayed discharge outcome quartiles for plotting 4d-array
                                
                                out_qa_pool.arr <- aperm(out_qa_pool.arr, 
                                                         perm = c(2:1, 4:3)
                                                         )  # Permute margins of QAed delayed discharge outcome quartiles for plotting 4d-array by "date_lgyr", "quantile", "comparison", "statistic"
                                
                                
                                out_qa.arr.ls <- list(nopooled = out_qa_nopool.arr, 
                                                      pooled = out_qa_pool.arr
                                                      )  # Set list by "type" of Qaed delayed discharge outcomes for plotting arrays
                                
                                return(out_qa.arr.ls)  # Return as output list by "type" of Qaed delayed discharge outcomes for plotting arrays
                                }, 
                              simplify = FALSE
                              )  # Derive list by "lag" of lists by "type" of QAed delayed discharge outcomes for plotting 3d-arrays by "date_lgyr", "trust_code", "statistic"


out_qa_plot.dat.ls2 <- sapply(names(out_qa_plot.arr.ls2), 
                              FUN = function(lg) 
                                sapply(names(out_qa_plot.arr.ls2[[lg]]), 
                                       FUN = function(typ) 
                                         as.data.frame.table(out_qa_plot.arr.ls2[[lg]][[typ]], 
                                                             responseName = "prop"
                                                             ),   # Format QAed delayed discharge outcomes 3d-array for plotting as data-frame
                                       simplify = FALSE
                                       ),   # Derive list by "lag" of QAed delayed discharge outcomes for plotting data-frames
                              simplify = FALSE
                              )  # Derive list by "lag" of lists by "type" of QAed delayed discharge outcomes for plotting data-frames


## Time-series plot of 
## QAed delayed discharge outcomes 
## vs calendar time 
## for all NHS trusts

lapply(names(out_qa_plot.arr.ls2), 
       FUN = function(lg) 
         lapply(dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)$statistic, 
                FUN = function(out)
                  {
                  matplot(out_qa.arr.ls[[lg]][, optica_status.dat$trust_code, "With OPTICA", out], 
                          type = "l", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                          col = 1, lty = 1, 
                          main = paste("Proportions of", 
                                       switch(match(out, table = dimnames(out_qa.arr.ls[[lg]])$statistic), 
                                              "Beds Occupied by", "Undischarged"
                                              ), 
                                       "Patients Medically Fit for Discharge\nfrom Acute NHS Trusts in England", 
                                       sep = " "
                                       ), 
                          xlab = switch(match(lg, table = names(out_qa.arr.ls)), 
                                        "Week", "Month"
                                        ), 
                          ylab = "Proportion of undischarged patients (%)", 
                          ylim = range(pretty(out_qa.arr.ls[[lg]][, setdiff(dimnames(out_qa.arr.ls[[lg]])$trust_code, 
                                                                            y = "pooled"
                                                                            ), 
                                                                  , out], 
                                              n = 8
                                              )
                                       )
                          )  # Time-series plots of 'With OPTICA' "comparator" delayed discharges outcomes
                
                matlines(out_qa.arr.ls[[lg]][, setdiff(dimnames(out_qa.arr.ls[[lg]])$trust_code, 
                                                       y = c(optica_status.dat$trust_code, "pooled")
                                                       ), "Without OPTICA", out], 
                         col = 8, lty = 2, lwd = 2
                         )  # Overlay 'Without OPTICA' "comparator" delayed discharges outcomes over time-series plot
                
                with(dimnames(out_qa.arr.ls[[lg]]), 
                     expr = axis(1, 
                                 at = seq_along(get(paste0("date_", lg, "yr"))), 
                                 labels = ifelse(seq_along(get(paste0("date_", lg, "yr"))) %% 2 == 1, 
                                                 get(paste0("date_", lg, "yr")), NA
                                                 ), 
                                 cex.axis = .8, las = 2
                                 )
                     )  # Overlay X-axis labels onto time-series plot
                
                axis(2, 
                     at = pretty(out_qa.arr.ls[[lg]][, setdiff(dimnames(out_qa.arr.ls[[lg]])$trust_code, 
                                                               y = "pooled"
                                                               ), 
                                                     , out], 
                                 n = 8
                                 ), 
                     labels = pretty(out_qa.arr.ls[[lg]][, setdiff(dimnames(out_qa.arr.ls[[lg]])$trust_code, 
                                                                   y = "pooled"
                                                                   ), 
                                                         , out], 
                                     n = 8
                                     ), 
                     cex.axis = .8, las = 2
                     )  # Overlay Y-axis labels onto time-series plot
                
                with(dimnames(out_qa.arr.ls[[lg]]), 
                     expr = abline(v = seq_along(get(paste0("date_", lg, "yr"))
                                                 )[grepl(switch(match(lg, 
                                                                      table = names(out_qa.arr.ls)
                                                                      ), 
                                                                "^52", "^Dec"
                                                                ), 
                                                         x = get(paste0("date_", lg, "yr"))
                                                         )] + .5, 
                                   col = 8, lty = 3
                                   )
                     )  # Overlay vertical grid year delimiters for readability
                
                legend("topleft", 
                       legend = dimnames(out_qa.arr.ls[[lg]])$comparator, 
                       bty = "n", cex = 1, col = c(1, 8), lty = seq.int(2), x.intersp = 1, y.intersp = .5#, inset = -.05, 
                       )  # Overlay legend with "comparator" keys onto time-series plot
                
                dev.print(pdf, 
                          file = file.path(output.dir, 
                                           "Graphs/QA/Descriptive", 
                                           switch(match(out, 
                                                        table = dimnames(out_qa.arr.ls[[lg]])$statistic
                                                        ), 
                                                  "Undischarged out of beds", 
                                                  "Undischarged out of dischargeable"
                                                  ), 
                                           switch(match(lg, table = names(out_qa.arr.ls)), 
                                                  "Weekly", "Monthly"
                                                  ), 
                                           paste(out, lg, "qa.pdf", 
                                                 sep = "_"
                                                 )
                                           ), 
                          width = 1024 / 72, height = 768 / 72
                          )  # Export time-series plot in .pdf format
                
                # dev.print(png, 
                #           file = file.path(output.dir, 
                #                            "Graphs/QA/Descriptive", 
                #                            switch(match(out, 
                #                                         table = dimnames(out_qa.arr.ls[[lg]])$statistic
                #                                         ), 
                #                                   "Undischarged out of beds", 
                #                                   "Undischarged out of dischargeable"
                #                                   ), 
                #                            switch(match(lg, table = names(out_qa.arr.ls)), 
                #                                   "Weekly", "Monthly"
                #                                   ), 
                #                            "OPTICA trusts (calendar time)", 
                #                            paste(out, lg, "qa.png", 
                #                                  sep = "_"
                #                                  )
                #                            ), 
                #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                #           # width = 14, height = 7, units = "in", res = 3e2
                #           # width = 1024, height = 768
                #           )  # Export time-series plot in .png format
                
                # unlink(file.path(output.dir, 
                #                  "Graphs/QA/Descriptive", 
                #                  switch(match(out, 
                #                               table = dimnames(out_qa.arr.ls[[lg]])$statistic
                #                               ), 
                #                         "Undischarged out of beds", 
                #                         "Undischarged out of dischargeable"
                #                         ), 
                #                  switch(match(lg, table = names(out_qa.arr.ls)), 
                #                         "Weekly", "Monthly"
                #                         ), 
                #                  "OPTICA trusts (calendar time)", 
                #                  paste(out, lg, "qa.*", 
                #                        sep = "_"
                #                        )
                #                  )
                #        )  # Remove time-series plot files
                }
         )
       )


## Time-series plots of 
## QAed delayed discharge outcomes 
## vs calendar time 
## across NHS trusts

lapply(names(out_qa_plot.arr.ls2), 
       FUN = function(lg) 
         lapply(dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)$statistic, 
                FUN = function(out) 
                  lapply(dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)$trust_code, 
                         FUN = function(trust)
                           {
                           
                           # lg <- names(out_qa_plot.arr.ls2)[1]; lg
                           # out <- dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)$statistic[2]; out
                           # trust <- dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)$trust_code[55]; trust
                           # rm(lg, out, trust)
                           
                           plot(out_qa_plot.arr.ls2[[lg]]$nopooled[, trust, out], 
                                type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                                col = 1, lty = 1, pch = 16, 
                                main = paste("Proportions of", 
                                             switch(match(out, 
                                                          table = dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)$statistic
                                                          ), 
                                                    "Beds Occupied by Patients Fit for Discharge\n", 
                                                    "Undischarged Patients Fit for Discharge\n"
                                                    ), 
                                             "from the", trust, "Acute NHS Trust in England", 
                                             sep = " "
                                             ), 
                                sub = paste(trust, "Trust", sep = " "), 
                                xlab = switch(match(lg, table = names(out_qa_plot.arr.ls2)), 
                                              "Week", "Month"
                                              ), 
                                ylab = "Proportion of undischarged patients (%)", 
                                ylim = range(pretty(c(out_qa_plot.arr.ls2[[lg]]$nopooled[, trust, out], 
                                                      out_qa_plot.arr.ls2[[lg]]$pooled[, paste0(c(25, 75), "%"), 
                                                                                       "Without OPTICA", out]
                                                      ), 
                                                    n = 8
                                                    )
                                             )
                                )  # Time-series plots of QAed delayed discharge outcomes
                           
                           # lines(predict(loess(prop ~ I(seq_along(get(paste0("date_", lg, "yr")))), 
                           #                     data = out_qa_plot.dat.ls2[[lg]]$nopooled, 
                           #                     subset = trust_code == trust & statistic == out, 
                           #                     na.action = na.exclude
                           #                     )
                           #               ), 
                           #       col = 1, lty = 2, lwd = 1
                           #       )  # Overlay LOESS point predictions over time-series plot
                           
                           lines(out_qa_plot.arr.ls2[[lg]]$pooled[, "50%", "Without OPTICA", out], 
                                 col = 2, lty = 4, lwd = 1, pch = 1
                                 )  # Overlay 'Without OPTICA' "comparator" median outcomes over time-series plot
                           
                           polygon(with(dimnames(out_qa_plot.arr.ls2[[lg]]$pooled), 
                                        expr = c(seq_along(get(paste0("date_", lg, "yr"))), 
                                                 rev(seq_along(get(paste0("date_", lg, "yr"))))
                                                 )
                                        ), 
                                   c(out_qa_plot.arr.ls2[[lg]]$pooled[, "75%", "Without OPTICA", out], 
                                         rev(out_qa_plot.arr.ls2[[lg]]$pooled[, "25%", "Without OPTICA", out])
                                     ), 
                                   density = 90, angle = NULL, col = rgb(1, 0, 0, alpha = .2), border = NA
                                   )  # Overlay 'Without OPTICA' "comparator" IQR outcomes over time-series plot
                           
                           with(dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled), 
                                expr = axis(1, 
                                            at = seq_along(get(paste0("date_", lg, "yr"))), 
                                            labels = ifelse(seq_along(get(paste0("date_", lg, "yr"))) %% 2 == 1, 
                                                            get(paste0("date_", lg, "yr")), 
                                                            NA
                                                            ), 
                                            cex.axis = .8, las = 2
                                            )
                                )  # Overlay X-axis labels onto time-series plot
                           
                           axis(2, 
                                at = pretty(c(out_qa_plot.arr.ls2[[lg]]$nopooled[, trust, out], 
                                              out_qa_plot.arr.ls2[[lg]]$pooled[, paste0(c(25, 75), "%"), 
                                                                               "Without OPTICA", out]
                                              ), 
                                            n = 8
                                            ), 
                                labels = pretty(c(out_qa_plot.arr.ls2[[lg]]$nopooled[, trust, out], 
                                                  out_qa_plot.arr.ls2[[lg]]$pooled[, paste0(c(25, 75), "%"), 
                                                                                   "Without OPTICA", out]
                                                  ), 
                                                n = 8
                                                ), 
                                cex.axis = .8, las = 2
                                )  # Overlay Y-axis labels onto time-series plot
                           
                           if(trust %in% optica_status.dat$trust_code)
                             {
                             abline(v = match(with(optica_status.dat, 
                                                   expr = 
                                                     get(paste0("start_", lg, "yr"))[match(trust, 
                                                                                           table = trust_code
                                                                                           )]
                                                   ), 
                                              table = dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)[[paste0("date_", lg, "yr")]]
                                              ), 
                                    col = 8, lty = 2, lwd = 2
                                    )  # Overlay vertical line at trust's OPTICA start date
                             
                             text(match(with(optica_status.dat, 
                                             expr = 
                                               get(paste0("start_", lg, "yr"))[match(trust, 
                                                                                     table = trust_code
                                                                                     )]
                                             ), 
                                        table = dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)[[paste0("date_", lg, "yr")]]
                                        ), 
                                  max(pretty(c(out_qa_plot.arr.ls2[[lg]]$nopooled[, trust, out], 
                                               out_qa_plot.arr.ls2[[lg]]$pooled[, paste0(c(25, 75), "%"), 
                                                                                "Without OPTICA", out]
                                               ), 
                                             n = 8
                                             )
                                      ), 
                                  labels = "OPTICA\ndeployed", 
                                  cex = 1, col = 8, pos = 4, adj = c(.5, -.1)
                                  )  # Overlay OPTICA start date onto time-series plot
                             }
                           
                           with(dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled), 
                                expr = abline(v = seq_along(get(paste0("date_", lg, "yr"))
                                                            )[grepl(switch(match(lg, 
                                                                                 table = names(out_qa_plot.arr.ls2)
                                                                                 ), 
                                                                           "^52", "^Dec"
                                                                           ), 
                                                                    x = get(paste0("date_", lg, "yr"))
                                                                    )] + .5, 
                                              col = 8, lty = 3
                                              )
                                )  # Overlay vertical grid year delimiters for readability
                           
                           # legend("topleft", 
                           #        legend = c("Observed", "Smoothed"), 
                           #        bty = "n", cex = 1, lty = seq.int(2), pch = c(16, NA)#, x.intersp = 1, y.intersp = .5
                           #        )  # Overlay legend with "statistic" keys onto time-series plot
                           
                           legend("topleft", 
                                  legend = c("Observed", "Non-OPTICA (IQR)"), 
                                  bty = "n", cex = 1, col = seq.int(2), lty = c(1, 4), pch = c(16, 1), x.intersp = 1, y.intersp = .5
                                  )  # Overlay legend with "statistic" keys onto time-series plot
                           
                           dev.print(pdf, 
                                     file = file.path(output.dir, 
                                                      "Graphs/QA/Descriptive", 
                                                      switch(match(out, 
                                                                   table = dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)$statistic
                                                                   ), 
                                                             "Undischarged out of beds", 
                                                             "Undischarged out of dischargeable"
                                                             ), 
                                                      switch(match(lg, table = names(out_qa_plot.arr.ls2)), 
                                                             "Weekly", "Monthly"
                                                             ), 
                                                      ifelse(trust %in% optica_status.dat$trust_code, 
                                                             "OPTICA trusts (calendar time)", 
                                                             "Non-OPTICA trusts"
                                                             ), 
                                                      paste(paste(out, lg, trust, "qa", 
                                                                  ifelse(trust %in% optica_status.dat$trust_code, 
                                                                         "opt", "noopt"
                                                                         ), 
                                                                  sep = "_"
                                                                  ), 
                                                            "pdf", 
                                                            sep = "."
                                                            )
                                                      ), 
                                     width = 1024 / 72, height = 768 / 72
                                     )  # Export time-series plot in .pdf format
                           
                           # dev.print(png, 
                           #           file = file.path(output.dir, 
                           #                            "Graphs/QA/Descriptive", 
                           #                            switch(match(out, 
                           #                                         table = dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)$statistic
                           #                                         ), 
                           #                                   "Undischarged out of beds", 
                           #                                   "Undischarged out of dischargeable"
                           #                                   ), 
                           #                            switch(match(lg, table = names(out_qa_plot.arr.ls2)), 
                           #                                   "Weekly", "Monthly"
                           #                                   ), 
                           #                            ifelse(trust %in% optica_status.dat$trust_code, 
                           #                                   "OPTICA trusts (calendar time)", 
                           #                                   "Non-OPTICA trusts"
                           #                                   ), 
                           #                            paste(paste(out, lg, trust, "qa", 
                           #                                        ifelse(trust %in% optica_status.dat$trust_code, 
                           #                                               "opt", "noopt"
                           #                                               ), 
                           #                                        sep = "_"
                           #                                        ), 
                           #                                  "png", 
                           #                                  sep = "."
                           #                                  )
                           #                            ), 
                           #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                           #           # width = 14, height = 7, units = "in", res = 3e2
                           #           # width = 1024, height = 768
                           #           )  # Export time-series plot in .png format
                           
                           # unlink(file.path(output.dir, 
                           #                  "Graphs/QA/Descriptive", 
                           #                  switch(match(out, 
                           #                               table = dimnames(out_qa_plot.arr.ls2[[lg]]$nopooled)$statistic
                           #                               ), 
                           #                         "Undischarged out of beds", 
                           #                         "Undischarged out of dischargeable"
                           #                         ), 
                           #                  switch(match(lg, table = names(out_qa_plot.arr.ls2)), 
                           #                         "Weekly", "Monthly"
                           #                         ), 
                           #                  ifelse(trust %in% optica_status.dat$trust_code, 
                           #                         "OPTICA trusts (calendar time)", 
                           #                         "Non-OPTICA trusts"
                           #                         ), 
                           #                  paste(paste(out, lg, trust, "qa", 
                           #                              ifelse(trust %in% optica_status.dat$trust_code, 
                           #                                     "opt", "noopt"
                           #                                     ), 
                           #                              sep = "_"
                           #                              ), 
                           #                        "*", 
                           #                        sep = "."
                           #                        )
                           #                  )
                           #        )  # Remove time-series plot files
                           }
                         )
                )
       )


## Time-series plots of 
## delayed discharge outcomes 
## vs calendar time 
## across NHS trusts

lapply(names(out_qa.arr.ls), 
       FUN = function(lg) 
         lapply(dimnames(out_qa.arr.ls[[lg]])$statistic, 
                FUN = function(out)
                  {
                  lapply(split(optica_status.dat$trust_code, 
                               f = gl(length(optica_status.dat$trust_code) %/% 5, 
                                      k = 5
                                      )
                               ), 
                         FUN = function(trust.vec)
                           {
                           matplot(out_qa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                   type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                                   col = hcl.colors(length(trust.vec), 
                                                    palette = hcl.pals("qualitative")[3]
                                                    ), 
                                   lty = 1, pch = 16, 
                                   main = paste("Proportions of", 
                                                switch(match(out, table = dimnames(out_qa.arr.ls[[lg]])$statistic), 
                                                       "Beds Occupied by", "Undischarged"
                                                       ), 
                                                "Patients Medically Fit for Discharge\nfrom Acute NHS Trusts in England", 
                                                sep = " "
                                                ), 
                                   sub = "OPTICA trusts", 
                                   xlab = switch(match(lg, table = names(out_qa.arr.ls)), 
                                                 "Week", "Month"
                                                 ), 
                                   ylab = "Proportion of undischarged patients (%)", 
                                   ylim = range(pretty(c(out_qa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                                         out_qa_plot.arr.ls2[[lg]]$pooled[, paste0(c(25, 75), "%"), 
                                                                                          "Without OPTICA", out]
                                                         ), 
                                                       n = 8
                                                       )
                                                )
                                   )  # Time-series plots of 'With OPTICA' "comparator" delayed discharges outcomes
                           
                           # matlines(sapply(trust.vec, 
                           #                 FUN = function(trust) 
                           #                   predict(loess(prop ~ I(seq_along(get(paste0("date_", lg, "yr")))), 
                           #                                 data = out_qa.dat.ls[[lg]], 
                           #                                 subset = trust_code == trust & 
                           #                                   comparator == "With OPTICA" & 
                           #                                   statistic == out, 
                           #                                 na.action = na.exclude
                           #                                 )
                           #                           ), 
                           #                 simplify = "array"
                           #                 ), 
                           #          col = hcl.colors(length(trust.vec), 
                           #                           palette = hcl.pals("qualitative")[3]
                           #                           ), 
                           #          lty = 2, lwd = 1
                           #          )  # Overlay LOESS point predictions over time-series plot
                           
                           with(dimnames(out_qa.arr.ls[[lg]]), 
                                expr = axis(1, 
                                            at = seq_along(get(paste0("date_", lg, "yr"))), 
                                            labels = ifelse(seq_along(get(paste0("date_", lg, "yr"))) %% 2 == 1, 
                                                            get(paste0("date_", lg, "yr")), NA
                                                            ), 
                                            cex.axis = .8, las = 2
                                            )
                                )  # Overlay X-axis labels onto time-series plot
                           
                           axis(2, 
                                at = pretty(c(out_qa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                              out_qa_plot.arr.ls2[[lg]]$pooled[, paste0(c(25, 75), "%"), 
                                                                               "Without OPTICA", out]
                                              ), 
                                            n = 8
                                            ), 
                                labels = pretty(c(out_qa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                                  out_qa_plot.arr.ls2[[lg]]$pooled[, paste0(c(25, 75), "%"), 
                                                                                   "Without OPTICA", out]
                                                  ), 
                                                n = 8
                                                ), 
                                cex.axis = .8, las = 2
                                )  # Overlay Y-axis labels onto time-series plot
                           
                           lines(out_qa_plot.arr.ls2[[lg]]$pooled[, "50%", "Without OPTICA", out], 
                                 col = 2, lty = 4, lwd = 1, pch = 1
                                 )  # Overlay 'Without OPTICA' "comparator" median outcomes over time-series plot
                           
                           polygon(with(dimnames(out_qa_plot.arr.ls2[[lg]]$pooled), 
                                        expr = c(seq_along(get(paste0("date_", lg, "yr"))), 
                                                 rev(seq_along(get(paste0("date_", lg, "yr"))))
                                                 )
                                        ), 
                                   c(out_qa_plot.arr.ls2[[lg]]$pooled[, "75%", "Without OPTICA", out], 
                                     rev(out_qa_plot.arr.ls2[[lg]]$pooled[, "25%", "Without OPTICA", out])
                                     ), 
                                   density = 90, angle = NULL, col = rgb(1, 0, 0, alpha = .2), border = NA
                                   )  # Overlay 'Without OPTICA' "comparator" IQR outcomes over time-series plot
                           
                           abline(v = match(with(optica_status.dat, 
                                                 expr = 
                                                   get(paste0("start_", lg, "yr"))[match(trust.vec, 
                                                                                         table = trust_code
                                                                                         )]
                                                 ), 
                                            table = dimnames(out_qa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]
                                            ), 
                                  col = hcl.colors(length(trust.vec), 
                                                   palette = hcl.pals("qualitative")[3]
                                                   ), 
                                  lty = 2, lwd = 2
                                  )  # Overlay vertical line at trust's OPTICA start date
                           
                           text(match(with(optica_status.dat, 
                                           expr = 
                                             get(paste0("start_", lg, "yr"))[match(trust.vec, 
                                                                                   table = trust_code
                                                                                   )]
                                           ), 
                                      table = dimnames(out_qa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]
                                      ), 
                                max(pretty(out_qa.arr.ls[[lg]][, trust.vec, "With OPTICA", out], 
                                           n = 8
                                           )
                                    ), 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 4#, adj = c(.5, -.1), cex = .8, col = 8
                                )  # Overlay OPTICA start date onto time-series plot
                           
                           with(dimnames(out_qa.arr.ls[[lg]]), 
                                expr = abline(v = seq_along(get(paste0("date_", lg, "yr"))
                                                            )[grepl(switch(match(lg, 
                                                                                 table = names(out_qa.arr.ls)
                                                                                 ), 
                                                                           "^52", "^Dec"
                                                                           ), 
                                                                    x = get(paste0("date_", lg, "yr"))
                                                                    )] + .5, 
                                              col = 8, lty = 3
                                              )
                                )  # Overlay vertical grid year delimiters for readability
                           
                           text(1, 
                                out_qa.arr.ls[[lg]][1, trust.vec, "With OPTICA", out], 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 2, cex = .75
                                )  # Overlay "trust_code" onto time-series plot
                           
                           text(length(dimnames(out_qa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]), 
                                out_qa.arr.ls[[lg]][length(dimnames(out_qa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]), 
                                                    trust.vec, "With OPTICA", out], 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 4, cex = .75
                                )  # Overlay "trust_code" onto time-series plot
                           
                           # legend("topleft", 
                           #        legend = c("Observed", "Smoothed"), 
                           #        bty = "n", cex = 1, lty = seq.int(2), pch = c(16, NA)#, x.intersp = 1, y.intersp = .5
                           #        )  # Overlay legend with "statistic" keys onto time-series plot
                           
                           legend("topleft", 
                                  legend = c("Observed", "Non-OPTICA (IQR)"), 
                                  bty = "n", cex = 1, col = seq.int(2), lty = c(1, 4), pch = c(16, 1)#, x.intersp = 1, y.intersp = .5
                                  )  # Overlay legend with "statistic" keys onto time-series plot
                           
                           dev.print(pdf, 
                                     file = file.path(output.dir, 
                                                      "Graphs/QA/Descriptive", 
                                                      switch(match(out, 
                                                                   table = dimnames(out_qa.arr.ls[[lg]])$statistic
                                                                   ), 
                                                             "Undischarged out of beds", 
                                                             "Undischarged out of dischargeable"
                                                             ), 
                                                      switch(match(lg, table = names(out_qa.arr.ls)), 
                                                             "Weekly", "Monthly"
                                                             ), 
                                                      "OPTICA trusts (calendar time)", 
                                                      paste(out, lg, 
                                                            paste(trust.vec, collapse = "."), 
                                                            "qa_opt.pdf", 
                                                            sep = "_"
                                                            )
                                                      ), 
                                     width = 1024 / 72, height = 768 / 72
                                     )  # Export time-series plot in .pdf format
                           
                           # dev.print(png, 
                           #           file = file.path(output.dir, 
                           #                            "Graphs/QA/Descriptive", 
                           #                            switch(match(out, 
                           #                                         table = dimnames(out_qa.arr.ls[[lg]])$statistic
                           #                                         ), 
                           #                                   "Undischarged out of beds", 
                           #                                   "Undischarged out of dischargeable"
                           #                                   ), 
                           #                            switch(match(lg, table = names(out_qa.arr.ls)), 
                           #                                   "Weekly", "Monthly"
                           #                                   ), 
                           #                            "OPTICA trusts (calendar time)", 
                           #                            paste(out, lg, 
                           #                                  paste(trust.vec, collapse = "."),
                           #                                  "qa_opt.png", 
                           #                                  sep = "_"
                           #                                  )
                           #                            ), 
                           #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                           #           # width = 14, height = 7, units = "in", res = 3e2
                           #           # width = 1024, height = 768
                           #           )  # Export time-series plot in .png format
                           
                           # unlink(file.path(output.dir, 
                           #                  "Graphs/QA/Descriptive", 
                           #                  switch(match(out, 
                           #                               table = dimnames(out_qa.arr.ls[[lg]])$statistic
                           #                               ), 
                           #                         "Undischarged out of beds", 
                           #                         "Undischarged out of dischargeable"
                           #                         ), 
                           #                  switch(match(lg, table = names(out_qa.arr.ls)), 
                           #                         "Weekly", "Monthly"
                           #                         ), 
                           #                  "OPTICA trusts (calendar time)", 
                           #                  paste(out, lg, 
                           #                        paste(trust.vec, collapse = "."), 
                           #                        "qa_opt.*", 
                           #                        sep = "_"
                           #                        )
                           #                  )
                           #        )  # Remove time-series plot files
                           }
                         )
                  
                  
                  lapply(split(setdiff(dimnames(out_qa.arr.ls[[lg]])$trust_code, 
                                       y = c("pooled", optica_status.dat$trust_code)
                                       ), 
                               f = gl(length(setdiff(dimnames(out_qa.arr.ls[[lg]])$trust_code, 
                                                     y = c("pooled", optica_status.dat$trust_code)
                                                     )
                                             ) %/% 3, 
                                      k = 3
                                      )
                               ), 
                         FUN = function(trust.vec)
                           {
                           matplot(out_qa.arr.ls[[lg]][, trust.vec, "Without OPTICA", out], 
                                   type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                                   col = hcl.colors(length(trust.vec), 
                                                    palette = hcl.pals("qualitative")[3]
                                                    ), 
                                   lty = 1, pch = 16, 
                                   main = paste("Proportions of", 
                                                switch(match(out, table = dimnames(out_qa.arr.ls[[lg]])$statistic), 
                                                       "Beds Occupied by", "Undischarged"
                                                       ), 
                                                "Patients Medically Fit for Discharge\nfrom Acute NHS Trusts in England", 
                                                sep = " "
                                                ), 
                                   sub = "Non-OPTICA trusts", 
                                   xlab = switch(match(lg, table = names(out_qa.arr.ls)), 
                                                 "Week", "Month"
                                                 ), 
                                   ylab = "Proportion of undischarged patients (%)", 
                                   ylim = range(pretty(out_qa.arr.ls[[lg]][, trust.vec, "Without OPTICA", out], 
                                                       n = 8
                                                       )
                                                )
                                   )  # Time-series plots of 'Without OPTICA' "comparator" delayed discharges outcomes
                           
                           # matlines(sapply(trust.vec, 
                           #                 FUN = function(trust) 
                           #                   predict(loess(prop ~ I(seq_along(get(paste0("date_", lg, "yr")))), 
                           #                                 data = out_qa.dat.ls[[lg]], 
                           #                                 subset = trust_code == trust & 
                           #                                   comparator == "Without OPTICA" & 
                           #                                   statistic == out, 
                           #                                 na.action = na.exclude
                           #                                 )
                           #                           ), 
                           #                 simplify = "array"
                           #                 ), 
                           #          col = hcl.colors(length(trust.vec), 
                           #                           palette = hcl.pals("qualitative")[3]
                           #                           ), 
                           #          lty = 2, lwd = 1
                           #          )  # Overlay LOESS point predictions over time-series plot
                           
                           with(dimnames(out_qa.arr.ls[[lg]]), 
                                expr = axis(1, 
                                            at = seq_along(get(paste0("date_", lg, "yr"))), 
                                            labels = ifelse(seq_along(get(paste0("date_", lg, "yr"))) %% 2 == 1, 
                                                            get(paste0("date_", lg, "yr")), NA
                                                            ), 
                                            cex.axis = .8, las = 2
                                            )
                                )  # Overlay X-axis labels onto time-series plot
                           
                           axis(2, 
                                at = pretty(out_qa.arr.ls[[lg]][, trust.vec, "Without OPTICA", out], 
                                            n = 8
                                            ), 
                                labels = pretty(out_qa.arr.ls[[lg]][, trust.vec, "Without OPTICA", out], 
                                                n = 8
                                                ), 
                                cex.axis = .8, las = 2
                                )  # Overlay Y-axis labels onto time-series plot
                           
                           with(dimnames(out_qa.arr.ls[[lg]]), 
                                expr = abline(v = seq_along(get(paste0("date_", lg, "yr"))
                                                            )[grepl(switch(match(lg, 
                                                                                 table = names(out_qa.arr.ls)
                                                                                 ), 
                                                                           "^52", "^Dec"
                                                                           ), 
                                                                    x = get(paste0("date_", lg, "yr"))
                                                                    )] + .5, 
                                              col = 8, lty = 3
                                              )
                                )  # Overlay vertical grid year delimiters for readability
                           
                           text(1, 
                                out_qa.arr.ls[[lg]][1, trust.vec, "Without OPTICA", out], 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 2, cex = .75
                                )  # Overlay "trust_code" onto time-series plot
                           
                           text(length(dimnames(out_qa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]), 
                                out_qa.arr.ls[[lg]][length(dimnames(out_qa.arr.ls[[lg]])[[paste0("date_", lg, "yr")]]), 
                                                    trust.vec, "Without OPTICA", out
                                                    ], 
                                labels = trust.vec, 
                                col = hcl.colors(length(trust.vec), 
                                                 palette = hcl.pals("qualitative")[3]
                                                 ), 
                                pos = 4, cex = .75
                                )  # Overlay "trust_code" onto time-series plot
                           
                           legend("topleft", 
                                  legend = c("Observed", "Smoothed"), 
                                  bty = "n", cex = 1, lty = seq.int(2), pch = c(16, NA)#, x.intersp = 1, y.intersp = .5
                                  )  # Overlay legend with "statistic" keys onto time-series plot
                           
                           dev.print(pdf, 
                                     file = file.path(output.dir, 
                                                      "Graphs/QA/Descriptive", 
                                                      switch(match(out, 
                                                                   table = dimnames(out_qa.arr.ls[[lg]])$statistic
                                                                   ), 
                                                             "Undischarged out of beds", 
                                                             "Undischarged out of dischargeable"
                                                             ), 
                                                      switch(match(lg, table = names(out_qa.arr.ls)), 
                                                             "Weekly", "Monthly"
                                                             ), 
                                                      "Non-OPTICA trusts", 
                                                      paste(out, lg, 
                                                            paste(trust.vec, collapse = "."), 
                                                            "qa_noopt.pdf", 
                                                            sep = "_"
                                                            )
                                                      ), 
                                     width = 1024 / 72, height = 768 / 72
                                     )  # Export time-series plot in .pdf format
                           
                           # dev.print(png, 
                           #           file = file.path(output.dir, 
                           #                            "Graphs/QA/Descriptive", 
                           #                            switch(match(out, 
                           #                                         table = dimnames(out_qa.arr.ls[[lg]])$statistic
                           #                                         ), 
                           #                                   "Undischarged out of beds", 
                           #                                   "Undischarged out of dischargeable"
                           #                                   ), 
                           #                            switch(match(lg, table = names(out_qa.arr.ls)), 
                           #                                   "Weekly", "Monthly"
                           #                                   ), 
                           #                            "Non-OPTICA trusts", 
                           #                            paste(out, lg, 
                           #                                  paste(trust.vec, collapse = "."), 
                           #                                  "qa_noopt.png", 
                           #                                  sep = "_"
                           #                                  )
                           #                            ), 
                           #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                           #           # width = 14, height = 7, units = "in", res = 3e2
                           #           # width = 1024, height = 768
                           #           )  # Export time-series plot in .png format
                           
                           # unlink(file.path(output.dir, 
                           #                  "Graphs/QA/Descriptive", 
                           #                  switch(match(out, 
                           #                               table = dimnames(out_qa.arr.ls[[lg]])$statistic
                           #                              ), 
                           #                         "Undischarged out of beds", 
                           #                         "Undischarged out of dischargeable"
                           #                         ), 
                           #                  switch(match(lg, table = names(out_qa.arr.ls)), 
                           #                         "Weekly", "Monthly"
                           #                         ), 
                           #                  "Non-OPTICA trusts", 
                           #                  paste(out, lg, 
                           #                        paste(trust.vec, collapse = "."), 
                           #                        "qa_noopt.*", 
                           #                        sep = "_"
                           #                        )
                           #                  )
                           #        )  # Remove time-series plot files
                           }
                         )
                  
                  
                  matplot(out_qa.arr.ls[[lg]][, "pooled", , out], 
                          type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                          col = c(4, 2), lty = 1, pch = 16, 
                          main = paste("Proportions of", 
                                       switch(match(out, table = dimnames(out_qa.arr.ls[[lg]])$statistic), 
                                              "Beds Occupied by", "Undischarged"
                                              ), 
                                       "Patients Medically Fit for Discharge\nfrom Acute NHS Trusts in England", 
                                       sep = " "
                                       ), 
                          sub = "Pooled trusts", 
                          xlab = switch(match(lg, table = names(out_qa.arr.ls)), 
                                        "Week", "Month"
                                        ), 
                          ylab = "Proportion of undischarged patients (%)", 
                          ylim = range(pretty(out_qa.arr.ls[[lg]][, "pooled", , out], 
                                              n = 8
                                              )
                                       )
                          )  # Time-series plots of key delayed discharge outcomes
                  
                  # matlines(sapply(dimnames(out_qa.arr.ls[[lg]])$comparator, 
                  #                 FUN = function(grp) 
                  #                   predict(loess(prop ~ I(seq_along(get(paste0("date_", lg, "yr")))), 
                  #                                 data = out_qa.dat.ls[[lg]], 
                  #                                 subset = trust_code == "pooled" & 
                  #                                   comparator == grp & 
                  #                                   statistic == out, 
                  #                                 na.action = na.exclude
                  #                                 )
                  #                           ), 
                  #                 simplify = "array"
                  #                 ), 
                  #          col = c(4, 2), lty = 2, lwd = 1
                  #          )  # Overlay LOESS point predictions by "comparator" over time-series plot
                  
                  with(dimnames(out_qa.arr.ls[[lg]]), 
                       expr = axis(1, 
                                   at = seq_along(get(paste0("date_", lg, "yr"))), 
                                   labels = ifelse(seq_along(get(paste0("date_", lg, "yr"))) %% 2 == 1, 
                                                   get(paste0("date_", lg, "yr")), NA
                                                   ), 
                                   cex.axis = .8, las = 2
                                   )
                       )  # Overlay X-axis labels onto time-series plot
                  
                  axis(2, 
                       at = pretty(out_qa.arr.ls[[lg]][, "pooled", , out], 
                                   n = 8
                                   ), 
                       labels = pretty(out_qa.arr.ls[[lg]][, "pooled", , out], 
                                       n = 8
                                       ), 
                       cex.axis = .8, las = 2
                       )  # Overlay Y-axis labels onto time-series plot
                  
                  with(dimnames(out_qa.arr.ls[[lg]]), 
                       expr = abline(v = seq_along(get(paste0("date_", lg, "yr"))
                                                   )[grepl(switch(match(lg, 
                                                                        table = names(out_qa.arr.ls)
                                                                        ), 
                                                                  "^52", "^Dec"
                                                                  ), 
                                                           x = get(paste0("date_", lg, "yr"))
                                                           )] + .5, 
                                     col = 8, lty = 3
                                     )
                       )  # Overlay vertical grid lines for readability
                  
                  # legend("topleft", 
                  #        legend = c("Observed", "Smoothed"), 
                  #        bty = "n", cex = 1, lty = seq.int(2), x.intersp = 1, y.intersp = .5
                  #        )  # Overlay legend with "comparator" keys onto time-series plot
                  
                  legend("topright", 
                         legend = dimnames(out_qa.arr.ls[[lg]])$comparator, 
                         bty = "n", cex = 1, fill = c(4, 2)#, x.intersp = 1, y.intersp = .8, inset = -.05, 
                         )  # Overlay legend with "comparator" keys onto time-series plot
                  
                  dev.print(pdf, 
                            file = file.path(output.dir, 
                                             "Graphs/QA/Descriptive", 
                                             switch(match(out, 
                                                          table = dimnames(out_qa.arr.ls[[lg]])$statistic
                                                          ), 
                                                    "Undischarged out of beds", 
                                                    "Undischarged out of dischargeable"
                                                    ), 
                                             switch(match(lg, table = names(out_qa.arr.ls)), 
                                                    "Weekly", "Monthly"
                                                    ), 
                                             "Pooled trusts", 
                                             paste(out, lg, "pooled_qa.pdf", sep = "_")
                                             ), 
                            width = 1024 / 72, height = 768 / 72
                            )  # Export time-series plot in .pdf format
                  
                  # dev.print(png, 
                  #           file = file.path(output.dir, 
                  #                            "Graphs/QA/Descriptive", 
                  #                            switch(match(out, 
                  #                                         table = dimnames(out_qa.arr.ls[[lg]])$statistic
                  #                                         ), 
                  #                                   "Undischarged out of beds", 
                  #                                   "Undischarged out of dischargeable"
                  #                                   ), 
                  #                            switch(match(lg, table = names(out_qa.arr.ls)), 
                  #                                   "Weekly", "Monthly"
                  #                                  ), 
                  #                            "Pooled trusts", 
                  #                            paste(out, lg, "pooled_qa.png", sep = "_")
                  #                            ), 
                  #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                  #           # width = 14, height = 7, units = "in", res = 3e2
                  #           # width = 1024, height = 768
                  #           )  # Export time-series plot in .png format
                  
                  # unlink(file.path(output.dir, 
                  #                  "Graphs/QA/Descriptive", 
                  #                  switch(match(out, 
                  #                               table = dimnames(out_qa.arr.ls[[lg]])$statistic
                  #                               ), 
                  #                         "Undischarged out of beds", 
                  #                         "Undischarged out of dischargeable"
                  #                         ), 
                  #                  "Pooled trusts", 
                  #                  paste(out, lg, "pooled_qa.*", sep = "_")
                  #                  )
                  #        )  # Remove time-series plot files
                  }
                )
       )


## Time-series plots of 
## delayed discharge outcomes 
## vs lapsed time 
## across NHS trusts

lapply(names(out_stg_qa.arr.ls), 
       FUN = function(lg) 
         lapply(dimnames(out_stg_qa.arr.ls[[lg]])$statistic, 
                FUN = function(out) 
                  lapply(dimnames(out_stg_qa.arr.ls[[lg]])$trust_code, 
                         FUN = function(trust)
                           {
                           
                           # lg <- names(out_stg_qa.arr.ls)[2]; lg
                           # out <- dimnames(out_stg_qa.arr.ls[[lg]])$statistic[1]; out
                           # trust <- dimnames(out_stg_qa.arr.ls[[lg]])$trust_code[5]; trust
                           # rm(lg, out, trust)
                           
                           thin_stg.ls <- list(lower = which(! is.na(out_stg_qa.arr.ls[[lg]][, trust, out]))[1], 
                                               upper = nrow(out_stg_qa.arr.ls[[lg]]) - 
                                                 which(rev(! is.na(out_stg_qa.arr.ls[[lg]][, trust, out])))[1]
                                               )  # Derive list of redundant NA cut-offs for QAed staggered delayed discharge outcomes vector
                           
                           out_stg_thin.vec <- out_stg_qa.arr.ls[[lg]][with(thin_stg.ls, 
                                                                            expr = seq.int(from = lower, to = upper)
                                                                            ), 
                                                                       trust, out]  # Derive vector of QAed staggered thinned delayed discharge outcomes
                           
                           
                           out_stg_thin.dat <- setNames(data.frame(names(out_stg_thin.vec), out_stg_thin.vec), 
                                                        nm = c(paste("start", lg, sep = "_"), "prop")
                                                        )  # Format staggered thinned delayed discharge outcomes vector as data-frame
                           
                           
                           plot(out_stg_thin.vec, 
                                type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                                col = 1, col.sub = 4, lty = 1, pch = 16, 
                                main = paste("Proportions of", 
                                             switch(match(out, 
                                                          table = dimnames(out_stg_qa.arr.ls[[lg]])$statistic
                                                          ), 
                                                    "Beds Occupied by Patients Fit for Discharge\n", 
                                                    "Undischarged Patients Fit for Discharge\n"
                                                    ), 
                                             "from", 
                                             ifelse(trust == "pooled", 
                                                    "Acute NHS Trusts", 
                                                    paste("the", trust, "Acute NHS Trust", sep =" ")
                                                    ), 
                                             "Deploying OPTICA in England", 
                                             sep = " "
                                             ), 
                                sub = "OPTICA-deploying trusts", 
                                xlab = paste(switch(match(lg, table = names(out_stg_qa.arr.ls)), 
                                                    "Weeks", "Months"
                                                    ), 
                                             "lapsed from OPTICA deployment", 
                                             sep = " "
                                             ), 
                                ylab = "Proportion of undischarged patients (%)", 
                                ylim = range(pretty(out_stg_thin.vec, n = 8))
                                )  # Time-series plots of key QAed staggered delayed discharge outcomes
                           
                           # lines(predict(loess(prop ~ I(seq_along(get(paste("start", lg, sep = "_")))), 
                           #                          data = out_stg_thin.dat, 
                           #                          na.action = na.exclude
                           #                     )
                           #               ), 
                           #       col = 1, lty = 2, lwd = 1
                           #       )  # Overlay LOESS point predictions over time-series plot
                           
                           axis(1, 
                                at = seq_along(out_stg_thin.vec), 
                                labels = c(ifelse(as.integer(names(out_stg_thin.vec)[as.integer(names(out_stg_thin.vec)) < 0]) %% 2 == 1, 
                                                  names(out_stg_thin.vec)[as.integer(names(out_stg_thin.vec)) < 0], NA), 
                                           ifelse(as.integer(names(out_stg_thin.vec)[as.integer(names(out_stg_thin.vec)) > 0]) %% 2 == 1, 
                                                  names(out_stg_thin.vec)[as.integer(names(out_stg_thin.vec)) > 0], NA)
                                           ), 
                                cex.axis = .8, las = 2
                                )  # Overlay X-axis labels onto time-series plot
                           
                           axis(2, 
                                at = pretty(out_stg_thin.vec, n = 8), 
                                labels = pretty(out_stg_thin.vec, n = 8), 
                                cex.axis = .8, las = 2
                                )  # Overlay Y-axis labels onto time-series plot
                           
                           text(seq_along(out_stg_thin.vec), 
                                out_stg_thin.vec, 
                                labels = rowSums(! is.na(out_stg_qa.arr.ls[[lg]][, optica_status.dat$trust_code, out])
                                                 )[names(out_stg_thin.vec)], 
                                cex = .8, col = 4, pos = 3#, adj = c(.5, -.1)
                                )  # Overlay OPTICA-deploying trust sizes onto time-series plot
                           
                           
                           abline(v = mean(match(as.character(c(-1, 1)), 
                                                 table = names(out_stg_thin.vec)
                                                 )
                                           ), 
                                  col = 8, lty = 4, lwd = 2
                                  )  # Overlay intervention start vertical delimiter
                           
                           text(mean(match(as.character(c(-1, 1)), 
                                           table = names(out_stg_thin.vec)
                                           )
                                     ), 
                                max(pretty(out_stg_thin.vec, n = 8)), 
                                labels = "OPTICA\ndeployed", 
                                cex = 1, col = 8, pos = 4#, adj = c(.5, -.1)
                                )  # Overlay OPTICA start date onto time-series plot
                           
                           abline(v = c(if(as.integer(names(out_stg_thin.vec))[1] < 0) 
                             rev(seq.int(which(as.integer(names(out_stg_thin.vec)) == "-1"), 
                                         1, 
                                         by = - switch(match(lg, 
                                                             table = names(out_stg_qa.arr.ls)
                                                             ), 52, 12
                                                       )
                                         )[-1] + 1
                                 ), 
                             seq.int(which(as.integer(names(out_stg_thin.vec)) == "1"), 
                                     length(names(out_stg_thin.vec)), 
                                     by = switch(match(lg, 
                                                       table = names(out_stg_qa.arr.ls)
                                                       ), 52, 12
                                                 )
                                     )[-1] - 1
                             ), 
                             col = 8, lty = 2, lwd = 2
                             )  # Overlay vertical grid lapsed year delimiters for readability
                           
                           # abline(v = match(as.character(with(optica_time.vec.ls2[[lg]], 
                           #                                    expr =
                           #                                      switch(match(lg, 
                           #                                                   table = names(out_stg_qa.arr.ls)
                           #                                                   ), 
                           #                                             52 * c(- rev(seq.int(start[trust] - 1) %/% 52), 
                           #                                                    seq.int((total - start[trust] + 1) %/% 52)
                           #                                                    ), 
                           #                                             12 * c(- rev(seq.int((start[trust] - 1) %/% 12)), 
                           #                                                    seq.int((total - start[trust] + 1) %/% 12)
                           #                                                    )
                           #                                             )
                           #                                    )
                           #                               ),
                           #                  table = names(out_stg_thin.vec)
                           #                  ),
                           #        col = 8, lty = 2, lwd = 2
                           #        )  # Overlay vertical grid lapsed year delimiters for readability
                           
                           # text(which(! is.na(out_stg_thin.vec))[1], 
                           #      head(out_stg_thin.vec[! is.na(out_stg_thin.vec)], 
                           #           n = 1
                           #           ), 
                           #      labels = trust, 
                           #      col = 1, pos = 2, cex = .75
                           #      )  # Overlay "trust_code" onto time-series plot
                           # 
                           # text(length(out_stg_thin.vec) - 
                           #        which(! is.na(rev(out_stg_thin.vec)))[1] + 1, 
                           #      tail(out_stg_thin.vec[! is.na(out_stg_thin.vec)], 
                           #           n = 1
                           #           ), 
                           #      labels = trust, 
                           #      col = 1, pos = 4, cex = .75
                           #      )  # Overlay "trust_code" onto time-series plot
                           
                           # legend("topleft", 
                           #        legend = c("Observed", "Smoothed"), 
                           #        bty = "n", cex = 1, lty = seq.int(2), pch = c(16, NA)#, x.intersp = 1, y.intersp = .5
                           #        )  # Overlay legend with "statistic" keys onto time-series plot
                           
                           dev.print(pdf, 
                                     file = file.path(output.dir, 
                                                      "Graphs/QA/Descriptive", 
                                                      switch(match(out, 
                                                                   table = dimnames(out_stg_qa.arr.ls[[lg]])$statistic
                                                                   ), 
                                                             "Undischarged out of beds", 
                                                             "Undischarged out of dischargeable"
                                                             ), 
                                                      switch(match(lg, table = names(out_stg_qa.arr.ls)), 
                                                             "Weekly", "Monthly"
                                                             ), 
                                                      ifelse(trust == "pooled", 
                                                             "Pooled trusts", 
                                                             "OPTICA trusts (lapsed time)"
                                                             ), 
                                                      paste(out, lg, trust, 
                                                            "stg_qa.pdf", 
                                                            sep = "_"
                                                            )
                                                      ), 
                                     width = 1024 / 72, height = 768 / 72
                                     )  # Export time-series plot in .pdf format
                           
                           # dev.print(png, 
                           #           file = file.path(output.dir, 
                           #                            "Graphs/QA/Descriptive", 
                           #                            switch(match(out, 
                           #                                         table = dimnames(out_stg_qa.arr.ls[[lg]])$statistic
                           #                                         ), 
                           #                                   "Undischarged out of beds", 
                           #                                   "Undischarged out of dischargeable"
                           #                                   ), 
                           #                            switch(match(lg, table = names(out_stg_qa.arr.ls)), 
                           #                                   "Weekly", "Monthly"
                           #                                   ), 
                           #                            ifelse(trust == "pooled", 
                           #                                   "Pooled trusts", 
                           #                                   "OPTICA trusts (lapsed time)"
                           #                                   ), 
                           #                            paste(out, lg, trust, 
                           #                                  "stg_qa.png", 
                           #                                  sep = "_"
                           #                                  )
                           #                            ),
                           #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                           #           # width = 14, height = 7, units = "in", res = 3e2
                           #           # width = 1024, height = 768
                           #           )  # Export time-series plot in .png format
                           
                           # unlink(file.path(output.dir, 
                           #                  "Graphs/QA/Descriptive", 
                           #                  switch(match(out, 
                           #                               table = dimnames(out_stg_qa.arr.ls[[lg]])$statistic
                           #                               ), 
                           #                         "Undischarged out of beds", 
                           #                         "Undischarged out of dischargeable"
                           #                         ), 
                           #                  switch(match(lg, table = names(out_stg_qa.arr.ls)), 
                           #                         "Weekly", "Monthly"
                           #                         ), 
                           #                            ifelse(trust == "pooled", 
                           #                                   "Pooled trusts", 
                           #                                   "OPTICA trusts (lapsed time)"
                           #                                   ), 
                           #                  paste(out, lg, trust, 
                           #                        "stg_qa.*", 
                           #                        sep = "_"
                           #                        )
                           #                  )
                           #        )  # Remove time-series plot files
                           }
                         )
                )
       )


## Time-series plot of 
## delayed discharge outcomes 
## vs lapsed time pooled across 
## NHS trusts with 1yr pre- and 
## post-OPTICA deployment data

lapply(names(out_1yrprepost_stg_qa.arr.ls), 
       FUN = function(lg) 
         lapply(dimnames(out_1yrprepost_stg_qa.arr.ls[[lg]])$statistic, 
                FUN = function(out) 
                  {
                  thin_pooled_stg.ls <- list(lower = which(! is.na(out_1yrprepost_stg_qa.arr.ls[[lg]][, "pooled", out]))[1], 
                                             upper = nrow(out_1yrprepost_stg_qa.arr.ls[[lg]]) - 
                                               which(rev(! is.na(out_1yrprepost_stg_qa.arr.ls[[lg]][, "pooled", out])))[1]
                                             )  # Derive list of redundant NA cut-offs for QAed staggered delayed discharge outcomes vector
                  
                  out_thin_pooled_stg.vec <- out_1yrprepost_stg_qa.arr.ls[[lg]][with(thin_pooled_stg.ls, 
                                                                                     expr = seq.int(from = lower, to = upper)
                                                                                     ), 
                                                                                "pooled", out]  # Derive vector of QAed staggered thinned delayed discharge outcomes
                  
                  
                  out_thin_pooled_stg.dat <- setNames(data.frame(names(out_thin_pooled_stg.vec), out_thin_pooled_stg.vec), 
                                                      nm = c(paste("start", lg, sep = "_"), "prop")
                                                      )  # Format staggered thinned delayed discharge outcomes vector as data-frame
                  
                  
                  plot(out_thin_pooled_stg.vec, 
                       type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
                       col = 1, col.sub = 4, lty = 1, pch = 16, 
                       main = paste("Proportions of", 
                                    switch(match(out, 
                                                 table = dimnames(out_1yrprepost_stg_qa.arr.ls[[lg]])$statistic
                                                 ), 
                                           "Beds Occupied by Patients Fit for Discharge\n", 
                                           "Undischarged Patients Fit for Discharge\n"
                                           ), 
                                    "from Pooled Acute NHS Trusts Deploying OPTICA in England", 
                                    sep = " "
                                    ), 
                       sub = "OPTICA-deploying trusts with 1yr data before and after deployment", 
                       xlab = paste(switch(match(lg, 
                                                 table = names(out_1yrprepost_stg_qa.arr.ls)
                                                 ), 
                                           "Weeks", "Months"
                                           ), 
                                    "lapsed from OPTICA deployment", 
                                    sep = " "
                                    ), 
                       ylab = "Proportion of undischarged patients (%)", 
                       ylim = range(pretty(out_thin_pooled_stg.vec, n = 8))
                       )  # Time-series plots of key QAed staggered delayed discharge outcomes
                  
                  # lines(predict(loess(prop ~ I(seq_along(get(paste("start", lg, sep = "_")))), 
                  #                          data = out_thin_pooled_stg.dat, 
                  #                          na.action = na.exclude
                  #                     )
                  #               ), 
                  #       col = 1, lty = 2, lwd = 1
                  #       )  # Overlay LOESS point predictions over time-series plot
                  
                  axis(1, 
                       at = seq_along(out_thin_pooled_stg.vec), 
                       labels = c(ifelse(as.integer(names(out_thin_pooled_stg.vec)[as.integer(names(out_thin_pooled_stg.vec)) < 0]) %% 2 == 1, 
                                         names(out_thin_pooled_stg.vec)[as.integer(names(out_thin_pooled_stg.vec)) < 0], NA), 
                                  ifelse(as.integer(names(out_thin_pooled_stg.vec)[as.integer(names(out_thin_pooled_stg.vec)) > 0]) %% 2 == 1, 
                                         names(out_thin_pooled_stg.vec)[as.integer(names(out_thin_pooled_stg.vec)) > 0], NA)
                                  ), 
                       cex.axis = .8, las = 2
                       )  # Overlay X-axis labels onto time-series plot
                  
                  axis(2, 
                       at = pretty(out_thin_pooled_stg.vec, n = 8), 
                       labels = pretty(out_thin_pooled_stg.vec, n = 8), 
                       cex.axis = .8, las = 2
                       )  # Overlay Y-axis labels onto time-series plot
                  
                  text(seq_along(out_thin_pooled_stg.vec), 
                       out_thin_pooled_stg.vec, 
                       labels = rowSums(! is.na(out_1yrprepost_stg_qa.arr.ls[[lg]][, setdiff(dimnames(out_1yrprepost_stg_qa.arr.ls[[lg]])$trust_code, 
                                                                                                 y = "pooled"
                                                                                                 )
                                                                                       , out]
                                                )
                                        )[names(out_thin_pooled_stg.vec)], 
                       cex = .8, col = 4, pos = 3#, adj = c(.5, -.1)
                       )  # Overlay OPTICA-deploying trust sizes onto time-series plot
                  
                  abline(v = mean(match(as.character(c(-1, 1)), 
                                        table = names(out_thin_pooled_stg.vec)
                                        )
                                  ), 
                         col = 8, lty = 4, lwd = 2
                         )  # Overlay intervention start vertical delimiter
                  
                  text(mean(match(as.character(c(-1, 1)), 
                                  table = names(out_thin_pooled_stg.vec)
                                  )
                            ), 
                       max(pretty(out_thin_pooled_stg.vec, n = 8)), 
                       labels = "OPTICA\ndeployed", 
                       cex = 1, col = 8, pos = 4#, adj = c(.5, -.1)
                       )  # Overlay OPTICA start date onto time-series plot
                  
                  abline(v = c(if(as.integer(names(out_thin_pooled_stg.vec))[1] < 0) 
                    rev(seq.int(which(as.integer(names(out_thin_pooled_stg.vec)) == "-1"), 
                                1, 
                                by = - switch(match(lg, 
                                                    table = names(out_1yrprepost_stg_qa.arr.ls)
                                                    ), 52, 12
                                              )
                                )[-1] + 1
                        ), 
                    seq.int(which(as.integer(names(out_thin_pooled_stg.vec)) == "1"), 
                            length(names(out_thin_pooled_stg.vec)), 
                            by = switch(match(lg, 
                                              table = names(out_1yrprepost_stg_qa.arr.ls)
                                              ), 52, 12
                                        )
                            )[-1] - 1
                    ), 
                    col = 8, lty = 2, lwd = 2
                    )  # Overlay vertical grid lapsed year delimiters for readability
                  
                  dev.print(pdf, 
                            file = file.path(output.dir, 
                                             "Graphs/QA/Descriptive", 
                                             switch(match(out, 
                                                          table = dimnames(out_1yrprepost_stg_qa.arr.ls[[lg]])$statistic
                                                          ), 
                                                    "Undischarged out of beds", 
                                                    "Undischarged out of dischargeable"
                                                    ), 
                                             switch(match(lg, table = names(out_1yrprepost_stg_qa.arr.ls)), 
                                                    "Weekly", "Monthly"
                                                    ), 
                                             "Pooled trusts", 
                                             paste(out, lg, 
                                                   "pooled_1yrprepost_stg_qa.pdf", 
                                                   sep = "_"
                                                   )
                                             ), 
                            
                            )  # Export time-series plot in .pdf format
                  
                  # dev.print(png, 
                  #           file = file.path(output.dir, 
                  #                            "Graphs/QA/Descriptive", 
                  #                            switch(match(out, 
                  #                                         table = dimnames(out_1yrprepost_stg_qa.arr.ls[[lg]])$statistic
                  #                                         ), 
                  #                                   "Undischarged out of beds", 
                  #                                   "Undischarged out of dischargeable"
                  #                                   ), 
                  #                            switch(match(lg, table = names(out_1yrprepost_stg_qa.arr.ls)), 
                  #                                   "Weekly", "Monthly"
                  #                                   ), 
                  #                            "Pooled trusts", 
                  #                            paste(out, lg, 
                  #                                  "pooled_1yrprepost_stg_qa.png", 
                  #                                  sep = "_"
                  #                                  )
                  #                            ), 
                  #           width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                  #           # width = 14, height = 7, units = "in", res = 3e2
                  #           # width = 1024, height = 768
                  #           )  # Export time-series plot in .png format
                  
                  # unlink(file.path(output.dir, 
                  #                  "Graphs/QA/Descriptive", 
                  #                  switch(match(out, 
                  #                               table = dimnames(out_1yrprepost_stg_qa.arr.ls[[lg]])$statistic
                  #                               ), 
                  #                         "Undischarged out of beds", 
                  #                         "Undischarged out of dischargeable"
                  #                         ), 
                  #                  switch(match(lg, table = names(out_1yrprepost_stg_qa.arr.ls)), 
                  #                         "Weekly", "Monthly"
                  #                         ), 
                  #                  "Pooled trusts", 
                  #                  paste(out, lg, 
                  #                        "pooled_1yrprepost_stg_qa.*", 
                  #                        sep = "_"
                  #                        )
                  #                  )
                  #        )  # Remove time-series plot files
                  }
                )
       )

# par(old_par)  # Restore default graphical parameters


################################
## Time-series plots of       ##
## delayed discharge outcomes ##
## for tHF analysis report    ##
################################

## Plot of QAed staggered introduction 
## of OPTICA across NHS trusts

barplot(rollout_plot.vec, 
        names.arg = "", axes = FALSE, 
        main = "", 
        xlab = "Month", ylab = "No. trusts", 
        xlim = range(barplot(rollout_plot.vec, plot = FALSE)), 
        ylim = range(pretty(rollout_plot.vec, n = 8))
        )  # Time-series barplot of cumulative OPTICA roll-out frequencies

axis(1, 
     at = barplot(rollout_plot.vec, plot = FALSE), 
     labels = ifelse(seq_along(rollout_plot.vec) %% 2 == 1, 
                     names(rollout_plot.vec), NA
                     ), 
     cex.axis = .8, las = 2
     )  # Overlay X-axis labels onto time-series barplot

axis(2, 
     at = pretty(rollout_plot.vec, n = 8), 
     cex.axis = .8, las = 2
     )  # Overlay Y-axis labels onto time-series barplot

abline(h = pretty(rollout_plot.vec, n = 8), col = 8, lty = 3)  # Overlay horizontal grid lines for readability

# dev.print(pdf, 
#           file = file.path(output.dir, 
#                            "Graphs/Report/rollout_thf.pdf"
#                            ), 
#           width = 1024 / 72, height = 768 / 72
#           )  # Export time-series plot in .pdf format

dev.print(png,
          file = file.path(output.dir,
                           "Graphs/Report/rollout_thf.png"
                           ),
          width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
          # width = 14, height = 7, units = "in", res = 3e2
          # width = 1024, height = 768
          )  # Export time-series plot in .png format

# unlink(file.path(output.dir, 
#                  file.path(output.dir, 
#                            "Graphs/Report/rollout_thf.*"
#                            )
#                  )
#        )  # Remove time-series plot files


## Time-series plot of 
## occupied beds undischarged 
## proportions vs calendar time 
## for all NHS trusts

matplot(out_qa.arr.ls$mo[, 
                         c(optica_status.dat$trust_code, "pooled"), 
                         "With OPTICA", "prop_bed"
                         ], 
        type = "l", axes = FALSE, lty = 1, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
        col = c(rep.int(1, times = length(optica_status.dat$trust_code)), 4), 
        lwd = c(rep.int(1, times = length(optica_status.dat$trust_code)), 6), 
        main = "", 
        xlab = "Month", 
        ylab = "Proportion of undischarged patients (%)", 
        ylim = range(pretty(out_qa.arr.ls$mo[, , , "prop_bed"], 
                            n = 8
                            )
                     )
        )  # Time-series plots of 'With OPTICA' "comparator" delayed discharges outcomes

matlines(out_qa.arr.ls$mo[, setdiff(dimnames(out_qa.arr.ls$mo)$trust_code, 
                                                         y = optica_status.dat$trust_code
                                                         ), "Without OPTICA", "prop_bed"], 
         col = c(rep.int(8, 
                         times = length(setdiff(dimnames(out_qa.arr.ls$mo)$trust_code, 
                                                y = c(optica_status.dat$trust_code, "pooled")
                                                )
                                        )
                         ), 
                 2
                 ), 
         lty = 2, 
         lwd = c(rep.int(1, 
                         times = length(setdiff(dimnames(out_qa.arr.ls$mo)$trust_code, 
                                                y = c(optica_status.dat$trust_code, "pooled")
                                                )
                                        )
                         ), 
                 6
                 )
         )  # Overlay 'Without OPTICA' "comparator" delayed discharges outcomes over time-series plot

matpoints(match(optica_status.dat$start_moyr, 
                table = dimnames(out_qa.arr.ls$mo)$date_moyr
                ), 
          apply(optica_status.dat[c("start_moyr", "trust_code")], 
                MARGIN = 1, 
                FUN = function(idx.vec) 
                  out_qa.arr.ls$mo[idx.vec[1], idx.vec[2], "With OPTICA", "prop_bed"]
                ), 
          cex = 1.5, pch = 4
          )  # Overlay OPTICA-roll-out start "date_moyr" over time-series plot

with(dimnames(out_qa.arr.ls$mo), 
     expr = axis(1, 
                 at = seq_along(date_moyr), 
                 labels = ifelse(seq_along(date_moyr) %% 2 == 1, 
                                 date_moyr, NA
                                 ), 
                 cex.axis = .8, las = 2
                 )
     )  # Overlay X-axis labels onto time-series plot

axis(2, 
     at = pretty(out_qa.arr.ls$mo[, , , "prop_bed"], 
                 n = 8
                 ), 
     labels = pretty(out_qa.arr.ls$mo[, , , "prop_bed"], 
                     n = 8
                     ), 
     cex.axis = .8, las = 2
     )  # Overlay Y-axis labels onto time-series plot

with(dimnames(out_qa.arr.ls$mo), 
     expr = abline(v = seq_along(date_moyr)[grepl("^Dec", 
                                                  x = date_moyr
                                                  )] + .5, 
                   col = 8, lty = 3
                   )
     )  # Overlay vertical grid year delimiters for readability

legend("topleft", 
       legend = dimnames(out_qa.arr.ls$mo)$comparator, 
       bty = "n", cex = 1, col = c(1, 8), lty = seq.int(2), x.intersp = 1, y.intersp = .5#, inset = -.05
       )  # Overlay legend with "comparator" keys onto time-series plot

legend("top", 
       legend = paste(dimnames(out_qa.arr.ls$mo)$comparator, 
                      "(pooled)", 
                      sep = " "
                      ), 
       bty = "n", cex = 1, fill = c(4, 2), x.intersp = 1, y.intersp = .75#, inset = -.05
       )  # Overlay legend with "comparator" keys onto time-series plot

# dev.print(pdf, 
#           file = file.path(output.dir, 
#                            "Graphs/Report/prop_bed_mo_qa_thf.pdf"
#                            ), 
#           width = 1024 / 72, height = 768 / 72
#           )  # Export time-series plot in .pdf format

dev.print(png,
          file = file.path(output.dir,
                           "Graphs/Report/prop_bed_mo_qa_thf.png"
                           ),
          width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
          # width = 14, height = 7, units = "in", res = 3e2
          # width = 1024, height = 768
          )  # Export time-series plot in .png format

# unlink(file.path(output.dir, 
#                            "Graphs/Report/prop_bed_mo_qa_thf.*"
#        )  # Remove time-series plot files


## Time-series plots of 
## QAed delayed discharge outcomes 
## vs calendar time 
## across NHS trusts

lapply(c("R0B", "RVW"), 
       FUN = function(trust)
         {
         start_trust <- match(subset(optica_status.dat, 
                                     subset = trust_code == trust, 
                                     select = start_moyr, 
                                     drop = TRUE
                                     ), 
                              table = dimnames(out_qa_plot.arr.ls2$mo$nopooled)$date_moyr
                              )  # Derive OPTICA deployment start point for "trust_code" 'trust'
         
         
         plot(out_qa_plot.arr.ls2$mo$nopooled[, trust, "prop_bed"], 
              type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
              col = 1, lty = 1, 
              pch = rep.int(c(1, 16), 
                            times = c(start_trust - 1, 
                                      length(dimnames(out_qa_plot.arr.ls2$mo$nopooled)$date_moyr) - 
                                        start_trust + 1
                                      )
                            ), 
              main = "", 
              # sub = paste(trust, "Trust", sep = " "), 
              xlab = "Month", 
              ylab = "Proportion of undischarged patients (%)", 
              ylim = range(pretty(c(out_qa_plot.arr.ls2$mo$nopooled[, trust, "prop_bed"], 
                                    out_qa_plot.arr.ls2$mo$pooled[, paste0(c(25, 75), "%"), 
                                                                  "Without OPTICA", "prop_bed"
                                                                  ]
                                    ), 
                                  n = 8
                                  )
                           )
              )  # Time-series plots of QAed delayed discharge outcomes
         
         # lines(predict(loess(prop ~ I(seq_along(date_moyr)), 
         #                     data = out_qa_plot.dat.ls2$mo$nopooled, 
         #                     subset = trust_code == trust, 
         #                     na.action = na.exclude
         #                     )
         #               ), 
         #       col = 1, lty = 2, lwd = 1
         #       )  # Overlay LOESS point predictions over time-series plot
         
         lines(out_qa_plot.arr.ls2$mo$pooled[, "50%", "Without OPTICA", "prop_bed"], 
               col = 2, lty = 4, lwd = 1, pch = 1
               )  # Overlay 'Without OPTICA' "comparator" median outcomes over time-series plot
         
         polygon(with(dimnames(out_qa_plot.arr.ls2$mo$pooled), 
                      expr = c(seq_along(date_moyr), 
                               rev(seq_along(date_moyr))
                               )
                      ), 
                 c(out_qa_plot.arr.ls2$mo$pooled[, "75%", "Without OPTICA", "prop_bed"], 
                   rev(out_qa_plot.arr.ls2$mo$pooled[, "25%", "Without OPTICA", "prop_bed"])
                   ), 
                 density = 90, angle = NULL, col = rgb(1, 0, 0, alpha = .2), border = NA
                 )  # Overlay 'Without OPTICA' "comparator" IQR outcomes over time-series plot
         
         with(dimnames(out_qa_plot.arr.ls2$mo$nopooled), 
              expr = axis(1, 
                          at = seq_along(date_moyr), 
                          labels = ifelse(seq_along(date_moyr) %% 2 == 1, 
                                          date_moyr, 
                                          NA
                                          ), 
                          cex.axis = .8, las = 2
                          )
              )  # Overlay X-axis labels onto time-series plot
         
         axis(2, 
              at = pretty(c(out_qa_plot.arr.ls2$mo$nopooled[, trust, "prop_bed"], 
                            out_qa_plot.arr.ls2$mo$pooled[, paste0(c(25, 75), "%"), 
                                                          "Without OPTICA", "prop_bed"]
                            ), 
                          n = 8
                          ), 
              labels = pretty(c(out_qa_plot.arr.ls2$mo$nopooled[, trust, "prop_bed"], 
                                out_qa_plot.arr.ls2$mo$pooled[, paste0(c(25, 75), "%"), 
                                                              "Without OPTICA", "prop_bed"]
                                ), 
                              n = 8
                              ), 
              cex.axis = .8, las = 2
              )  # Overlay Y-axis labels onto time-series plot
         
         with(dimnames(out_qa_plot.arr.ls2$mo$nopooled), 
              expr = abline(v = seq_along(date_moyr
                                          )[grepl("^Dec", x = date_moyr)] + .5, 
                            col = 8, lty = 3
                            )
              )  # Overlay vertical grid year delimiters for readability
         
         # legend("topleft", 
         #        legend = c("Observed", "Smoothed"), 
         #        bty = "n", cex = 1, lty = seq.int(2), pch = c(16, NA)#, x.intersp = 1, y.intersp = .5
         #        )  # Overlay legend with "statistic" keys onto time-series plot
         
         legend("topleft", 
                legend = c("Observed", "Non-OPTICA (IQR)"), 
                bty = "n", cex = 1, col = seq.int(2), lty = c(1, 4), x.intersp = 1, y.intersp = .5
                )  # Overlay legend with "statistic" keys onto time-series plot
         
         legend("top", 
                legend = dimnames(out_qa_plot.arr.ls2$mo$pooled)$comparison, 
                bty = "n", cex = 1, col = 1, pch = c(16, 1), x.intersp = 1, y.intersp = .5
                )  # Overlay legend with "statistic" keys onto time-series plot
         
         
         # dev.print(pdf, 
         #           file = file.path(output.dir, 
         #                            "Graphs/Report", 
         #                            paste("prop_bed_mo", trust, "opt.pdf", sep = "_"), 
         #                            width = 1024 / 72, height = 768 / 72
         #                            )
         #           )  # Export time-series plot in .pdf format
         
         dev.print(png,
                   file = file.path(output.dir,
                                    "Graphs/Report",
                                    paste("prop_bed_mo", trust, "opt.png", sep = "_")
                                    ),
                   width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
                   # width = 14, height = 7, units = "in", res = 3e2
                   # width = 1024, height = 768
                   )  # Export time-series plot in .png format
         
         # unlink(file.path(output.dir, 
         #                  "Graphs/Report", 
         #                  paste("prop_bed_mo", trust, "opt.*", sep = "_")
         #                  )
         #        )  # Remove time-series plot files
         }
       )


## Time-series plot of 
## delayed discharge outcomes 
## vs lapsed time 
## across NHS trusts

thin_stg.ls <- list(lower = which(! is.na(out_stg_qa.arr.ls$mo[, "pooled", "prop_bed"]))[1], 
                    upper = nrow(out_stg_qa.arr.ls$mo) - 
                      which(rev(! is.na(out_stg_qa.arr.ls$mo[, "pooled", "prop_bed"])))[1]
                    )  # Derive list of redundant NA cut-offs for QAed staggered delayed discharge outcomes vector

out_stg_thin.vec <- out_stg_qa.arr.ls$mo[with(thin_stg.ls, 
                                              expr = seq.int(from = lower, to = upper)
                                              ), 
                                         "pooled", "prop_bed"]  # Derive vector of QAed staggered thinned delayed discharge outcomes


out_stg_thin.dat <- setNames(data.frame(names(out_stg_thin.vec), out_stg_thin.vec), 
                             nm = c(paste("start", lg, sep = "_"), "prop")
                             )  # Format staggered thinned delayed discharge outcomes vector as data-frame


plot(out_stg_thin.vec, 
     type = "b", axes = FALSE, lwd = 2, cex = 1.25, cex.main = 1.25, cex.lab = 1, 
     col = 1, col.sub = 4, lty = 1, pch = 16, 
     main = "", 
     xlab = "Months lapsed from OPTICA deployment", 
     ylab = "Proportion of undischarged patients (%)", 
     ylim = range(pretty(out_stg_thin.vec, n = 8))
     )  # Time-series plots of key QAed staggered delayed discharge outcomes

# lines(predict(loess(prop ~ I(seq_along(start_moyr)), 
#                          data = out_stg_thin.dat, 
#                          na.action = na.exclude
#                     )
#               ), 
#       col = 1, lty = 2, lwd = 1
#       )  # Overlay LOESS point predictions over time-series plot

axis(1, 
     at = seq_along(out_stg_thin.vec), 
     labels = c(ifelse(as.integer(names(out_stg_thin.vec)[as.integer(names(out_stg_thin.vec)) < 0]) %% 2 == 1, 
                       names(out_stg_thin.vec)[as.integer(names(out_stg_thin.vec)) < 0], NA), 
                ifelse(as.integer(names(out_stg_thin.vec)[as.integer(names(out_stg_thin.vec)) > 0]) %% 2 == 1, 
                       names(out_stg_thin.vec)[as.integer(names(out_stg_thin.vec)) > 0], NA)
                ), 
     cex.axis = .8, las = 2
     )  # Overlay X-axis labels onto time-series plot

axis(2, 
     at = pretty(out_stg_thin.vec, n = 8), 
     labels = pretty(out_stg_thin.vec, n = 8), 
     cex.axis = .8, las = 2
     )  # Overlay Y-axis labels onto time-series plot

text(seq_along(out_stg_thin.vec), 
     out_stg_thin.vec, 
     labels = rowSums(! is.na(out_stg_qa.arr.ls$mo[, optica_status.dat$trust_code, out])
                      )[names(out_stg_thin.vec)], 
     cex = .8, col = 4, pos = 3#, adj = c(.5, -.1)
     )  # Overlay OPTICA-deploying trust sizes onto time-series plot

abline(v = mean(match(as.character(c(-1, 1)), 
                      table = names(out_stg_thin.vec)
                      )
                ), 
       col = 8, lty = 4, lwd = 2
       )  # Overlay intervention start vertical delimiter

abline(v = c(if(as.integer(names(out_stg_thin.vec))[1] < 0) 
  rev(seq.int(which(as.integer(names(out_stg_thin.vec)) == "-1"), 1, 
              by = - 12
              )[-1] + 1
      ), 
  seq.int(which(as.integer(names(out_stg_thin.vec)) == "1"), 
          length(names(out_stg_thin.vec)), 
          by = 12
          )[-1] - 1
  ), 
  col = 8, lty = 2, lwd = 2
  )  # Overlay vertical grid lapsed year delimiters for readability

# dev.print(pdf, 
#           file = file.path(output.dir, 
#                            "Graphs/Report/prop_bed_mo_pooled_stg_thf.pdf"
#                            ), 
#           width = 1024 / 72, height = 768 / 72
#           )  # Export time-series plot in .pdf format
                           
dev.print(png, 
          file = file.path(output.dir, 
                           "Graphs/Report/prop_bed_mo_pooled_stg_thf.png"
                           ), 
          width = res_scale * 1024 / 72, height = res_scale * 768 / 72, units = "px", res = res_scale
          # width = 14, height = 7, units = "in", res = 3e2
          # width = 1024, height = 768
          )  # Export time-series plot in .png format

# unlink(file.path(output.dir, 
#                  "Graphs/Report/prop_bed_mo_pooled_stg_thf.*"
#                  )

#        )  # Remove time-series plot files
