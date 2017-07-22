###=========================================================================#
### Processing script to support Reporting Activity
###=========================================================================#

###=========================================================================#
###== FUNCTIONS ============================================================#
###-- getDALY_per_region ....... aggregate DALYs per WHO region
###-- getDALY_per_subregion .... aggregate DALYs per WHO subregion
###----| sum_DALYrun ........... sum two 'DALYrun' objects
###-- summarize ................ summarize 'DALYrun_region' object
###----| get_contrib ........... derive contributions ("yld", "yll")
###-- DALY_map ................. produce global burden of disease map
###-- FERG_report .............. produce FERG summary report
###-- plot_missing ............. plot missing values
###-- summarize_imputation ..... summarize imputation results
###-- imputation_report ........ generate report of imputation results
###-- get_total_cases .......... obtain cases by WHO subregion & total
###-- mean_age ................. calculate mean age from age distribution


##-------------------------------------------------------------------------##
## AGGREGATE DALYs PER WHO REGION -----------------------------------------##

getDALY_per_region <-
function(agent, date_DALY, fb = FALSE) {
  ## placeholders for 'DALYrun' and 'incidence'
  DALYrun <- NULL
  incidence <- NULL

  DALYrun_region <-
    list(AFR =  new("DALYrun"), 
         AMR =  new("DALYrun"),
         EMR =  new("DALYrun"),
         EUR =  new("DALYrun"),
         SEAR = new("DALYrun"),
         WPR =  new("DALYrun"))

  incidence_region <-
    list(AFR =  numeric(), 
         AMR =  numeric(),
         EMR =  numeric(),
         EUR =  numeric(),
         SEAR = numeric(),
         WPR =  numeric())

  label <- ifelse(fb, "fb/FB_DALY_", "DALY_")

  region <- factor(crpop_2015$WHORegion)

  pb <- txtProgressBar(max = length(crpop_2015$ISO3), style = 3)
  for (i in seq_along(crpop_2015$ISO3)) {
    ## load 'DALYrun' and 'incidence' per country
    file <-
      paste0(label, agent, "_", crpop_2015$ISO3[i], "-", date_DALY, ".RData")
    load(file)

    ## which region?
    r <- as.numeric(region[i])

    ## set K and r
    if (length(DALYrun_region[[r]]@K) == 0)
      DALYrun_region[[r]]@K <- DALYrun@K
    if (length(DALYrun_region[[r]]@r) == 0)
      DALYrun_region[[r]]@r <- DALYrun@r

    ## sum 'DALYrun' samples per region
    if (length(DALYrun_region[[r]]@samples) == 0) {
      DALYrun_region[[r]]@samples <- DALYrun@samples 
    } else {
      DALYrun_region[[r]]@samples <-
        sum_DALYrun(DALYrun_region[[r]]@samples, DALYrun@samples)
    }

    ## sum 'incidence' per region
    if (length(incidence_region[[r]]) == 0) {
      incidence_region[[r]] <- incidence$cases
    } else {
      incidence_region[[r]] <- incidence_region[[r]] + incidence$cases
    }

    ## set progress bar
    setTxtProgressBar(pb, i)
  }

  cat("\n")

  ## return DALYs per region
  return(list(DALYrun_region, incidence_region))
}


##-------------------------------------------------------------------------##
## AGGREGATE DALYs PER WHO SUBREGION --------------------------------------##

getDALY_per_subregion <-
function(agent, date_DALY, fb = FALSE) {
  ## placeholders for 'DALYrun' and 'incidence'
  DALYrun <- NULL
  incidence <- NULL

  DALYrun_subregion <-
    list(AFR_D  = new("DALYrun"),
         AFR_E  = new("DALYrun"),
         AMR_A  = new("DALYrun"),
         AMR_B  = new("DALYrun"),
         AMR_D  = new("DALYrun"),
         EMR_B  = new("DALYrun"),
         EMR_D  = new("DALYrun"),
         EUR_A  = new("DALYrun"),
         EUR_B  = new("DALYrun"),
         EUR_C  = new("DALYrun"),
         SEAR_B = new("DALYrun"),
         SEAR_D = new("DALYrun"),
         WPR_A  = new("DALYrun"),
         WPR_B  = new("DALYrun"))

  incidence_subregion <-
    list(AFR_D  = numeric(),
         AFR_E  = numeric(),
         AMR_A  = numeric(),
         AMR_B  = numeric(),
         AMR_D  = numeric(),
         EMR_B  = numeric(),
         EMR_D  = numeric(),
         EUR_A  = numeric(),
         EUR_B  = numeric(),
         EUR_C  = numeric(),
         SEAR_B = numeric(),
         SEAR_D = numeric(),
         WPR_A  = numeric(),
         WPR_B  = numeric())

  label <- ifelse(fb, "fb/FB_DALY_", "DALY_")

  subregion <- factor(crpop_2015$SUB)

  pb <- txtProgressBar(max = length(crpop_2015$ISO3), style = 3)
  for (i in seq_along(crpop_2015$ISO3)) {
    ## load 'DALYrun' and 'incidence' per country
    file <-
      paste0(label, agent, "_", crpop_2015$ISO3[i], "-", date_DALY, ".RData")
    load(file)

    ## which region?
    r <- as.numeric(subregion[i])

    ## set K and r
    if (length(DALYrun_subregion[[r]]@K) == 0)
      DALYrun_subregion[[r]]@K <- DALYrun@K
    if (length(DALYrun_subregion[[r]]@r) == 0)
      DALYrun_subregion[[r]]@r <- DALYrun@r

    ## sum 'DALYrun' samples per subregion
    if (length(DALYrun_subregion[[r]]@samples) == 0) {
      DALYrun_subregion[[r]]@samples <- DALYrun@samples 
    } else {
      DALYrun_subregion[[r]]@samples <-
        sum_DALYrun(DALYrun_subregion[[r]]@samples, DALYrun@samples)
    }

    ## sum 'incidence' per region
    if (length(incidence_subregion[[r]]) == 0) {
      incidence_subregion[[r]] <- incidence$cases
    } else {
      incidence_subregion[[r]] <- incidence_subregion[[r]] + incidence$cases
    }

    ## set progress bar
    setTxtProgressBar(pb, i)
  }

  cat("\n")

  ## return DALYs per subregion
  return(list(DALYrun_subregion, incidence_subregion))
}


##-------------------------------------------------------------------------##
## SUM TWO 'DALYrun' objects ----------------------------------------------##

sum_DALYrun <-
function(x, y) {
  ## number of nodes
  n_nodes <- length(x)
  for (node in seq(n_nodes)) {

    ## number of data elements per node
    n_data <- length(x[[node]])
    for (data in seq(n_data))
      x[[node]][[data]] <- x[[node]][[data]] + y[[node]][[data]]
  }

  ## return x + y
  return(x)
}


##-------------------------------------------------------------------------##
## SUMMARIZE 'DALYrun_region' ---------------------------------------------##

get_contrib <-
function(x) {
  nodes <- sapply(x[[1]]@samples, function(x) length(x))
  contrib <- ifelse(nodes == 6, "yld", "yll")
  return(contrib)
}


summarize <-
function(x, what, rate = FALSE,
         age = c("all", "<5", "5+"), denom = 1e5, names = "all") {
  ## check 'age'
  age <- match.arg(age)

  ## derive contributions per node
  contrib <- get_contrib(x[[1]])

  ## check 'names'
  if (length(names) == 1 && names == "all") {
    names <- names(contrib)

  } else {
    for (i in seq_along(names))
      names[i] <- match.arg(names[i], names(contrib))
  }

  ## derive regions
  if (length(x[[1]]) == 6) {
    regions_2015 <- crpop_2015$WHORegion

  } else if (length(x[[1]]) == 14) {
    regions_2015 <- crpop_2015$SUB

  } else {
    stop("regions not recognized.")
  }

  ## population by region
  pop_by_region_2015 <-
    switch(age,
           "all" = tapply(apply(Popul_2015, 3, sum), regions_2015, sum),
           "<5"  = tapply(apply(Popul_2015, 3, function(x) sum(x[1:2, ])),
                          regions_2015, sum),
           "5+"  = tapply(apply(Popul_2015, 3, function(x) sum(x[3:19, ])),
                          regions_2015, sum))

  ## prepare objects
  out <- matrix(0, ncol = 5, nrow = length(unique(regions_2015)) + 1)
  global <- global_inc <- 0
  what_yld <- ifelse(what %in% c("daly", "daly_case"), "yld", what)
  what_yll <- ifelse(what %in% c("daly", "daly_case"), "yll", what)

  ## define age group columns
  cols <-
    switch(age,
           "all" = 1:22,
           "<5"  = c(1:2, 12:13),
           "5+"  = c(3:11, 14:22))

  ## aggregate results over regions
  for (r in seq_along(unique(regions_2015))) {

    if (what %in% c("incidence", "daly_case")) {
      region_inc <- rowSums(x[[2]][[r]][, cols])
      global_inc <- region_inc + global_inc
    }

    if (what != "incidence") {
      region <- 0
      for (j in seq_along(contrib)) {
        if (contrib[j] == "yld" &
            what %in% c("cases", "yld", "daly", "daly_case") &
            names(contrib[j]) %in% names) {
          region <- region +
                    rowSums(x[[1]][[r]]@samples[[j]][[what_yld]][, cols])
        } else if (contrib[j] == "yll" &
                   what %in% c("deaths", "yll", "daly", "daly_case") &
                   names(contrib[j]) %in% names) {
          region <- region +
                    rowSums(x[[1]][[r]]@samples[[j]][[what_yll]][, cols])
        }
      }
      global <- global + region

      if (what == "daly_case") {
        region_daly_case <- region / (region_inc / denom)
      }
    }

    if (what == "incidence") {
      region <- region_inc
    } else if (what == "daly_case") {
      region <- region_daly_case
    }

    sd_denom <- ifelse(rate, pop_by_region_2015[r], 1)

    out[r, ] <-
      c(mean(region, na.rm = TRUE),
        median(region, na.rm = TRUE),
        quantile(region, c(.025, .975), na.rm = TRUE),
        sd((region / denom) / sd_denom, na.rm = TRUE))
  }

  if (what == "incidence") {
    global <- global_inc
  } else if (what == "daly_case") {
    global <- global / (global_inc / denom)
  }

  sd_denom <- ifelse(rate, sum(pop_by_region_2015), 1)

  out[length(unique(regions_2015)) + 1, ] <-
    c(mean(global, na.rm = TRUE),
      median(global, na.rm = TRUE),
      quantile(global, c(.025, .975), na.rm = TRUE),
      sd((global / denom) / sd_denom, na.rm = TRUE))

  ## apply appropriate denominator
  if (rate) {
    out[, 1:4] <- out[, 1:4] / c(pop_by_region_2015, sum(pop_by_region_2015))
  } else {
    out[, 1:4] <- out[, 1:4] / denom
  }

  ## finalize object
  rownames(out) <- c(sort(unique(regions_2015)), "GLOBAL")
  colnames(out) <- c("mean", "median", "2.5%", "97.5%", "sd")
  return(out)
}


##-------------------------------------------------------------------------##
## GLOBAL MAP -------------------------------------------------------------##

DALY_map <-
function(x, agent,
         cat = "pretty", numCats = 7,
         palette = brewer.pal(numCats, "Reds"), 
         what = c("daly", "sd", "inc", "mrt"),
         save = TRUE, type = c("png", "tiff"),
         legend_title, legend_digits = 3) {
  ## check 'what'
  what <- match.arg(what)

  ## check 'type'
  type <- match.arg(type)

  ## derive regions
  if (length(x[[1]]) == 6) {
    region  <- "region"
    regions_2015 <- countryRegion_2015$WHORegion
    sPDF    <- sPDF_WHOreg
  } else if (length(x[[1]]) == 14) {
    region  <- "subregion"
    regions_2015 <- countryRegion_2015$WHOSubregion
    sPDF    <- sPDF_WHOsub
  } else {
    stop("regions not recognized.")
  }

  ## calculate data to plot
  data_to_plot <-
    switch(
      what,
      daly = c(head(summarize(x, "daly", rate = TRUE)[, 2], -1), NA),
      sd   = c(head(summarize(x, "daly", rate = TRUE)[, 5], -1), NA),
      inc  = c(head(summarize(x, "incidence", rate = TRUE)[, 2], -1), NA),
      mrt  = c(head(summarize(x, "deaths", rate = TRUE)[, 2], -1), NA))

  ## add data_to_plot to sPDF object
  sPDF@data$data_to_plot <- data_to_plot

  ## calculate breaks
  breaks <-
    round(
      rwmGetClassBreaks(data_to_plot,
                        numCats = numCats,
                        catMethod = cat),
      legend_digits)

  ## define labels
  labels <-
    c(paste(head(breaks, -1), tail(breaks, -1), sep = " - "), "N/A")

  ## start png device if image is to be saved
  if (save) {
    if (type == "png") {
      png(paste(agent, "_", region, "_", what, ".png", sep = ""),
          width = 8, height = 3.25, units = "in", res = 300)
    } else if (type == "tiff") {
      tiff(paste(agent, "_", region, "_", what, ".tiff", sep = ""),
           width = 8, height = 3.25, units = "in", res = 300)
    }
  }

  ## create map
  par(mar = c(0, 0, 0, 0))
  map <-
    mapPolys(
      sPDF,
      nameColumnToPlot = "data_to_plot",
      colourPalette = palette,
      catMethod = cat,
      borderCol = "grey",
      mapTitle = "",
      missingCountryCol = "white",
      oceanCol = "grey95",
      addLegend = FALSE)
  legend("bottomleft",
         legend = labels,
         title = legend_title,
         pch = 22, pt.cex = 2, col = "grey75",
         pt.bg = c(palette, "white"),
         cex = .7, bg = "white")

  ## close graphics device if image is to be saved
  if (save) {
    graphics.off()
  }
}


##-------------------------------------------------------------------------##
## PDF REPORT -------------------------------------------------------------##

FERG_report <-
function(DALY, agent, agent_full, dm_file, today,
         by_age = FALSE, denom = 1e5,
         digits_net = 0, digits_rate = 3, scale = 0.8,
         add = NULL, fb = FALSE, exp = NULL, DALY_total = NULL) {
  ## derive regions
  if (length(DALY[[1]]) == 6) {
    by <- "region"
  } else if (length(DALY[[1]]) == 14) {
    by <- "subregion"
  } else {
    stop("regions not recognized.")
  }

  ## knit and rename
  knit2pdf(system.file("report.Rnw", package = "FERG"))
  file.rename("report.pdf",
              paste0(agent, "_", by, "-report_",
                     ifelse(fb, "FB_", ""), today, ".pdf"))
  file.remove(paste0("report.", c("tex", "log", "aux", "toc", "out")))

  ## success message
  cat("Report generated for the global burden of ",
      agent_full, " by WHO ", by, ".\n", sep = "")
}


##-------------------------------------------------------------------------##
## PLOT MISSING DATA ------------------------------------------------------##

plot_missing <-
function(inc, WHOsub, agent, agent_full, save = FALSE) {
  x <- data.frame(inc = inc, WHOsub = WHOsub, stringsAsFactors = FALSE)
  x$ISO <- toupper(crpop_2015$ISO3)
  x$is_data <- !is.na(x$inc)
  is_data_region <- tapply(inc, WHOsub, function(x) !all(is.na(x)))

  ## remove Antarctica
  sPDF <- getMap()[-which(getMap()$ADMIN=="Antarctica"), ]

  match_name <- match(sPDF$ISO3, x$ISO)

  sPDF@data$is_data <- !is.na(x$inc)[match_name]
  sPDF@data$is_data <-
    sPDF@data$is_data -
    as.numeric(is.na(x$inc) * is_data_region[x$WHOsub])[match_name]

  if (save) {
    png(paste(agent, "missing.png", sep = "_"),
        width = 7, height = 3.25, units = "in", res = 300)
  }

  par(mar = c(0, 0, 2, 0) + .1)
  mapPolys(
    sPDF,
    nameColumnToPlot = "is_data",
    catMethod = "categorical",
    colourPalette = c("orange", "red", "limegreen"),
    addLegend = FALSE,
    mapTitle = paste(agent_full, " (n=", sum(x$is_data), ")", sep = ""),
    missingCountryCol = "grey95",
    borderCol = "grey50")
  legend("bottomleft",
         c("country data",
           "no country data",
           "no country & region data",
           "N/A"),
         fill = c("limegreen", "orange", "red", "grey95"),
         cex = .8)

  if (save) {
    graphics.off()
  }
}


##--------------------------------------------------------------------------#
## SUMMARIZE IMPUTATION RESULTS --------------------------------------------#

summarize_imputation <-
function(db, db_imputed) {
  ## extract 'inc', 'WHOsub' and 'y'
  inc <- db$Inc
  WHOsub <- db$WHOsub
  y <- db_imputed$y

  ## define which countries have missing data
  is_na <- is.na(inc)

  ## summarize imputations per WHO subregion
  reg_fit <- data.frame(WHOsub = sort(unique(WHOsub)))
  reg_fit$sd <- reg_fit$up <- reg_fit$lw <- reg_fit$med <- reg_fit$mean <- NA

  for (i in unique(WHOsub[is_na])) {
    reg <- which(sort(unique(WHOsub)) == i)
    reg_fit$mean[reg] <-
      mean(y[, WHOsub[is_na] == i])
    reg_fit$med[reg] <-
      median(y[, WHOsub[is_na] == i])
    reg_fit$lw[reg] <-
      quantile(y[, WHOsub[is_na] == i], .025)
    reg_fit$up[reg] <-
      quantile(y[, WHOsub[is_na] == i], .975)
    reg_fit$sd[reg] <-
      sd(y[, WHOsub[is_na] == i])
  }

  ## add observed means
  reg_fit$obs_mean <-
    tapply(inc, WHOsub, mean, na.rm = T)

  ## add number of observations per WHO subregion
  n_obs <- tapply(inc[!is_na], WHOsub[!is_na], length)
  n_obs_WHOsub <- rep(0, 14)
  n_obs_WHOsub[sort(unique(WHOsub)) %in% names(n_obs)] <- n_obs
  reg_fit$n_data <-
    paste(
      n_obs_WHOsub, "/",
      with(crpop_2015,
           tapply(Country, paste(WHORegion, WHOsubRegion), length)),
      sep = "")

  ## round values
  reg_fit[, 2:7] <- round(reg_fit[, 2:7], 3)

  ## rename columns
  colnames(reg_fit) <-
    c("WHOsub", "mean", "median", "2.5%", "97.5%", "sd",
      "observed mean", "available data")

  ## return data.frame
  return(list(reg_fit = reg_fit,
              within_region_var  = mean(1 / db_imputed$tau),
              global_geom_mean   = mean(exp(db_imputed$mu.c)),
              between_region_var = mean(1 / db_imputed$mu.tau)))
}


##--------------------------------------------------------------------------#
## GENERATE REPORT OF IMPUTATION RESULTS -----------------------------------#

imputation_report <-
function(db, db_imputed, db_merged, pop, agent, agent_full,
         tab1_options = NULL, tab2_options = NULL) {
  today <- format(Sys.time(), "%Y%m%d")

  imp_summary <- summarize_imputation(db, db_imputed)
  imp_summary$reg_fit <- imp_summary$reg_fit[, -6]
  cases <- get_total_cases(db_merged, pop)[, -6]
  cases[, 2:5] <- round(cases[, 2:5], 0)

  knit2pdf(system.file("imputation_report.Rnw", package = "FERG"))
  file.rename("imputation_report.pdf",
              paste0(agent, "_imputation-report_", today, ".pdf"))
  file.remove("imputation_report.tex", 
              "imputation_report.log",
              "imputation_report.aux")
  cat("Imputation report generated for ", agent_full, ".\n", sep = "")
}


##--------------------------------------------------------------------------#
## OBTAIN CASES BY WHO SUBREGION & TOTAL -----------------------------------#

get_total_cases <-
function(db_merged, pop) {
  ## simulate inc per country
  inc <-
    t(apply(db_merged, 1,
            function(x) sim(1e3, "Percentiles", x, "INC")))

  ## simulate cases per country
  cases <- inc * pop

  ## sum cases per WHO subregion
  cases_per_subregion <- rowsum(cases, crpop_2015$SUB)

  ## sum cases globally
  cases_total <- colSums(cases_per_subregion)

  ## create summary data.frame
  out <-
    data.frame(
      c(sort(unique(crpop_2015$SUB)),
        "GLOBAL"),
      c(rowMeans(cases_per_subregion),
        mean(cases_total)),
      c(apply(cases_per_subregion, 1, median),
        median(cases_total)),
      c(apply(cases_per_subregion, 1, function(x) quantile(x, 0.025)),
        quantile(cases_total, 0.025)),
      c(apply(cases_per_subregion, 1, function(x) quantile(x, 0.975)),
        quantile(cases_total, 0.975)),
      c(apply(cases_per_subregion, 1, sd),
        sd(cases_total)),
      c(tapply(apply(Popul_2015, 3, sum), crpop_2015$SUB, sum),
        sum(Popul_2015)))

  rownames(out) <-
    NULL
  colnames(out) <-
    c("WHOsub", "mean", "median", "2.5%", "97.5%", "sd", "pop")

  ## return summary data.frame
  return(out)
}


##--------------------------------------------------------------------------#
## CALCULATE MEAN AGE FROM AGE DISTRIBUTION --------------------------------#

mean_age <-
function() {
  age_dist <- t(read.table(file = "clipboard", sep = "\t", dec = ","))[, 1]
  mean_age <- sum(age_dist * FERG_means)
  who_le <- approx(std_LE_WHO[, 1], std_LE_WHO[, 2], mean_age)$y
  return(list(mean_age = mean_age, who_le = who_le))
}