## ----knitr, cache = F, results = "markup", echo = T, warning = T---------
# start with a clean workspace
rm(list=ls())
gc()

# Set the global knitr options
knitr::opts_chunk$set(cache = F, results = "hide", echo = T ,include = T)

# what should the output file be called?
filename = "STEP 1: Zero-inflated data simulation"

## ----initiate, cache = F, results = "hide", echo = F---------------------
# choose correct working directory
r_on_server <- T
if (r_on_server == T) {
  basicpath <- "/net/ifs1/san_projekte/projekte/"
} else {
  basicpath <- "/mnt/ifs1_projekte/"
}
setwd(basicpath)

# set working directory
pathwd <-
  paste0(
    basicpath,
    "genstat/02_projekte/1703_ge_metab_a1_b3_sorbs/171124_ZeroInflatedDataSim"
  )
setwd(pathwd)

# get additional functions from Holger Kirstens newest RProfile file
newest_rprofile <-
  function(designation = paste0(basicpath, "genstat/07_programme/rtools/RProfile_hk/")) {
    files <- list.files(designation)
    RProfiles <- files[grep("Rprofile_hk_", files)]
    newest_RProfile <- tail(sort(RProfiles), n = 1)
    suppressPackageStartupMessages(source(paste0(designation, newest_RProfile)))
  }
newest_rprofile()

# start time measurement, define alternative package directory, define start codes for external start of the script
if (r_on_server == T) {
  initializeSkript(myfilename = filename, computer = "forostar") # enter which server you are on
} else
  initializeSkript(myfilename = filename, computer = "local")

# Packages
for (i in c(
  "knitr",
  "data.table",
  "broom",
  "ordinal",
  "MASS",
  "parallel"
)) {
  suppressPackageStartupMessages(library(i, character.only = TRUE))
}

## ----setup---------------------------------------------------------------
# code by HKirsten (holgerman)
# set parameters for simulation, e.g. effect size, n...
set.seed(0815)

# magnitude of simulated effect  
effekte = c(0.01, 0.02, 0.05, 0.1, 0.3)

# size of simulated data set
n = 5000

# magnitude of zero-inflation in %
proz0 = c(20, 50, 80)

# how often should each step be repeated?
realisierungen = 1000

# type of categories
categart = c("quantile", "bereiche")

# number of categories
categnum = c(2:10, 20, 30, 40, 50)

## ----create.scenarios----------------------------------------------------
# create dt with all scenario combinations
szenarien <- data.table(expand.grid(effekte = effekte,
                                    n = n,
                                    proz0 = proz0,
                                    categart = categart,
                                    categnum = categnum,
                                    realization = 1:realisierungen))
szenarien[,num := 1:.N]

## ----test.data-----------------------------------------------------------
# apply over each szenario
res <- mclapply(szenarien$num,  function(x) {
  
  x <- szenarien[num == x]
  
  # x <- szenarien[268, ] # debug
  effekt <- x$effekte
  n <- x$n
  proz0 <- x$proz0
  categart <- x$categart
  categnum <- x$categnum
  num <- x$num 
  real <-  x$realization
  
  # create data for scenario
  df <- data.table(x = rnorm(n))
  df[, y := x * effekt + rnorm(n) * (1 - effekt) + 2]
  
  # fit lm
  linmod <- df[, summary(lm(y ~ x))]
  
  # add spearman rank correlation to compare with Theis
  cor <- tidy(df[, cor.test(x = x, y = y, method = "pear")])
  
  # add summary statistics
  res <- data.table(num = num,
                    realization = real,
                    effekt = effekt,
                    n = n,
                    proz0 = proz0,
                    categart = categart,
                    categnum = categnum,
                    pearson.cor = cor$estimate,
                    pearson.cor.pval = cor$p.value,
                    linmod.r2 = linmod$r.squared,
                    linmod.pval = linmod$coefficients["x","Pr(>|t|)"],
                    linmod.beta = linmod$coefficients["x","Estimate"])
  
  # create zero-inflated y-derived data
  df[, y.0infl := y]
  df[y < quantile(y.0infl, probs = proz0/100), y.0infl := 0]
  min.found = df[y.0infl > 0, min(y.0infl)]
  df[y.0infl > 0, y.0infl := y.0infl - 0.99*min.found]
  
  # check
  # df[,.N,y.0infl==0]
  # hist(df$y.0infl, breaks = 50)
  
  # sort by y (category)
  setorder(df, y)
  
  # create categories to calculate polr/ordinal over
  if(categart == "quantile") {
    
    # categorisation over quantiles
    df[y.0infl > 0, y.0infl.categ := cut(y.0infl, breaks = quantile(y.0infl, probs = seq(0, 1, 1/categnum)), include.lowest = T)]
    categs.created <- df[, levels(y.0infl.categ)]
    df[y.0infl == 0, y.0infl.categ := factor(0, labels =  "[0]")]
    df[, y.0infl.categ := factor(y.0infl.categ, levels = c("[0]", categs.created))]
  } else if(categart == "bereiche"){
    
    # category separating over range
    df[y.0infl > 0, y.0infl.categ := cut(y.0infl, breaks = seq(0, max(y.0infl), length.out = categnum + 1), include.lowest = T)]
    categs.created <- df[, levels(y.0infl.categ)]
    df[y.0infl == 0, y.0infl.categ := factor(0, labels =  "[0]")]
    df[, y.0infl.categ := factor(y.0infl.categ, levels = c("[0]", categs.created))]
    
  } else {
    stop("Error")
  }
  
  # now we have the complete data and start fitting the model
  # start with fitting the regular glm for a dichotomized y
  linmod.dicho <- df[, summary(glm(y.0infl > 0 ~ x, family = "binomial"))]
  linmod.dicho.coefs <- setDT(as.data.frame(coefficients(linmod.dicho)), keep.rownames = T)
  linmod.dicho.pval <- unlist(linmod.dicho.coefs[rn == "x", "Pr(>|z|)"])
  linmod.dicho.beta <- unlist(linmod.dicho.coefs[rn == "x","Estimate"])
  
  # enter results
  res[, `:=`(dicho.pval = linmod.dicho.pval,
             dicho.beta = linmod.dicho.beta,
             dicho.AIC = linmod.dicho$aic)]
  
  # fit the ordinal proportional odds model
  logit.mod <- df[, summary(ordinal::clm(y.0infl.categ ~ x))]
  logit.mod.2 <- df[, ordinal::clm(y.0infl.categ ~ x)]
  logit.coefs <- setDT(as.data.frame(logit.mod$coefficients), keep.rownames = T)
  logit.pval <- unlist(logit.coefs[rn == "x", "Pr(>|z|)"])
  
  # enter results
  res[, `:=`(logit.beta = logit.mod.2$beta,
             logit.pval = logit.pval,
             logit.AIC = as.numeric(logit.mod.2$info$AIC))]
  
  return(res)
}, mc.cores = 10, mc.cleanup = T)

# bind everything together
all.scenarios <- rbindlist(res)


## ----save----------------------------------------------------------------
# Save for use in STEP2
write.table(x = all.scenarios,
            file = paste0(pathwd, "/results/",
                          format(Sys.time(), '%y%m%d'),
                          "_SimulationResults.csv"),
            sep = "\t", row.names = F)

## ----SessionInfo, echo=F, results='markup'-------------------------------
sessionInfo()

