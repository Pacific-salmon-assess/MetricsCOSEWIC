
#### Library Deps. ####

# Package dependencies
library(dplyr)
library(tidyr)
library(devtools)
library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(gridExtra)
#library(shinystan)
library(bridgesampling)
library(forcats)

# rstan setup
library(rstan) # for the old stan version

# cmdstanr setup
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# cmdstanr::check_cmdstan_toolchain(fix = TRUE) # to fix issues where toolchain is not installed
  # notification: will install mingw32-make and g++ with Rtools42
# cmdstanr::install_cmdstan(cores = parallel::detectCores())
# cmdstanr::check_cmdstan_toolchain() # check that your cmdstanr toolchain is properly setup
library(cmdstanr)

library(bayesplot)
library(posterior)

# install_github("Pacific-salmon-assess/MetricsCOSEWIC", dependencies = TRUE,
#                build_vignettes = FALSE) # If not already installed
library(MetricsCOSEWIC)
source("R/runStan.R")


#### Read in Data ####

data <- read.csv(here::here("COSEWIC_WorkedExamples/MarineFish/Sebastes", "Sebastes_SampleData.csv"))
data.long <- data

stk <- "SebastesA"
gen <- 3
scenario.name <- "long-time-series" #"short-time-series"

# Unknown code:
# if (!file.exists(here::here(eval(stk))))  dir.create(here::here(eval(stk)))
#
# if (file.exists(here::here(eval(stk)))){
#   if (!file.exists(here::here(eval(stk), "long-time-series"))) {
#     dir.create(here::here(eval(stk), "long-times-eries"))
#   }
#   if (!file.exists(here::here(eval(stk), "short-time-series"))) {
#     dir.create(here::here(eval(stk), "short-times-eries"))
#   }
# }

# other
yrs.window <- (3 * gen) + 1
calc.year <- 2021
du.df.long <- data.long %>% filter(DU==stk)

# Arithmetic smooth time-series (as in 2017 COSEWIC report on Fr sockeye)
du.df.long$Abd <- smoothSeries(du.df.long$Abd, gen=gen, filter.sides=1,
                               log.transform = FALSE, out.exp = FALSE,
                               na.rm=FALSE)

du.df.long <- du.df.long %>% mutate(logAbd=log(Abd))

# Shorten data set to years used to estimate trends
if(scenario.name == "short-time-series"){
  du.df <- du.df.long %>% filter(Year > (calc.year - yrs.window) &
                                   Year <= calc.year)
}
if(scenario.name == "long-time-series"){
  du.df <- du.df.long
}

#### Run Stan ####

# ERRORS
  # *year.scale not found* - FIXED
  # linear-exp errors regarding variable's not existing

# Original: stanr
# Run STAN with standardized data, exp prior on var, and 2.5 sigma priors on
# slope and yi (DEFAULT)
stan.out.or <- run.stan(du.label=stk, du.df=du.df, yrs.window=yrs.window,
                     standardize.data = TRUE,
                     scenario.name = scenario.name,
                     prior_sigma_type = "exp")

out.df <- data.frame(Value=stan.out$samples$Perc_Change, PriorSigma=2.5,
                     std.data=1, VarPrior="Exp")

# New: cmdstanr
stan.out.nu <- run.stan(du.label=stk, du.df=du.df, yrs.window=yrs.window,
                        standardize.data = TRUE,
                        scenario.name = scenario.name,
                        prior_sigma_type = "exp")


#### Explore Stan and cmdstanr Outputs ####
