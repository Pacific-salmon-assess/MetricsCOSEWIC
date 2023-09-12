
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
  # DOUBLE CHECK WHAT RSTAN VERSION IS RUNNING
  # You may need to follow:
    # RStan and Rtool compatibility issues:
    # Requires restarting R for this case
    # https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows
    # install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
    # install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# library(StanHeaders)
# packageVersion("rstan") # 2.26.22 is the correct version according to the MetricsCOSEWIC-stan repo
library(rstan) # for the old stan version
  # currently running tools 4.2.2

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
# source(here::here("R/runStan.R")) # is this right?
# Replace with here::here syntax
# Testing
# # script.dir <- dirname(sys.frame(1)$ofile)
# rstudioapi::getActiveDocumentContext()$path

#### Read in Data ####

# Bare minimums - runStan will run with just these
data <- read.csv(here::here("COSEWIC_WorkedExamples/MarineFish/Sebastes", "Sebastes_SampleData.csv"))
du.df <- data
du.df <- du.df %>% mutate(logAbd=log(Abd))
stk <- "SebastesA" # The name of the DU in question - only used for labeling
du.df <- du.df %>% filter(DU==stk) # For filtering just one DU out of a larger set
# scenario.name <- "long-time-series" #"short-time-series"
scenario.name <- "bug-test"
# Required for computation
gen <- 3
yrs.window <- (3 * gen) + 1 # *TOR*: Do we want separation of generations and year windows?



# Optional inputs - currently under testing as to their necessity
du.df.long <- du.df # reset for following

# *TOR*: Can runStan handle multiple DU's at one time?
du.df.long <- data.long %>% filter(DU==stk)

# Arithmetic smooth time-series (as in 2017 COSEWIC report on Fr sockeye)
du.df.long$Abd <- smoothSeries(du.df.long$Abd, gen=gen, filter.sides=1,
                               log.transform = FALSE, out.exp = FALSE,
                               na.rm=FALSE)

# Shorten data set to years used to estimate trends
calc.year <- 2021
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
  # linear-exp errors regarding variable's not existing - FIXED

# Original command line: stanr
  # Now will include both stanr model and cmdstanr model outputs
  # See stan.out$cmdstan for cmdstanr model raw output

# Run STAN with standardized data, exp prior on var, and 2.5 sigma priors on
  # slope and yi (DEFAULT)
stan.out <- run.stan(du.label=stk,
                     du.df=du.df,
                     raw=TRUE,
                     # yrs.window=yrs.window,
                     standardize.data = TRUE,
                     scenario.name = scenario.name,
                     prior_sigma_type = "exp")
# Just double check the fit.obj is present in the stan.out list (S4 object)

# Original command for df creation from stanr
# out.df <- data.frame(Value=stan.out$samples$Perc_Change, PriorSigma=2.5,
#                      std.data=1, VarPrior="Exp")

# New: cmdstanr # Not currently in use
  # cmdstanr inputs should be included in stan.out currently
# stan.nu <- run.stan(du.label=stk, du.df=du.df, yrs.window=yrs.window,
#                         standardize.data = TRUE,
#                         scenario.name = scenario.name,
#                         prior_sigma_type = "exp")


#### Explore Stan and cmdstanr Outputs ####

# Tor: This is space any testing of the outputs.

