# MetricsCOSEWIC
R Package for calculating COSEWIC metrics. Initial focus is on alternative estimates of *Percent Change* and *Probability of Decline*.

**WARNING: This is a pre-release development version. Repo is public for testing package install in another repo. DO NOT USE YET**

* Gottfried Pestal (Developer, gpestal"AT"solv.ca)
* Dr. Carrie Holt (Project Lead, Carrie.Holt"AT"dfo-mpo.gc.ca)


## Background

* initial version is a spin-off from a metrics package developed for status assessments under Canada's Wild Salmon policy ([WSPMetrics](https://github.com/Pacific-salmon-assess/WSP-Metrics-Pkg)).
* modifications include:
   * modify function arguments, input options, and outputs to customize for COSEWIC requirements
   * redesign the estimation step for probability of decline (increase robustness, explore transition from JAGS to STAN)
   * streamline the calculations and outputs for multiple DUs
* This is a stand-alone package, and is expected to diverge from the [WSPMetrics](https://github.com/Pacific-salmon-assess/WSP-Metrics-Pkg) package as functionality is expanded with contributions from various COSEWIC expert processess.


### Install

To install this package directly from github, use

```
install.packages("devtools") # Install the devtools package
library(devtools) # Load the devtools package.
install_github("SOLV-Code/MetricsCOSEWIC", dependencies = TRUE, build_vignettes = FALSE)
library(MetricsCOSEWIC)				
```



## Main Steps

The main function is *multiFit()*, which takes a data set of DU abundances, calculates *Percent Change* and *Probability of Decline* using 3 alternative methods (see [wiki](https://github.com/SOLV-Code/MetricsCOSEWIC/wiki/1-Probability-of-Decline:-Estimation-Methods)), then produces summary files and diagnostic plots. **Note**:*This function call does a log-transform before estimating the slope, but then converts the estimate of perc change back to the original units.* 

The data needs to be organized into a data frame like this:

DU | Year | Abd
-- | -- | --
Stock1 | 1960 | 7850.564
Stock1 | 1961 | 29719.99
Stock1 | 1962 | 22306.82


Using the built in data set, the function call looks like this:


```
data.in <- SR_Sample %>% select(Stock,Year,Spn) %>% rename(DU=Stock,Abd = Spn)


window.in <- data.frame(DU = unique(data.in$DU),Window = 13)
# this assumes that all DUs have a 4yr avg generation, and calculates Perc Change over 3 gen +1

multi.out <- multiFit(data.df, window.df, plot.file =  "Test_PercChange_Plots.pdf")

multi.out$Summary
head(multi.out$Output)

write.csv(multi.out$Output,"Test_Outputs.csv",row.names = FALSE)
write.csv(multi.out$Summary,"Test_Summary.csv",row.names = FALSE)
```


*multiFit()* is a wrapper for *comparePercChange()*, which does the calculation for a single DU.
*comparePercChange* provides more end-user control of settings and has more detailed output.
Using the built in data set, the function call looks like this:

```
stk <- "Stock3"
gen <- 4
yrs.do <- (3 * gen) +1
calc.yr <- 2017

test.df <- SR_Sample %>%
            dplyr::filter(Stock == stk) %>%
            select(Year,Spn)
head(test.df)

test.df.sub <- test.df %>% dplyr::filter(Year > calc.yr - yrs.do )
test.df.sub

fit.out <- comparePercChange(du.label = stk,
                             du.df = test.df,
                             yrs.window = yrs.do ,
                             calc.yr = 2017,
                             samples.out = TRUE,
                             plot.pattern = TRUE,
                             plot.posteriors = TRUE,
                             plot.boxes  = TRUE)


names(fit.out)
fit.out$Summary

```