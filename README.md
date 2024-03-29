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

	
## Documentation

### Reports and Presentations

A progress report, presentation, and worked examples are available on [Google Drive](https://drive.google.com/drive/folders/1y1d0TF8v2kWWbHXkTaK0mb3BQjUc7xrZ?usp=sharing)



### Wiki Pages

* [Context and Purpose](https://github.com/SOLV-Code/MetricsCOSEWIC/wiki)
* [Package Structure](https://github.com/SOLV-Code/MetricsCOSEWIC/wiki/1-Package-Structure)
* [Probability of Decline: Estimation Methods](https://github.com/SOLV-Code/MetricsCOSEWIC/wiki/2-Probability-of-Decline:-Estimation-Methods)

	
	

## Install

First, install the [R Statistical Software](https://mirror.rcg.sfu.ca/mirror/CRAN/) and the [RStudio front-end](https://www.rstudio.com/products/rstudio/download/).

Then you can install this package directly from github, using the following code on the command line in RStudio.

```
install.packages("devtools") # Install the devtools package
library(devtools) # Load the devtools package.
install_github("Pacific-salmon-assess/MetricsCOSEWIC", dependencies = TRUE, build_vignettes = FALSE)
library(MetricsCOSEWIC)				
```

**Note: You also need to install [JAGS](http://mcmc-jags.sourceforge.net/).**

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

library(dplyr) 
library(tidyr) 

data.in <- SR_Sample %>% select(Stock,Year,Spn) %>% rename(DU=Stock,Abd = Spn)
head(data.in)

write.csv(data.in,"tmp.csv")

window.in <- data.frame(DU = unique(data.in$DU),Window = 13)

multi.out <- multiFit(data.df = data.in, window.df = window.in, plot.file =  "Test_PercChange_Plots.pdf")


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

# Added NA for testing: test.df$Spen[50] <- NA

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

## Worked Examples

We are building a library of worked examples, showing
the steps from `*.csv` data files to summary outputs.

Note that these worked examples use data sets that are external to the package.
Two sample data sets are included inside the package.
Use `?SR_Sample` or `?DU_SampleData` for the details.



### Marine Fish

* [Sebastes](https://github.com/SOLV-Code/MetricsCOSEWIC/tree/main/COSEWIC_WorkedExamples/MarineFish/Sebastes). 

### Birds

* [Hirundinidae](https://github.com/SOLV-Code/MetricsCOSEWIC/tree/main/COSEWIC_WorkedExamples/Birds/Hirundinidae)













