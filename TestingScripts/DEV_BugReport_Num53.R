########################################################################
# BrCo Bug Report
#https://github.com/SOLV-Code/MetricsCOSEWIC/issues/53




require(tidyverse)
require(rstanarm)
require(rstan)
require(shinystan)
require(coda)

library(MetricsCOSEWIC)


test.data <- data.frame(cbind(rep(1,26), seq(1990,2015), rnorm(26,1000,100)))
colnames(test.data) <- c("DU", "Year", "Abd")
test.data$Abd[c(2,8,9,10,11)] <- NA

head(test.data)
unique(test.data$DU)

test.data.window <- data.frame(DU = unique(test.data$DU),Window = 15)

folder.path = getwd()

multi.out <- multiFit(data.df = test.data, window.df = test.data.window, plot.file = paste0(folder.path,"/TestSummaryPlots.pdf"))



# running the individual fitting function

last.yr <- max(test.data$Year)

comparePercChange(du.label = "DU 2",
                  du.df = test.data,
                  yrs.window = 15,
                  calc.yr = last.yr,
                  samples.out = FALSE,
                  plot.pattern = TRUE,
                  plot.posteriors = TRUE,
                  plot.boxes  = TRUE)






########

test.data.2 <- SR_Sample %>% dplyr::filter(Stock %in% c("Stock1","Stock23")) %>%
            select(Stock,Year,Spn) %>% rename(DU=Stock,Abd = Spn)

#test.data.2 <- SR_Sample %>% select(Stock,Year,Spn) %>% rename(DU=Stock,Abd = Spn)


test.data.2

multi.out <- multiFit(data.df = test.data.2,
                      window.df = data.frame(DU = unique(test.data.2$DU),Window = 13),
                      plot.file = paste0(folder.path,"/TestSummaryPlots.pdf"))

multi.out
