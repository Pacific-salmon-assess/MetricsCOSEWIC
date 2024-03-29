########################################################################
# Bug Report: Issues #61 and #62
# https://github.com/SOLV-Code/MetricsCOSEWIC/issues/62
# https://github.com/SOLV-Code/MetricsCOSEWIC/issues/61



require(tidyverse)
require(rstanarm)
require(rstan)
require(shinystan)
require(coda)

library(MetricsCOSEWIC)


test.data <- data.frame(cbind(rep(1,26), seq(1990,2015), rnorm(26,1000,100)))
colnames(test.data) <- c("DU", "Year", "Abd")
test.data$Abd[c(2,8,9,10,11)] <- NA
#test.data$Abd[c(12,18,19,20,21)] <- NA

head(test.data)
unique(test.data$DU)

test.data.window <- data.frame(DU = unique(test.data$DU),Window = 15)
last.yr <- max(test.data$Year)


folder.path = getwd()



#  testing the individual estimation function

test.df.sub <- test.df %>% dplyr::filter(Year %in% 2003:2015)
test.df.sub


est.jags <- calcPercChangeMCMC(vec.in = log(test.df.sub$Spn),
                               method = "jags",
                               model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                               perc.change.bm = c(-30,-50,-70),
                               out.type = "long",
                               mcmc.plots = FALSE,
                               convergence.check = FALSE# ??Conv check crashes on ts() ???
                              )

est.jags

est.jags$probdecl


# testing the "compare" function

comparePercChange(du.label = "DU 1",
                  du.df = test.data %>% select(Year, Abd),
                  yrs.window = 15,
                  calc.yr = last.yr,
                  samples.out = FALSE,
                  plot.pattern = TRUE,
                  plot.posteriors = TRUE,
                  plot.boxes  = TRUE)




####################################
# test multifit on a dummy data set

multi.out <- multiFit(data.df = test.data, window.df = test.data.window, plot.file = paste0(folder.path,"/TestSummaryPlots.pdf"))

multi.out


######################################
# test multifit on a real data set

data.in <- SR_Sample %>% select(Stock,Year,Spn) %>% rename(DU=Stock,Abd = Spn)
head(data.in)

#write.csv(data.in,"tmp.csv")

window.in <- data.frame(DU = unique(data.in$DU),Window = 13)

multi.out.sr <- multiFit(data.df = data.in, window.df = window.in, plot.file =  "testing_output/Test_SRData_PercChange_Plots.pdf")

multi.out.sr

