
require(tidyverse)
require(rstanarm)
require(rstan)
require(shinystan)
require(coda)

library(MetricsCOSEWIC)


# SR_Sample is a built in data set
# use '?SR_Sample' for more information
?SR_Sample
names(SR_Sample)
sort(unique(SR_Sample$Stock))
length(unique(SR_Sample$Stock))
head(SR_Sample)


# DU_SampleData is another built in data set
?DU_SampleData
names(DU_SampleData)
sort(unique(DU_SampleData$DU))
length(unique(DU_SampleData$DU))
head(DU_SampleData)



###################################################################################
#  WRAPPER FUNCTION: multiFit FUNCTION


# run it on the SR data file

data.in <- SR_Sample %>% select(Stock,Year,Spn) %>% rename(DU=Stock,Abd = Spn)
head(data.in)

#write.csv(data.in,"tmp.csv")

window.in <- data.frame(DU = unique(data.in$DU),Window = 13)

multi.out.sr <- multiFit(data.df = data.in, window.df = window.in, plot.file =  "testing_output/Test_SRData_PercChange_Plots.pdf")


write.csv(multi.out.sr$Output,"testing_output/Test_SRData_Outputs.csv",row.names = FALSE)
write.csv(multi.out.sr$Summary,"testing_output/Test_SRData_Summary.csv",row.names = FALSE)



# run it on the DU sample data file

data.in <- DU_SampleData
head(data.in)

#write.csv(data.in,"tmp.csv")

window.in <- DU_SampleAges %>% mutate(Window = 3 *Avg_Gen + 1) %>% select(-Avg_Gen)

multi.out.sr <- multiFit(data.df = data.in, window.df = window.in, plot.file =  "testing_output/Test_DU_Data_PercChange_Plots.pdf")

write.csv(multi.out.sr$Output,"testing_output/Test_DU_Data_Outputs.csv",row.names = FALSE)
write.csv(multi.out.sr$Summary,"testing_output/Test_DU_Data_Summary.csv",row.names = FALSE)


# checking Ck_DU_W -> has a 0 record -> fixing with hardwired change to 0.1

#ck_w <- data.in %>% dplyr::filter(DU == "Chinook_DU_W" )
#calcPercChangeSimple(ck_w %>% select(Abd) %>% unlist %>% tail(13) %>% log())
#plotPattern(ck_w$Year)
# ck_w %>% mutate(Abd = recode(Abd, "0" = 0.1))

#ck_ab <- data.in %>% dplyr::filter(DU == "Chinook_DU_AB" )
#calcPercChangeSimple(ck_ab %>% select(Abd) %>% unlist %>% tail(15) %>% log())

#sk_q <- data.in %>% dplyr::filter(DU == "Sockeye_DU_Q" )





#####################################################
# TESTING INDIVIDUAL FUNCTIONS



# Settings
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






###########################

plotPattern(yrs = test.df$Year ,vals = log(test.df$Spn),
            width=1,color="darkblue",
            yrs.axis=TRUE,vals.axis=TRUE,
            vals.lim=c(0,14), hgrid=TRUE,vgrid=FALSE,
            pch.val=19,pch.bg=NULL)
abline(v=c(calc.yr,calc.yr-yrs.do+1),col="red")

#plot(test.df$Year,log(test.df$Spn),
#     type="o",col="darkblue",bty="n",
#     xlab="Year",ylab="Spn",pch=19,
#     main=stk)
#abline(v=c(calc.yr,calc.yr-yrs.window+1),col="red")



# use the deterministic function (single series version)
calcPercChangeSimple(test.df.sub$Spn)
est.simple <- calcPercChangeSimple(log(test.df.sub$Spn))
est.simple



#################################################
# JAGS VERSION


# use the MCMC jags version
est.jags <- calcPercChangeMCMC(vec.in = log(test.df.sub$Spn),
                   method = "jags",
                   model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                   perc.change.bm = -25,
                   out.type = "long",
                   mcmc.plots = FALSE,
                   convergence.check = FALSE# ??Conv check crashes on ts() ???
                   )
est.jags$pchange
est.jags$probdecl
est.jags$summary
est.jags$slope.converged
est.jags$samples

plotPattern(yrs = test.df$Year ,vals = log(test.df$Spn),
            width=1,color="darkblue",
            yrs.axis=TRUE,vals.axis=TRUE,
            #vals.lim=c(10,14),
            hgrid=TRUE,vgrid=FALSE,
            pch.val=19,pch.bg=NULL)
#abline(v=c(calc.yr,calc.yr-yrs.window+1),col="red")

addFit(data.df = test.df.sub, coeff = list(intercept = est.jags$summary["intercept","50%"],
                                       slope = est.jags$summary["slope","50%"] )
)


plotPattern(yrs = test.df.sub$Year ,vals = log(test.df.sub$Spn),
            width=1,color="darkblue",
            yrs.axis=TRUE,vals.axis=TRUE,
            #vals.lim=c(10,14),
            hgrid=TRUE,vgrid=FALSE,
            pch.val=19,pch.bg=NULL)
#abline(v=c(calc.yr,calc.yr-yrs.window+1),col="red")

addFit(data.df = test.df.sub, coeff = list(intercept = est.jags$summary["intercept","50%"],
                                           slope = est.jags$summary["slope","50%"] )
)


#################################################
# RSTANARM VERSION

est.rstanarm <- calcPercChangeMCMC(vec.in = log(test.df.sub$Spn),
                               method = "rstanarm",
                               model.in = NULL, # hardwired regression model form, so no input
                               perc.change.bm = -25,
                               out.type = "long",
                               mcmc.plots = FALSE,
                               convergence.check = FALSE# NOT IMPLEMENTED YET
                              )


est.rstanarm$pchange
est.rstanarm$pchange.raw
est.rstanarm$probdecl
est.rstanarm$summary
est.rstanarm$slope.converged

head(est.rstanarm$samples)
dens.tmp <- density(est.rstanarm$samples$Perc_Change)
plot(dens.tmp)
dens.tmp$x
dens.tmp$y

plotPattern(yrs = test.df$Year ,vals = log(test.df$Spn),
            width=1,color="darkblue",
            yrs.axis=TRUE,vals.axis=TRUE,
            vals.lim=c(0,14), hgrid=TRUE,vgrid=FALSE,
            pch.val=19,pch.bg=NULL)
#abline(v=c(calc.yr,calc.yr-yrs.window+1),col="red")

addFit(data.df = test.df.sub, coeff = list(intercept = est.rstanarm$summary["intercept","50%"],
                                           slope = est.rstanarm$summary["slope","50%"] )
)




plotDistribution(
  x.lab = "Perc Change",
  samples = list(jags = est.jags$samples$Perc_Change ,rstanarm = est.rstanarm$samples$Perc_Change   ),
  ref.lines = list(MLE = est.simple$pchange,BM = -25),
  plot.range = NULL #NULL #c(-90,90)
)







#######################################################

## INDIVIDUAL FUNCTION EXAMPLE OF COMPARISON



# pre-setup
stk <- "Stock3"
gen <- 4
calc.yr <- 2017


# inputs
du.df <- SR_Sample %>%
  dplyr::filter(Stock == stk) %>%
  select(Year,Spn)

yrs.window <- (3 * gen) +1
samples.out <- TRUE
plot.pattern <- TRUE
plot.posteriors <- TRUE
plot.boxes  = TRUE

# FN CALCS
du.df.sub <- du.df %>% dplyr::filter(Year > calc.yr - yrs.window )
du.df.sub


est.simple <- calcPercChangeSimple(log(test.df.sub$Spn))

est.jags <- calcPercChangeMCMC(vec.in = log(test.df.sub$Spn),
                               method = "jags",
                               model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                               perc.change.bm = -25,
                               out.type = "long",
                               mcmc.plots = FALSE,
                               convergence.check = FALSE# ??Conv check crashes on ts() ???
                                )


est.rstanarm <- calcPercChangeMCMC(vec.in = log(test.df.sub$Spn),
                                   method = "rstanarm",
                                   model.in = NULL, # hardwired regression model form, so no input
                                   perc.change.bm = -25,
                                   out.type = "long",
                                   mcmc.plots = FALSE,
                                   convergence.check = FALSE# NOT IMPLEMENTED YET
)



percentile.values <- c(0.025,0.25,0.5,0.75,0.975)
percentile.labels <- c("p2.5","p25","Med","p75","p97.5","Rhat")
extract.labels <- c("2.5%","25%","50%","75%","97.5%","Rhat")

out.mat <- matrix(NA,ncol = 4, nrow=13,
                        dimnames = list(c("MLE",paste("Jags",percentile.labels,sep="_"),  paste("RStanArm",percentile.labels,sep="_")),
                                        c("pchange","probdecl","slope","intercept"))
                            )

out.mat["MLE",] <-round(c(est.simple$pchange,NA,est.simple$slope,est.simple$intercept),5)


out.mat[grepl("Jags",dimnames(out.mat)[[1]]),"slope"] <- round(est.jags$summary["slope",extract.labels],5)
out.mat[grepl("Jags",dimnames(out.mat)[[1]]),"intercept"] <- round(est.jags$summary["intercept",extract.labels],5)
out.mat[grepl("Jags",dimnames(out.mat)[[1]]),"pchange"] <- c(quantile(est.jags$samples$Perc_Change,probs = percentile.values),NA)
out.mat["Jags_Med","probdecl"] <- round(est.jags$probdecl,5)


out.mat[grepl("RStanArm",dimnames(out.mat)[[1]]),"slope"] <- unlist(round(est.rstanarm$summary["slope",extract.labels],5))
out.mat[grepl("RStanArm",dimnames(out.mat)[[1]]),"intercept"] <- unlist(round(est.rstanarm$summary["intercept",extract.labels],5))

out.mat[grepl("RStanArm",dimnames(out.mat)[[1]]),"pchange"] <- c(quantile(est.rstanarm$samples$Perc_Change,probs = percentile.values),NA)
out.mat["RStanArm_Med","probdecl"] <- round(est.rstanarm$probdecl,5)
out.mat
write.csv(out.mat,"sample_output.csv",row.names = TRUE)

percchange.df <- data.frame(
  MLE = c(NA,NA,est.simple$pchange,NA, NA),
  Jags = quantile(est.jags$samples$Perc_Change,probs = percentile.values),
  Stan =  quantile(est.rstanarm$samples$Perc_Change,probs = percentile.values))

percchange.df




plotDistribution(
  x.lab = "Perc Change",
  samples = list(jags = est.jags$samples$Perc_Change ,rstanarm = est.rstanarm$samples$Perc_Change   ),
  ref.lines = list(MLE = est.simple$pchange,BM = -25),
  plot.range = c(-90,90) #NULL #c(-90,90)
)


plotBoxes(box.df = percchange.df, y.lab  = "Perc Change", ref.lines = list(BM = -25), plot.range = NULL)


















est.jags$summary["intercept","50%"]


est.simple$pchange
est.jags$pchange

est.rstanarm$pchange
est.rstanarm$probdecl





est.jags$summary
est.rstanarm$summary






#### RstanArm PIECES TESTING ----------------------------------------------------------------------------------------------------


est.stan <- stan_lm(logSpn ~ Year, test.df.sub,
        prior = NULL,
        seed = 12345)

summary(est.stan)
names(est.stan)
est.stan$coefficients
est.stan$fitted.values
est.stan$data

names(est.stan$stanfit)
class(est.stan$stanfit)
est.stan$stanfit

mcmc.samples.test <- as.data.frame(est.stan$stanfit) #As.mcmc.list(est.stan$stanfit)
names(mcmc.samples.test)
head(mcmc.samples.test)
quantile(mcmc.samples.test$"(Intercept)")


as.data.frame(est.stan$stan_summary)



lines(est.stan$data$Year,est.stan$fitted.values,col="red", lwd=1, lty=1)


launch_shinystan(est.stan, ppd = FALSE)


