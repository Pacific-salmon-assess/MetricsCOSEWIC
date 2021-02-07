
require(tidyverse)
require(rstanarm)
require(rstan)
require(shinystan)
require(coda)

library(MetricsCOSEWIC)


# SR_Sample is a built in data set
# use '?SR_Sample' for more information
names(SR_Sample)
unique(SR_Sample$Stock)

# Settings
stk <- "Stock9"
gen <- 4
yrs.window <- (3 * gen) +1
calc.yr <- 2017


test.df <- SR_Sample %>%
            dplyr::filter(Stock == stk) %>%
            select(Year,Spn)
test.df

test.df.sub <- test.df %>% dplyr::filter(Year > calc.yr - yrs.window ) %>% mutate(logSpn = log(Spn))
test.df.sub


plotPattern(yrs = test.df$Year ,vals = log(test.df$Spn),
            width=1,color="darkblue",
            yrs.axis=TRUE,vals.axis=TRUE,
            vals.lim=c(0,14), hgrid=TRUE,vgrid=FALSE,
            pch.val=19,pch.bg=NULL)
abline(v=c(calc.yr,calc.yr-yrs.window+1),col="red")

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





#### RstanArm TESTING ----------------------------------------------------------------------------------------------------


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


