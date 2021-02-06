
require(tidyverse)
require(rstanarm)
require(shinystan)

library(MetricsCOSEWIC)


# SR_Sample is a built in data set
# use '?SR_Sample' for more information
names(SR_Sample)


# Settings
stk <- "Stock3"
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
            vals.lim=c(10,14), hgrid=TRUE,vgrid=FALSE,
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

# use the MCMC jags version
est.jags <- calcPercChangeMCMC(vec.in = log(test.df.sub$Spn),
                   model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                   perc.change.bm = -25,
                   out.type = "long",
                   mcmc.plots = FALSE,
                   convergence.check = FALSE # ??Conv check crashes on ts() ???
                   )
est.jags$pchange
est.jags$probdecl
est.jags$summary



plotPattern(yrs = test.df$Year ,vals = log(test.df$Spn),
            width=1,color="darkblue",
            yrs.axis=TRUE,vals.axis=TRUE,
            vals.lim=c(10,14), hgrid=TRUE,vgrid=FALSE,
            pch.val=19,pch.bg=NULL)
#abline(v=c(calc.yr,calc.yr-yrs.window+1),col="red")

addFit(data.df = test.df.sub, coeff = list(intercept = est.jags$summary["intercept","50%"],
                                       slope = est.jags$summary["slope","50%"] )
)



#### Rstan version


est.stan <- stan_lm(logSpn ~ Year, test.df.sub,
        prior = NULL,
        seed = 12345)

names(est.stan)
est.stan$coefficients
est.stan$fitted.values
est.stan$data

lines(est.stan$data$Year,est.stan$fitted.values,col="red", lwd=1, lty=1)


launch_shinystan(est.stan, ppd = FALSE)


