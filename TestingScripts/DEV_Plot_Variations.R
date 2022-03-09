

require(tidyverse)
require(rstanarm)
require(rstan)
require(shinystan)
require(coda)

library(MetricsCOSEWIC)




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

pdf("TestPlot_ComparePercChange.pdf" ,onefile=TRUE,height=8.5, width = 11)

fit.out <- comparePercChange(du.label = stk,
                             du.df = test.df,
                             yrs.window = yrs.do ,
                             calc.yr = 2017,
                             samples.out = TRUE,
                             plot.fitted = TRUE,
                             plot.pattern = TRUE,
                             plot.posteriors = TRUE,
                             plot.boxes  = TRUE)
dev.off()

names(fit.out)
fit.out$Summary


############################
# testing multifit

test.data <- SR_Sample %>% dplyr::rename(DU=Stock,Abd=Spn) %>% select(DU,Year,Abd)
#test.data <- DU_SampleData
test.data.window <- data.frame(DU = unique(test.data$DU),Window = 15)


folder.path = getwd()

multi.out <- multiFit(data.df = test.data ,
                      window.df = test.data.window, plot.file = paste0(folder.path,"/TestSummaryPlots.pdf"))









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
                   perc.change.bm = c(-30,-50,-70,-25),
                   out.type = "long",
                   mcmc.plots = FALSE,
                   convergence.check = FALSE# ??Conv check crashes on ts() ???
                   )
est.jags$pchange
est.jags$probdecl
est.jags$summary
est.jags$slope.converged
est.jags$samples




plotDistribution(
  x.lab = "Perc Change",
  samples = list(Bayesian = est.jags$samples$Perc_Change ),
  det.est = est.simple$pchange,
  plot.range = c(-90,90) #NULL #c(-90,90)
)






# TRY CUStOM PRIORS AND MCMC SETTINGS

vec.use <- log(test.df.sub$Spn)
yrs.use <- 0:(length(vec.use)-1)

est.jags2 <- calcPercChangeMCMC(vec.in = vec.use,
                               method = "jags",
                               model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                               perc.change.bm = -25,
                               out.type = "long",
                               mcmc.plots = FALSE,
                               convergence.check = FALSE ,# ??Conv check crashes on ts() ???
                               priors = list( p_intercept = median(vec.use,na.rm=TRUE),
                                                 tau_intercept = (1/ max(vec.use,na.rm=TRUE))^2 ,
                                                 p_slope = 0,
                                                 tau_slope =  (1 / ( max(vec.use,na.rm=TRUE)/ max(yrs.use) ))^2
                                 ),
                                 mcmc.settings = list(n.chains = 3, n.iter = 120000, n.burnin = 20000, n.thin = 10)
                          )


est.jags2$pchange
est.jags2$probdecl
est.jags2$summary








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


# zoomed version

plotPattern(yrs = test.df.sub$Year ,vals = log(test.df.sub$Spn),
            width=1,color="darkblue",
            yrs.axis=TRUE,vals.axis=TRUE,
            #vals.lim=c(10,14),
            hgrid=TRUE,vgrid=FALSE, vlines = c(2010,2016),
            pch.val=19,pch.bg=NULL,pch.cex = 2)
#abline(v=c(calc.yr,calc.yr-yrs.window+1),col="red")

addFit(data.df = test.df.sub, coeff = list(intercept = est.jags$summary["intercept","50%"],
                                           slope = est.jags$summary["slope","50%"] )
)

###############################################################
# DEV FOR ROSS'S PLOT



test.reg <- plotFit(data.plot = test.df.sub,
                    fit.plot = est.jags$summary
                    ,title = "Fitted Trend",
                    y.lab = "Mature Individuals",
                    exp.do = FALSE)
test.reg$data
test.reg$plot


test.exp <- plotFit(data.plot = test.df.sub,
                    fit.plot = est.jags$summary
                    ,title = "Fitted Trend",
                    y.lab = "Mature Individuals",
                    exp.do = TRUE)

test.exp$data
test.exp$plot
