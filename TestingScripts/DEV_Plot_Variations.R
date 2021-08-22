

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


# zoomed version

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

###############################################################
# DEV FOR ROSS'S PLOT


plotFit <- function(data.plot,fit.plot,title = "Fitted Trend",y.lab = "Abundance",exp.do = FALSE){


  plot.df <- data.frame(Year = data.plot$Year,
                        Val = log(data.plot$Spn)  ) %>%
    mutate(Med = fit.plot["intercept","50%"] + fit.plot["slope","50%"] * 1:length(data.plot$Year)) %>%
    mutate(p2.5 = fit.plot["intercept","2.5%"] + fit.plot["slope","2.5%"] * 1:length(data.plot$Year)) %>%
    mutate(p97.5 = fit.plot["intercept","97.5%"] + fit.plot["slope","97.5%"] * 1:length(data.plot$Year))

  if(exp.do){ plot.df <- plot.df %>% mutate_at(2:5,exp)  }

  fit.ggplot <- ggplot() +
    geom_line(aes(x=plot.df$Year, y=plot.df$Val)) +
    geom_point(aes(x=plot.df$Year, y=plot.df$Val), shape=16) +
    geom_line(aes(x=plot.df$Year, y=plot.df$Med), linetype='solid') +
    geom_line(aes(x=plot.df$Year, y=plot.df$p2.5), linetype='dotdash') +
    geom_line(aes(x=plot.df$Year, y=plot.df$p97.5), linetype='dotdash') +
    scale_x_continuous(breaks= seq(min(plot.df$Year)-1,max(plot.df$Year)+1,2)) +
    xlab('') + ylab(y.lab) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +

    ggtitle(title)

  return(list(plot = fit.ggplot,data = plot.df))

}


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
