#' comparePercChange
#'
#' this function calculates 3 alternative versions of the Perc Change metric: 1 deterministic (MLE) and 2 Bayesian (JAGS via rjags, STAN via RStanArm)
#' @param du.label short label for the the Designatable Unit (DU)
#' @param du.df data frame with the DU time series for : 2 columns, first one is "Year", second the abundance.
#' @param yrs.window number of years to use for the percent change calculation (e.g. 3 gen +1)
#' @param perc.change.bm  benchmark for Prob(Decl>BM), default = c(-30,-50,-70)
#' @param calc.yr year for which the perc change is calculated
#' @param samples.out if TRUE, include the posterior samples in the output
#' @param plot.pattern if TRUE, create a plot of the time series with alternative slope estimates
#' @param plot.distributions if TRUE, create a plot of the posterior distributions (kernel density plot)
#' @param plot.boxes if TRUE, create a plot of the posterior distributions (box plots)
#' @export


comparePercChange  <- function(du.label,du.df, yrs.window,
perc.change.bm = c(-30,-50,-70), calc.yr, samples.out = TRUE,
plot.pattern = TRUE, plot.fitted = TRUE, plot.posteriors = TRUE, plot.boxes  = TRUE,  do.rstanarm = FALSE, print.mcmc = TRUE){

#warning("NOTE: input time series is log-transformed before slope calc, but Perc Change estimate is backtransformed")

du.df.sub <- du.df %>% dplyr::filter(Year > calc.yr - yrs.window, Year <= calc.yr)
#du.df.sub

# If any zeroes in the data, then the log-transform below causes problems
# using the strategy from Perry et al 2021 (https://journals.plos.org/plosone/article/comments?id=10.1371/journal.pone.0245941)
# as suggested by Carrie Holt at https://github.com/SOLV-Code/MetricsCOSEWIC/issues/15

zero.idx <- du.df.sub[,2] == 0
zero.idx[is.na(zero.idx)] <- FALSE

if(sum(zero.idx) > 0) {warning(paste0(sum(zero.idx),"records of 0 were replaced with a random value greater than 0 and less than one-half of the lowest non-zero value in the data for that variable, as per Perry et al 2021"))}

du.df.sub[zero.idx,2] <- runif(sum(zero.idx,na.rm = TRUE),0.00000001, min(du.df.sub[!zero.idx,2],na.rm = TRUE)/2)


# if too many NA, then things can go haywire in the MCMC.
# -> Need at least half the data points before trying MCMC
if(sum(!is.na(du.df.sub[,2])) < length(du.df.sub[,2])/2 ){na.skip <- TRUE} # this results in NA outputs, but stops crashing
if(sum(!is.na(du.df.sub[,2])) >= length(du.df.sub[,2])/2 ){na.skip <- FALSE}


percentile.values <- c(0.025,0.25,0.5,0.75,0.975)
percentile.labels <- c("p2.5","p25","Med","p75","p97.5","Rhat")
extract.labels <- c("2.5%","25%","50%","75%","97.5%","Rhat")

if(do.rstanarm) {
out.mat <- matrix(NA,ncol = 3, nrow=13,
                        dimnames = list(c("MLE",paste("Jags",percentile.labels,sep="_"),
													paste("RStanArm",percentile.labels,sep="_")),
                                      c("pchange","slope","intercept"))
                            )
}

if(!do.rstanarm) {
out.mat <- matrix(NA,ncol = 3, nrow=7,
                        dimnames = list(c("MLE",paste("Jags",percentile.labels,sep="_")),
                                      c("pchange","slope","intercept"))
                            )
}



out.probdecl <- data.frame(BM = perc.change.bm, ProbDecl = NA)





if(!na.skip){

est.simple <- calcPercChangeSimple(log(du.df.sub[,2]))

est.jags <- calcPercChangeMCMC(vec.in = log(du.df.sub[,2]),
                               method = "jags",
                               model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                               perc.change.bm = perc.change.bm,
                               out.type = "long",
                               mcmc.plots = FALSE,
                               convergence.check = FALSE,# ??Conv check crashes on ts() ??? -> change to Rhat check
                               print.mcmc = print.mcmc
                               )

if(do.rstanarm) {est.rstanarm <- calcPercChangeMCMC(vec.in = log(du.df.sub[,2]),
                                   method = "rstanarm",
                                   model.in = NULL, # hardwired regression model form, so no input
                                   perc.change.bm = perc.change.bm,
                                   out.type = "long",
                                   mcmc.plots = FALSE,
                                   convergence.check = FALSE,
                                   print.mcmc = print.mcmc)
								   }
if(!do.rstanarm){est.rstanarm <- NULL}



out.mat["MLE",] <-round(c(est.simple$pchange,est.simple$slope,est.simple$intercept),5)


out.mat[grepl("Jags",dimnames(out.mat)[[1]]),"slope"] <- round(est.jags$summary["slope",extract.labels],5)
out.mat[grepl("Jags",dimnames(out.mat)[[1]]),"intercept"] <- round(est.jags$summary["intercept",extract.labels],5)
out.mat[grepl("Jags",dimnames(out.mat)[[1]]),"pchange"] <- c(quantile(est.jags$samples$Perc_Change,probs = percentile.values),NA)

out.probdecl <- round(est.jags$probdecl,5)

if(do.rstanarm) {

out.mat[grepl("RStanArm",dimnames(out.mat)[[1]]),"slope"] <- unlist(round(est.rstanarm$summary["slope",extract.labels],5))
out.mat[grepl("RStanArm",dimnames(out.mat)[[1]]),"intercept"] <- unlist(round(est.rstanarm$summary["intercept",extract.labels],5))

out.mat[grepl("RStanArm",dimnames(out.mat)[[1]]),"pchange"] <- c(quantile(est.rstanarm$samples$Perc_Change,probs = percentile.values),NA)

out.probdecl.rstanarm <- round(est.rstanarm$probdecl,5)

}


if(do.rstanarm) {
percchange.df <- data.frame(
  MLE = c(NA,NA,est.simple$pchange,NA, NA),
  Jags = quantile(est.jags$samples$Perc_Change,probs = percentile.values),
  Stan =  quantile(est.rstanarm$samples$Perc_Change,probs = percentile.values))
}

if(!do.rstanarm) {
percchange.df <- data.frame(
  MLE = c(NA,NA,est.simple$pchange,NA, NA),
  Jags = quantile(est.jags$samples$Perc_Change,probs = percentile.values) )
}








if(plot.pattern){

plotPattern(yrs = du.df$Year ,vals = log(du.df[,2]),
            width=1,color="darkblue",
            yrs.axis=TRUE,vals.axis=TRUE,
            vals.lim=NULL, hgrid=TRUE,vgrid=FALSE,
            pch.val=19,pch.bg=NULL)


if(do.rstanarm) {addFit(data.df = du.df.sub, coeff = list(intercept = est.rstanarm$summary["intercept","50%"],
                                           slope = est.rstanarm$summary["slope","50%"] ) )  }

addFit(data.df = du.df.sub, coeff = list(intercept = est.jags$summary["intercept","50%"],
                                           slope = est.jags$summary["slope","50%"] ) )
addFit(data.df = du.df.sub, coeff = list(intercept = est.simple$intercept,slope = est.simple$slope )  )


title(main = du.label, outer=TRUE,line=-2,col.main = "darkblue",cex.main=1.7)

}







if(plot.posteriors){

if(do.rstanarm) {

plotDistribution(
  x.lab = "Perc Change",
  samples = list(jags = est.jags$samples$Perc_Change ,rstanarm = est.rstanarm$samples$Perc_Change   ),
  det.est = est.simple$pchange,
  plot.range = c(-90,90) #NULL #c(-90,90)
	)
}



if(!do.rstanarm) {

plotDistribution(
  x.lab = "Perc Change",
  samples = list(Bayesian = est.jags$samples$Perc_Change ),
  det.est = est.simple$pchange,
  plot.range = c(-90,90) #NULL #c(-90,90)
	)
}
title(main = du.label, outer=TRUE,line=-2,col.main = "darkblue",cex.main=1.7)
}





if(plot.boxes){
plotBoxes(box.df = percchange.df %>% dplyr::rename(Deterministic = MLE, Bayesian = Jags), y.lab  = "Perc Change",   det.est = est.simple$pchange, plot.range = NULL)
title(main = du.label, outer=TRUE,line=-2,col.main = "darkblue",cex.main=1.7)
}



if(plot.fitted){


gg.plot <- plotFit(data.plot = du.df.sub,
                    fit.plot = est.jags$summary,
					title = "Fitted Trend (Bayesian / JAGS)",
                    y.lab = "Ln(Mature Individuals)",
                    exp.do = FALSE)

print(gg.plot$plot)

}

} # end if !na.skip



if(!do.rstanarm) out.list <- list(Summary = out.mat, ProbDecl = out.probdecl)
if(do.rstanarm) out.list <- list(Summary = out.mat, ProbDecl = out.probdecl,
                                 Probdecl.rstan = out.probdecl.rstanarm)


if(samples.out & !na.skip){  out.list <- c(out.list, samples = list(rstanarm = est.rstanarm$samples, jags = est.jags$samples)) }
if(samples.out & na.skip){  out.list <- c(out.list, samples = list(rstanarm = NA, jags = NA)) }




return(out.list)

} # end comparePercChange function

