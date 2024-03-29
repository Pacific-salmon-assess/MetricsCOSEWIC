#' Function to run stan to esimate probabiltiy of declines
#' Created by: Carrie Holt
#' params list for function params
#' @param du.label [character()] # pathing for the project, used through here::here
#' @param du.df [data.frame()] # input data
#'  # Abd is supplied as raw will be required to be converted into logAbd
#'  # Columns required by title:
#'    # 'Year', 'Abd', 'logAbd', 'DU'

#' @param yrs.window [numeric()] [vector()] # ? - guessing at from Larkin
#' @param perc.change.bm [character()] [vector()] # ? - guessing at from Larkin

#' @param calc.yr [numeric()]
#'
#' @param standardize.data [logical()]
#' @param scenario.name [character()] # for creating the scenario outputs folder
#' @param mcmc.plots [logical()]
#' @param prior_sigma_type [character()]
#' @param prior_sigma [numeric()]
#' @param H0 [logical()]

#' @param path.results [character()] # NO LONGER USED

#' @importFrom rlang .data
#' @importFrom rlang :=

#' @export
#'

run.stan <- function(du.label,
                     du.df,
                     raw = FALSE,
                     gen = 3, # default 3 generations
                     yrs.window = (3 * gen) + 1, # default equation using gen
                     # gen = 3, # default?
                     # yrs.window = (3 * gen) + 1, # default?
                     perc.change.bm = c(-30,-50,-70),
                     # calc.yr, # *Tor*: can this be set to NULL?
                      # calc.yr doesn't appear elsewhere in the function
                     standardize.data = TRUE,
                     scenario.name = "untitled_scenario", # filler title
                     # path.results="", # NO LONGER USED
                     mcmc.plots = TRUE,
                     prior_sigma_type = "exp",
                     prior_sigma=2.5,
                     H0=FALSE)
# end function definition


# run.stan function code core and outputs
{

  year.scale <- FALSE #standardize x-axis (years) as well. This is not needed

  #### Data checks and Testing ####

  # Function to do the logAbd transformation for the
  # user. Keep in mind, log() R defaults to nat log (e)
  # consider adding a default function that if logAbd is not detected then run
  if (raw==TRUE) {
    du.df <- du.df %>% mutate(logAbd = log(Abd))
  }

  if (!"logAbd" %in% colnames(du.df)) {
    # du.df <- du.df %>% mutate(logAbd = log(Abd))
    stop("Error: 'logAbd' column not found in the data frame. Please set 'raw = TRUE' and/or ensure
         your data includes: 'Year', 'DU', and 'logAbd'.")
  }

  if (!"Year" %in% colnames(du.df)) {
    stop("Error: 'Year' column not found in the data frame.Please ensure
         your data includes: 'Year', 'DU', and 'logAbd'.")
  }

  if (!"DU" %in% colnames(du.df)) {
    stop("Error: 'DU' column not found in the data frame.Please ensure
         your data includes: 'Year', 'DU', and 'logAbd'.")
  }

  #### Create folder for outputs ####
  out.dir.stock <- here::here(du.label)

  if (file.exists(out.dir.stock) == FALSE){
    dir.create(out.dir.stock)
  }

  out.dir <- here::here(du.label, scenario.name) #here(path.results,scenario.name)

  if (file.exists(out.dir) == FALSE){
    dir.create(out.dir)
  }

  #### Standardize data ####

  if (standardize.data){
    du.df.raw <- du.df# Save raw values
    du.df <- du.df %>% mutate(mean= mean(na.omit(logAbd)), sd= sd(na.omit(logAbd)))
    du.df <- du.df %>% mutate(logAbd = (logAbd-mean)/sd)
  }

  #### Priors ####

  # ORIGINAL PRIORS- for unstandardized data
  if(!standardize.data){
    # Priors for intercept, normally distributed: ~N(mean, sig)
    intercept_mean <-  median(du.df$logAbd, na.rm=T)
    intercept_sig <-  max(du.df$logAbd, na.rm=T)
    # max value in the time-series is an upper bound on plausible sigmas on y-i

    # Priors for slope, normally distributed: ~N(mean, sig)
    slope_mean <-  0
    slope_sig <-  max(du.df$logAbd, na.rm=T) /
      (max(du.df$Year) - min(du.df$Year))
    # max rise/max run is an upper bound on plausible sigmas on slope
  }

  # UPDATED PRIORS- for standardized data
  if(standardize.data){
    # Priors for intercept, normally distributed: ~N(mean, sig)
    intercept_mean <-  0# mean(du.df$logAbd, na.rm=T) #this value is ~0
    intercept_sig <-   prior_sigma# 2.5* sd(du.df$logAbd, na.rm=T) #this value is ~2.5.
    # 2.5 is recommended by Gelman and used in rstanarm as weakly informative
    # https://cran.r-project.org/web/packages/rstanarm/vignettes/priors.html
    # see also https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations

    # Priors for slope, normally distributed: ~N(mean, sig)
    slope_mean <-  0
    slope_sig <-  prior_sigma
  }

  # ORIGINAL Priors for residual variance, inverse gamma distributed:
  # ~Inv_Gamma(alpha,beta), where alpha = beta = 'Sig_Gam_Dist'
  Sig_Gam_Dist <-  0.1#0.00001

  # UPDATED priors for residual variance, sigma exponentially distributed:
  # exponential(rate = 1)
  # https://cran.r-project.org/web/packages/rstanarm/vignettes/priors.html
  exp_rate <- 1
  if(year.scale) {exp_rate <- 1/sd(0:(length(du.df$Year)-1))}


  #### Set up data inputs for Stan ####
  # Does this need to be changed to match Larkin: define Stan data section?

  # test for missing value codes

  #du.df$logAbd[c(2)] <- NA

  data <- list()
  data$Year <- 0:(length(du.df$Year)-1)
  if(year.scale) data$Year <- scale(data$Year)[,1]
  data$logAbd <- replace_na(du.df$logAbd, 99) #du.df$logAbd
  data$N <- dim(du.df)[1]
  data$intercept_mean <- intercept_mean
  data$Sig_Gam_Dist <- Sig_Gam_Dist
  data$exp_rate <- exp_rate
  data$slope_mean <- slope_mean
  data$intercept_sig <- intercept_sig
  data$slope_sig <- slope_sig
  # Which years have observations? (i.e., are not NAs)
  data$logAbd_obs <- as.numeric(!is.na(du.df$logAbd))
  data$slope <- 0# for H0 model
  data$intercept <- 0# for HO model

  #### Original stan code model
  # Included original stan model code for comparison during testing

  # Original
  # if(prior_sigma_type=="exp"){
  #   if(!H0) stan_fit <- stan(file = 'MetricsCOSEWIC/inst/stan/linear-exp.stan', data = data,
  #                            iter = 10000, chains = 4, thin =10,
  #                            control = list(adapt_delta = 0.95), refresh = 0)
  #   if(H0) stan_fit <- stan(file = 'MetricsCOSEWIC/inst/stan/linear-exp-H0.stan', data = data,
  #                           iter = 10000, chains = 4, thin =10,
  #                           control = list(adapt_delta = 0.95), refresh = 0)
  # }

  # system.file attempt
  if(prior_sigma_type=="exp"){
    stan_file <- if (H0) {
      system.file('stan/linear-exp-H0.stan', package = "MetricsCOSEWIC")
    } else {
      system.file('stan/linear-exp.stan', package = "MetricsCOSEWIC")
    }

    stan_fit <- stan(file = stan_file, data = data,
                      iter = 10000, chains = 4, thin =10,
                      control = list(adapt_delta = 0.95), refresh = 0)
  }

  # Original
  # if(prior_sigma_type=="invgamma"){
  #   stan_fit <- stan(file = 'MetricsCOSEWIC/inst/stan/linear-invgamma.stan', data = data, iter = 10000,
  #                    chains = 4, thin =10,  control = list(adapt_delta = 0.95),
  #                    refresh = 0)
  # }

  # system.file attempt
  if(prior_sigma_type=="invgamma"){
    stan_file <- system.file('stan/linear-invgamma.stan', package = "MetricsCOSEWIC")
    stan_fit <- stan(file = stan_file, data = data, iter = 10000,
                      chains = 4, thin =10,  control = list(adapt_delta = 0.95),
                      refresh = 0)
  }

  #### Create stan model object ####
  # Now translated for cmdstanr::cmdstan_model() instead of base stan()

  # E.g. from Larkin for Creating model object
  # mod <- cmdstanr::cmdstan_model(
  #   stan_file = system.file(
  #     "stan","linear-exp-H0",
  #     package = "MetricsCOSEWIC",
  #     mustWork = TRUE
  #   ),
  #   include_path = system.file(
  #     "stan",
  #     package = "MetricsCOSEWIC",
  #     mustWork = TRUE
  #   )
  # )

  # Question: How will this work with 3 stan models?
  # Will we need three different runStan functions or will there
  # be a way to loop through depending on a mod to select the correct
  # system file?

  # using a if loop based on function parameters:
    # prior_sigma_type=='exp'
    # H0 or !H0

# BEGIN NEW CODE
    # Error: no files found using system.file
  # Commented out for the moment bug-fixing
  # if(prior_sigma_type=="exp"){
  #   if(!H0) mod <- cmdstanr::cmdstan_model(
  #       stan_file = here::here('MetricsCOSEWIC/inst/stan/linear-exp.stan')
  #        # stan_file = system.file(
  #        #   "stan","linear-exp",
  #        #   package = "MetricsCOSEWIC",
  #        #   mustWork = TRUE
  #        # ),
  #       ,
  #        include_path = system.file(
  #          "stan",
  #          package = "MetricsCOSEWIC",
  #          mustWork = TRUE
  #        )
  #      )

  # Commented out for the moment bug-fixing
  #   if(H0) mod <- cmdstanr::cmdstan_model(
  #     stan_file = here::here('MetricsCOSEWIC/inst/stan/linear-exp-H0.stan')
  #     # stan_file = system.file(
  #     #   "stan","linear-exp-H0",
  #     #   package = "MetricsCOSEWIC",
  #     #   mustWork = TRUE
  #     # )
  #     ,
  #     include_path = system.file(
  #       "stan",
  #       package = "MetricsCOSEWIC",
  #       mustWork = TRUE
  #     )
  #   )
  # }

  # Commented out for the moment bug-fixing
  # if(prior_sigma_type=="invgamma"){
  #   mod <- cmdstanr::cmdstan_model(
  #     stan_file = here::here('MetricsCOSEWIC/inst/stan/linear-invgamma.stan')
  #     # stan_file = system.file(
  #     #   "stan","linear-invgamma",
  #     #   package = "MetricsCOSEWIC",
  #     #   mustWork = TRUE
  #     # )
  #     ,
  #     include_path = system.file(
  #       "stan",
  #       package = "MetricsCOSEWIC",
  #       mustWork = TRUE
  #     )
  #   )
  # }
  #
  # #### Model Sampling/Optimization ####
  # # Where can I put in the model parameters e.g. iterations, chains, thins, etc.
  # # Consider making these separate objects prior to optimization?

  # Fit with sampling function (MCMC)
  # Commented out while bug fixing
  # fit_sample <- mod$sample(
  #   data = data,
  #   refresh = 0,
  #   iter_sampling = 10000, # might need to be changed to iter_warmup or iter_sampling
  #   chains = 4,
  #   thin = 10,
  #   adapt_delta = 0.95# control
  # )

# END NEW CODE

# Model optimization
  # Unsure if model optimization is actually required for this model setup
  # Fit with optimize function (MLE)
  # fit_optim <- mod$optimize(
  #   data = data,
  #   #data = stan_data, # **** THIS DOES NOT MATCH ****
  #   refresh = 0,
  #   # seed = 123
  #   iter = 10000
  #   # threads
  #   #thin = 10
  #   # control
  # )

  # runStan.R (original) code used the following values:
    # iter = 10000
    # chains = 4
    # thin = 10
    # control = list(adapt_delta = 0.95)
    # refresh = 0


  #### Pull out parameter estimates ####

  # Consider commenting out everything below and testing line-by-line.
  # That way you can test that the function samples correctly and then
  # check the model output differences between stan and cmdstanr.


  # **** These outputs MUST be changed to match the above changes with
  # cmdstanr ****

  # Sections:
    # - converged slope where rhat is < 1.02
    # - slope, intercept, and sigma
    # - Summary of fit
    # - Log Abundance of Fits, Predicted, Prior Predictions
    # - Log marginal likelihood
    # - Posteriors
      # - Used to calculate percent changes for visualization

  # The new model is now named: "fit_sample"
    # Should be able to find and replace "stan_fit" or "fit_sample"
    # bayesplot should understand the difference directly between the two
    # only 3 changes need to be made in the following lines, then
    # it is transferred into All_Ests

  slope.converged <- bayesplot::rhat(stan_fit)["slope"] < 1.02
    # change stan_fit to fit_sample

  conv.out <- bayesplot::rhat(stan_fit)[c("slope", "intercept", "sigma")]
    # change stan_fit to fit_sample


  All_Ests <- data.frame(summary(stan_fit)$summary)
  All_Ests$Param <- row.names(All_Ests)

  logAbd_Fits_Stan <- All_Ests[grepl("logAbd_Fit_out", All_Ests$Param),  ]
  logAbd_Preds_Stan <- All_Ests[grepl("logAbd_Pred", All_Ests$Param),  ]
  logAbd_PriorPreds_Stan <- All_Ests[grepl("logAbd_PriorPred", All_Ests$Param),  ]


  # Compute log marginal likelihood via bridge sampling for H1
  logML <- bridge_sampler(stan_fit, silent = TRUE)


  # I don't think this is used, though may be helpful for plotting CI and prediction interval (after rescaling predictions)
  FitsDF <- data.frame(Year = du.df$Year, R = du.df$logAbd, Fit = logAbd_Fits_Stan$X50.,
                       Year = 1:dim(logAbd_Fits_Stan)[1],
                       CI_up = logAbd_Fits_Stan$X97.5.,
                       CI_low = logAbd_Fits_Stan$X2.5.,
                       Pred = logAbd_Preds_Stan$X50.,
                       Pred_up = logAbd_Preds_Stan$X97.5.,
                       Pred_low = logAbd_Preds_Stan$X2.5.)


  if(!H0){
    # get  posteriors
    fit_values <- rstan::extract(stan_fit)
    intercept_Post <- fit_values$intercept
    slope_Post <- fit_values$slope

    if(standardize.data){
      # Unstandardize posteriors
      if(!year.scale){
        sd <- du.df %>% pull(sd) %>% unique()
        mean <- du.df %>% pull(mean) %>% unique()
        intercept_Post <- fit_values$intercept * sd + mean
        slope_Post <- fit_values$slope * sd
      }

      if(year.scale){
        sd.y <- du.df %>% pull(sd) %>% unique()
        mean.y <- du.df %>% pull(mean) %>% unique()
        sd.x <- sd(0:(length(du.df$Year)-1))
        mean.x <- mean(0:(length(du.df$Year)-1))
        intercept_Post <- mean.y - fit_values$slope * (sd.y/sd.x) * mean.x
        slope_Post <- fit_values$slope * (sd.y/sd.x)

      }

    }

    # I don't think this list, 'out', is needed, could refer to intercept_Post and slope_Post directly below
    # Return fit and predicted values
    out <- list()
    out$Fits <- FitsDF
    out$Ests <- All_Ests
    out$intercept_Post <- intercept_Post
    out$slope_Post <- slope_Post

    # Calculate % change from the fit at the start and end of the 13 years
    mcmc.samples <- data.frame(int=out$intercept_Post, slope=out$slope_Post,
                               Perc_Change = NA,Perc_Change_Raw = NA)
    if(!year.scale) {
      mcmc.samples <- mcmc.samples %>%
        mutate(Fit_Start = int + slope * data$Year[1] ) %>%
        mutate(Fit_End = int + slope * data$Year[yrs.window])
    }

    if(year.scale) {
      yrs <-  0:(length(du.df$Year)-1)
      mcmc.samples <- mcmc.samples %>%
        # mutate(Fit_Start = int + slope * yrs[1] ) %>%
        # mutate(Fit_End = int + slope * yrs[yrs.window])
        mutate(Fit_Start = int + slope * yrs[length(yrs) - yrs.window + 1] ) %>%
        mutate(Fit_End = int + slope * yrs[length(yrs)])
    }

    # Identify any years with negative start years (only occurs if abundances are
    # scaled prior to log transform)
    neg.start.idx <-  mcmc.samples[,"Fit_Start"] < 0


    mcmc.samples[,"Perc_Change"][!neg.start.idx] <-
      (exp(mcmc.samples[,"Fit_End"][!neg.start.idx]) - exp(mcmc.samples[,"Fit_Start"][!neg.start.idx])) /
      exp(mcmc.samples[,"Fit_Start"][!neg.start.idx]) * 100

    mcmc.samples[,"Perc_Change"][neg.start.idx] <-
      (exp(mcmc.samples[,"Fit_End"][neg.start.idx]) + exp(mcmc.samples[,"Fit_Start"][neg.start.idx])) /
      exp(abs(mcmc.samples[,"Fit_Start"][neg.start.idx])) * 100

    mcmc.summary <- as.data.frame(summary(stan_fit))


    pchange <- median(mcmc.samples[,"Perc_Change"])
    pchange.df <- data.frame(pchange=mcmc.samples[,"Perc_Change"])
    probdecl <- data.frame(BM = perc.change.bm,ProbDecl = NA )

    for(i in 1:length(perc.change.bm)){

      probdecl[i,2] <- sum(mcmc.samples[,"Perc_Change"] <= perc.change.bm[i]) / dim(mcmc.samples)[1] *100

    }
    probdecl <- probdecl %>% arrange(BM)


    # Plot probability distribution of declines relative to thresholds
    p.dist <- ggplot(pchange.df,aes(pchange)) + geom_density() +
      geom_vline(xintercept=-30, linetype="dashed", colour="yellow") +
      geom_vline(xintercept=-50, linetype="dashed", colour="orange") +
      geom_vline(xintercept=-70, linetype="dashed", colour="red")
    ggsave("prob_declines.pdf", p.dist, path=out.dir)


    if(mcmc.plots){
      posterior <- as.matrix(stan_fit)
      plot_title <- ggtitle("Posterior distributions- slope",
                            "with medians and 80% intervals")
      p1 <- mcmc_areas(posterior,
                       pars = c("slope"),
                       prob = 0.8) + plot_title

      plot_title <- ggtitle("Posterior distributions- intercept",
                            "with medians and 80% intervals")
      p2 <- mcmc_areas(posterior,
                       pars = c("intercept"),
                       prob = 0.8) + plot_title

      posterior2 <- extract(stan_fit, inc_warmup = FALSE, permuted = FALSE)

      color_scheme_set("mix-blue-pink")
      p3 <- mcmc_trace(posterior2,  pars = c("slope", "intercept"), n_warmup = 0,
                       facet_args = list(nrow = 2, labeller = label_parsed)) +
        facet_text(size = 15)

      # Get observed data vector for posterior predictive check
      logAbd_obs <- data$logAbd
      logAbd_obs[which(logAbd_obs==99)]<- NA #Replace 99 with NAs
      #launch_shinystan(stan_fit)

      # Get posterior predicted values for ndraws
      pp <- posterior[,grepl("logAbd_Pred", colnames(posterior))]
      ndraws <- 200
      pp <- data.frame(pp) %>% sample_n(ndraws)
      pp <- data.frame(t(pp))
      pp$obs <- logAbd_obs

      pp_long <- pivot_longer(pp, cols=1:(ndraws+1))
      pp_long$obs <- 1
      pp_long <- pp_long %>% mutate(obs = replace(obs, name == "obs", 0))


      # Plot posterior predictive check with observe distribution of log(Abd)
      p4 <- ggplot(pp_long, mapping = aes(x=value)) +
        geom_density(aes( group=factor(name)), color = "grey", alpha=0.1)  +
        geom_density(data = subset(pp_long, obs==0), colour="black", size=1.4) +
        ggtitle("Posterior Predictive Disributions")

      # Get prior predictive distribution
      priorp <- posterior[,grepl("logAbd_PriorPred", colnames(posterior))]
      priorp <- data.frame(priorp)

      priorp_long <- pivot_longer(priorp, cols=1:(dim(priorp)[2]), names_to="Years", names_prefix="logAbd_PriorPred.")
      # Unstandardize prior predictions
      if (standardize.data){
        mean.y <- mean(du.df.raw$logAbd, na.rm=T)
        sd.y <- sd(du.df.raw$logAbd, na.rm=T)
        priorp_long <- priorp_long %>% mutate(rawValue =
                                                (value/sd.y) + mean.y)

      }
      if (!standardize.data){
        du.df.raw<- data.frame(logAbd =  du.df$logAbd)
        priorp_long <- priorp_long %>% mutate(rawValue = value)
      }

      # Plot prior predictive check

      priorp_long <- priorp_long %>% mutate(Years=as.factor(as.numeric(Years)))

      p5 <- ggplot(priorp_long, mapping = aes(x=rawValue, y=as.factor(Years))) +
        geom_density_ridges() +
        ggtitle("Prior Predictive Disributions", "with range of observed data between vertical dashed lines") +
        ylab("Years") + xlab("Log(abundance)") +
        geom_vline(xintercept=min(du.df.raw$logAbd, na.rm=T), linetype="dotted") +
        geom_vline(xintercept=max(du.df.raw$logAbd, na.rm=T), linetype="dotted") +
        xlim(-50, 100) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      # xlim(0, 500000) +
      # geom_vline(xintercept=exp(min(du.df.raw$logAbd, na.rm=T)), linetype="dotted") +
      # geom_vline(xintercept=exp(max(du.df.raw$logAbd, na.rm=T)), linetype="dotted")


      ggsave("slope_posterior.pdf", p1, path=out.dir)
      ggsave("intercept_posterior.pdf", p2, path=out.dir)
      ggsave("trace_plot.pdf", p3, path=out.dir)
      ggsave("posterior_predictive_check.pdf", p4, path=out.dir)
      ggsave("prior_predictive_check.pdf", p5, path=out.dir)
      ggsave("posterior_predictive_check.png", p4, path=out.dir)
      ggsave("prior_predictive_check.png", p5, path=out.dir)
    }# End of mcmc.plots

    write.csv(mcmc.samples,file=paste(out.dir,"/mcmc_samples.csv",sep=""))
    write.csv(conv.out,file=paste(out.dir,"/convergenceRhat.csv",sep=""))


    # Return fit and predicted values

    # if(out.type=="short"){ out.list <- list(pchange = pchange,probdecl = probdecl, summary = mcmc.summary,
    #                                         slope.converged = slope.converged, conv.details = conv.out)}
    #
    out.list <- list(pchange = pchange,
                     probdecl = probdecl,
                     summary = mcmc.summary,
                     slope.converged = slope.converged,
                     conv.details = conv.out,
                     samples = mcmc.samples,
                     fit.obj = stan_fit,
                     logML = logML# ,
                     # cmdstan = fit_sample)
    )

  }# End of if (!H0)

  # For null hypothesis of slope=0, return only marginal likelihood for BF
  if(H0) out.list <- list(logML = logML)

  return(out.list)

  }
