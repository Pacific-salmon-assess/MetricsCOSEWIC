#' calcPercChangeSimple
#'
#' this function applies the function \code{\link{comparePercChange}} to each DU in a data set and generates summary outputs plus diagnostic plots.
#' @param data.df data frame with columns DU, Year, Abd
#' @param window.df data frame with time windows for each DU (e.g. 3 gen +1)
#' @param plot.file text string with path and filename for pdf of diagnostic plots
#' @export


multiFit  <- function(data.df, window.df, plot.file =  "PercChange_Plots.pdf"){

out.df <- data.frame() # as per https://stackoverflow.com/a/32342704

loop.start <- proc.time()


pdf(plot.file ,onefile=TRUE,height=8.5, width = 11)

for(du.do in sort(unique(data.df$DU)) ){

print(paste("starting",du.do, "--------------------------------"))

layout(matrix(c(1,1,2,3),ncol=2,byrow=TRUE))

df.use <- data.df %>% dplyr::filter(DU == du.do) %>%  select(Year,Abd)
last.yr <- max(df.use$Year)

fit.out <- comparePercChange(du.label = du.do,
                              du.df = df.use,
                              yrs.window = window.df  %>% dplyr::filter(DU == du.do) %>%  select(Window) %>% unlist,
                             calc.yr = last.yr,
                              samples.out = FALSE,
                             plot.pattern = TRUE,
                             plot.posteriors = TRUE,
                             plot.boxes  = TRUE)


out.df <- rbind(out.df, cbind(DU = du.do, Year = last.yr, rownames_to_column(as.data.frame(fit.out$Summary),"Var")))


title(main = du.do, outer=TRUE,line=-2,col.main = "darkblue",cex.main=1.7)

}


dev.off()



summary.out <- out.df %>% select(DU,Year,Var, pchange) %>% dplyr::filter(grepl("MLE", Var ) | grepl("Med", Var )) %>%
   mutate(pchange = round(pchange,1)) %>%
   pivot_wider(id_cols = c(DU,Year),names_from = Var, values_from = pchange ) %>%
  rowwise() %>% mutate(Min = min(MLE,Jags_Med, RStanArm_Med,na.rm= TRUE),
                       Max = max(MLE,Jags_Med, RStanArm_Med,na.rm= TRUE)) %>%
  mutate(Diff = Max- Min) %>% arrange(Min)


#head(summary.out)


print("total time for this set of perc change calcs")
print(proc.time() - loop.start )


out.list <- list(Output = out.df, Summary = summary.out )


return(out.list)




} # end multiFit function
