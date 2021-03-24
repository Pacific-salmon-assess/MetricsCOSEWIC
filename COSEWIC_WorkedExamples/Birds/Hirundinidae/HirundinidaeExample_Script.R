
##################################################################################
## WORKED EXAMPLE: HIRUNDINIDAE (Swallows, Martins)

library(tidyverse)
library(MetricsCOSEWIC)


# load the packages
library(rjags)
library(rstanarm)
library(rstan)
library(coda)
library(tidyverse)


if(FALSE){

## run this piece only when you want to make sure the most recent pkg is being used
install.packages("devtools") # Install the devtools package
library(devtools) # Load the devtools package.
install_github("SOLV-Code/MetricsCOSEWIC",
               dependencies = TRUE,
               build_vignettes = FALSE)
}


library(MetricsCOSEWIC)



#------------------------------------------------------------------
# Part 1: Get the data and generate summary plots


folder.path <- "COSEWIC_WorkedExamples/Birds/Hirundinidae/"

# read in the data
data.use <- read.csv(paste0(folder.path,"Hirundinidae_Sample_Data.csv"),stringsAsFactors = FALSE)
head(data.use )


summary.df <- data.use %>% group_by(Species,Region) %>%
                summarize(NumYears = n(),
                          Min_Index = min(Index,na.rm=TRUE),
                          Mean_Index = mean(Index,na.rm=TRUE),
                          Max_Index = max(Index,na.rm=TRUE))
summary.df
write.csv(summary.df, paste0(folder.path,"Hirundinidae_Output/Hirundinidae_DataSummary.csv"),row.names = FALSE)


species.list <- sort(unique(data.use$Species))
region.list <- unique(data.use$Region)


yr.range <- range(data.use$Year)


for(plot.type in c("Medians","Ranges")){


for(species.plot in species.list){

png(filename =  paste0(folder.path,"Hirundinidae_Output/TimeSeries_",species.plot,"_",plot.type,".png"),
      width = 480*3.5, height = 480*4, units = "px", pointsize = 14*2.5, bg = "white",  res = NA)

par(mfrow = c(3,2))

# start looping through regions

for(region.plot in region.list){


plot.df <- data.use %>% dplyr::filter(Species == species.plot, Region == region.plot) %>%
              arrange(Year) # just in case
plot.df

if(plot.type == "Medians"){ y.lim <- range(plot.df$Index,na.rm=TRUE)}
if(plot.type == "Ranges"){ y.lim <- c(min(plot.df$Index_q_0.025,na.rm=TRUE),max(plot.df$Index_q_0.975,na.rm=TRUE))}

plot(plot.df$Year,plot.df$Index,ylim=y.lim,xlim=yr.range,type= "l", pch=19, col="darkblue", bty="n", lwd=2,
     xlab="Year", ylab = "Index",main=region.plot)

if(plot.type == "Ranges"){
  polygon(c(plot.df$Year,rev(plot.df$Year)),
        c(plot.df$Index_q_0.05,rev(plot.df$Index_q_0.95)),
        col="lightblue",border=NA)
  lines(plot.df$Year,plot.df$Index,type= "l", pch=19, col="darkblue",  lwd=2)
  }

} # end looping through regions

title(main = species.plot, outer=TRUE, line=-2,col.main = "darkblue",cex.main=1.4)

dev.off() # close png

} # end looping through species

} # end looping through plot types



#------------------------------------------------------------------
# Part 2: Run the Metric Calculations

# check the help file for required inputs
?multiFit

# convert the data set into the required input format
# create  single DU column from Species and Region
data.in <- data.use %>% mutate(DU = paste0(Species, "_", Region)) %>%
              rename(Abd = Index) %>% select(DU,Year, Abd) %>% arrange(DU, Year)


head(data.in)

# specify time window for slope calc
# assuming 4 yr gen for both species, using 12 year windows for now

window.in <- data.frame(DU = sort(unique(data.in$DU)), Window = 12)
window.in

# run the function that calculates alternative metrics for multiple DUs
multi.out <- multiFit(data.df = data.in, window.df = window.in,
                      plot.file = paste0(folder.path,"Hirundinidae_Output/Hirundinidae_SummaryPlots.pdf") )

# check the outputs
head(multi.out$Output)
head(multi.out$Summary)

# write the outputs to a file
write.csv(multi.out$Output, paste0(folder.path,"Hirundinidae_Output/Hirundinidae_Example_Outputs.csv"), row.names = FALSE)
write.csv(multi.out$Summary, paste0(folder.path,"Hirundinidae_Output/Hirundinidae_Example_Summary.csv"), row.names = FALSE)













