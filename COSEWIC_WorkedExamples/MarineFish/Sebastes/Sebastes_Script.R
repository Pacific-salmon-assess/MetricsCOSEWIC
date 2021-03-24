## WORKED EXAMPLE: SEBASTES

# load the packages
library(rjags)
library(rstanarm)
library(rstan)
library(coda)
library(tidyverse)

## to make sure the most recent pkg is being used

install.packages("devtools") # Install the devtools package
library(devtools) # Load the devtools package.
install_github("SOLV-Code/MetricsCOSEWIC",
               dependencies = TRUE,
               build_vignettes = FALSE)
library(MetricsCOSEWIC)


folder.path <- "COSEWIC_WorkedExamples/MarineFish/Sebastes/"



# get a data set
# added "fileEncoding="UTF-8-BOM" due fix the "ï.." on the first column name
# as per https://stackoverflow.com/questions/24568056/rs-read-csv-prepending-1st-column-name-with-junk-text/24568505
data.in <- read.csv(paste0(folder.path,"Sebastes_SampleData.csv"), fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)

# rescale the data
data.in$Abd <- data.in$Abd/1000000
head(data.in)
#View((data.in))

window.in <- read.csv(paste0(folder.path,"Sebastes_TimeWindows.csv"), fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
head(window.in)
#View((window.in))

# run the function that calculates alternative metrics for multiple DUs
multi.out <- multiFit(data.df = data.in, window.df = window.in,
                      plot.file = paste0(folder.path,"Sebastes_Output/Sebastes_SummaryPlots.pdf") )

# check the outputs
head(multi.out$Output)
head(multi.out$Summary)

# write the outputs to a file
write.csv(multi.out$Output, paste0(folder.path,"Sebastes_Output/Sebastes_Example_Outputs.csv"), row.names = FALSE)
write.csv(multi.out$Summary, paste0(folder.path,"Sebastes_Output/Sebastes_Example_Summary.csv"), row.names = FALSE)
