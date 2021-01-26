
require(tidyverse)
require(rstanarm)
require(shinystan)


# SR_Sample is a built in data set
# use '?SR_Sample' for more information
names(SR_Sample)
test.df <- SR_Sample %>% dplyr::filter(Stock == "Stock1") %>% select(Year,Spn)
test.df
plot()
