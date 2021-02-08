#' Sample Spawner Recruit Data
#'
#' A dataset containing spawners, recruits, spawner estimate expansions, and age comp by brood year for mutiple stocks.
#'
#' @format A data frame with  5 required and some optional variables:
#' \describe{
#'   \item{Stock}{ stock name}
#'   \item{Year}{brood year}
#'   \item{Spn}{spawner estimate}
#'   \item{Rec}{recruitment estimate}
#'   \item{logRpS}{calculated value of log(Rec/Spn), usually with mutate(logRpS = log(Rec/Spn))}
#'   \item{SpnExp}{OPTIONAL: expansion factor applied to get the spawner estimate}
#'   \item{RecAgeNumber}{proportion each age class contributes to the total recruits (e.g. RecAge3, RecAge4, RecAge5}
#'   \item{}{}
#'   ...
#' }
#' @source Dummy Data
"SR_Sample"


#' Sample Abundance Data
#'
#' A dataset containing abundance data (may be total abundance, or a relative index) for various Sockeye, Coho, and Chinook stocks.
#'
#' @format A data frame with  5 required and some optional variables:
#' \describe{
#'   \item{Species}{ Species}
#'   \item{Year}{ estimate year}
#'   \item{DU}{ DU Name}
#'   \item{Abd}{abundance estimate}
#'   \item{}{}
#'   ...
#' }
#' @source Dummy Data
"DU_SampleData"



#' Sample Age Data
#'
#' A dataset containing dominant age classes for the DUs in DU_SampleData
#'
#' @format A data frame with 
#' \describe{
#'   \item{DU}{ DU Name}
#'   \item{Avg_Gen}{Average generation time (i.e. dominant age class}
#'   \item{}{}
#'   ...
#' }
#' @source Dummy Data
"DU_SampleAges"