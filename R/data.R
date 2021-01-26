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
