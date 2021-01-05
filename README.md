# MetricsCOSEWIC
R Package for calculating COSEWIC metrics. Initial focus is on Probability of Decline.

**WARNING: This is a pre-release development version. Repo is public for testing package install in another repo. DO NOT USE YET**


## Background

* initial version is a spin-off from a metrics package developed for status assessments under Canada's Wild Salmon policy ([WSPMetrics](https://github.com/Pacific-salmon-assess/WSP-Metrics-Pkg)).
* modifications include:
   * modify function arguments, input options, and outputs to customize for COSEWIC requirements
   * redesign the estimation step for probability of decline (increase robustness, explore transition from JAGS to STAN)
* This is a stand-alone package, and is expected to diverge from the *WSPMEtrics* package as functionality is expanded with contributions from various COSEWIC expert processess.


## Package Structure

