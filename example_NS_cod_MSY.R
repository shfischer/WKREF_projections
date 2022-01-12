#' ---
#' title: "NS cod example - find MSY"
#' output: github_document
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' ## Background
#' 
#' This document illustrates finding MSY for NS cod with a stochastic OM.
#' 
#' ## Load R packages
#' 
## ----results = "hide", message = FALSE, warning = FALSE---------------------------------
library(doParallel)
library(tidyr)
library(dplyr)
library(FLCore)
library(FLash)
library(ggplotFL)

source("funs.R")

#' 
#' ## Load OM(s)
#' 
#' Load stochastic and deterministic OMs
## ----message = FALSE, warning = FALSE---------------------------------------------------
### FLStock
stk <- readRDS("OM_files/cod4/stk.rds")
stk_det <- readRDS("OM_files/cod4/stk_det.rds")
### recruitment model and residuals
sr <- readRDS("OM_files/cod4/sr.rds")
sr_det <- readRDS("OM_files/cod4/sr_det.rds")
sr_res <- readRDS("OM_files/cod4/sr_res.rds")
sr_res_det <- readRDS("OM_files/cod4/sr_res_det.rds")

#' 
#' ## Stochastic OM
#' 
#' ### Explore search space
#' 
#' First, run some F values:
#' 
## ----message = FALSE, warning = FALSE---------------------------------------------------
### define projection
proj_yrs <- 2018:2037 ### years used in projection
Blim <- 107000 ### from ICES WGNSSK 2018 and used by WKNSMSE 2019
risk_limit <- 0.05 ### 5% risk limit
stat_yrs <- 2028:2037 ### years in which catch and risk are calculated (last 10)
### set up environment for storing results
env_stochastic <- new.env()
### this is needed because "optimise" does not store results
### run a few values
fs <- seq(0, 1, 0.01)
. <- foreach(target = fs) %do% {
  proj_stats(stk = stk, sr = sr, sr_res = sr_res, proj_yrs = proj_yrs, 
             target = target,
             Blim = Blim, risk_limit = risk_limit, stat_yrs = stat_yrs,
             trace = TRUE, trace_env = env_stochastic)
}
runs_stochastic <- bind_rows(get("res_trace", envir = env_stochastic))

#' 
#' We can visualise the results
#' 
## ----message = FALSE, warning = FALSE---------------------------------------------------
runs_stochastic %>%
    pivot_longer(SSB:objective) %>%
    ggplot(aes(x = target, y = value)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line() + geom_point() +
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()

#' 
#' ### Find MSY
#' 
#' The highest catch where Blim risk is <= 5% is somewhere between F = 0.2 and F = 0.5. We can use `optimise` (a golden section search) to find MSY:
#' 
## ----message = FALSE, warning = FALSE---------------------------------------------------
### find optimum
MSY_stochastic <- optimise(f = proj_stats, 
                           stk = stk, sr = sr, sr_res = sr_res, 
                           proj_yrs = proj_yrs, Blim = Blim, 
                           risk_limit = risk_limit, stat_yrs = stat_yrs,
                           trace = TRUE, trace_env = env_stochastic,
                           interval = c(0.2, 0.5),
                           lower = 0.2, upper = 0.5,
                           maximum = TRUE,
                           tol = 0.00001)
### Fmsy is
MSY_stochastic$maximum

### extract results
runs_stochastic <- bind_rows(bind_rows(runs_stochastic,
                                       get("res_trace", envir = env_stochastic)))
saveRDS(runs_stochastic, "OM_files/cod4/MSY_runs_stochastic.rds")

### plot
runs_stochastic %>%
    pivot_longer(SSB:objective) %>%
    ggplot(aes(x = target, y = value)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line() +
    geom_vline(xintercept = MSY_stochastic$maximum, linetype = "dashed") + 
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()

#' 
#' 
#' ## Do the same for the "deterministic" OM
#' 
## ----message = FALSE, warning = FALSE---------------------------------------------------
env_det <- new.env()
### this is needed because "optimise" does not store results
### run a few values
fs <- seq(0, 1, 0.01)
. <- foreach(target = fs) %do% {
  proj_stats(stk = stk_det, sr = sr_det, sr_res = sr_res_det, 
             proj_yrs = proj_yrs, 
             target = target,
             Blim = Blim, risk_limit = risk_limit, stat_yrs = stat_yrs,
             trace = TRUE, trace_env = env_det)
}
runs_det <- bind_rows(get("res_trace", envir = env_det))
runs_det %>%
    pivot_longer(SSB:objective) %>%
    ggplot(aes(x = target, y = value)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line() + geom_point() +
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()
### try F = 0.3, ... 0.5
### find optimum
MSY_det <- optimise(f = proj_stats, 
                    stk = stk_det, sr = sr_det, sr_res = sr_res_det, 
                    proj_yrs = proj_yrs, Blim = Blim, 
                    risk_limit = risk_limit, stat_yrs = stat_yrs,
                    trace = TRUE, trace_env = env_det,
                    interval = c(0.3, 0.5),
                    lower = 0.3, upper = 0.5,
                    maximum = TRUE,
                    tol = 0.00001)
### Fmsy is
MSY_det$maximum

### extract results
runs_det <- bind_rows(bind_rows(runs_det,
                                       get("res_trace", envir = env_det)))
saveRDS(runs_det, "OM_files/cod4/MSY_runs_det.rds")

### plot
runs_det %>%
    pivot_longer(SSB:objective) %>%
    ggplot(aes(x = target, y = value)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line() +
    geom_vline(xintercept = MSY_det$maximum, linetype = "dashed") + 
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()

#' 
#' ## Compare stochastic and deterministic OM
## ----message = FALSE, warning = FALSE---------------------------------------------------
bind_rows(runs_det %>% mutate(MSY = "deterministic"),
          runs_stochastic %>% mutate(MSY = "stochastic")) %>%
    pivot_longer(cols = c(SSB, Catch, Rec, Risk, objective)) %>%
    ggplot(aes(x = target, y = value, colour = MSY)) +
    geom_line() +
    facet_wrap(~name, scales = "free_y") +
    geom_vline(data = data.frame(MSY = c("deterministic", "stochastic"),
                                 x = c(MSY_det$maximum, MSY_stochastic$maximum)),
               aes(xintercept = x, colour = MSY), linetype = "dashed") + 
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0, MSY = NA), 
               aes(target, value)) +
    theme_bw()

#' 
#' 
#' ## Fp.05
#' 
#' ### Explore search space
#' 
#' First, run some F values:
#' 
## ----message = FALSE, warning = FALSE---------------------------------------------------
### set up environment for storing results
env_stochastic_Fp.05 <- new.env()
### this is needed because "optimise" does not store results
### run a few values
fs <- seq(0, 1, 0.01)
. <- foreach(target = fs) %do% {
  proj_stats(stk = stk, sr = sr, sr_res = sr_res, proj_yrs = proj_yrs, 
             target = target, objective = "risk",
             Blim = Blim, risk_limit = risk_limit, stat_yrs = stat_yrs,
             trace = TRUE, trace_env = env_stochastic_Fp.05)
}
runs_stochastic_Fp.05 <- bind_rows(get("res_trace", envir = env_stochastic_Fp.05))

#' 
#' We can visualise the results
#' 
## ----message = FALSE, warning = FALSE---------------------------------------------------
runs_stochastic_Fp.05 %>%
    pivot_longer(SSB:objective) %>%
    ggplot(aes(x = target, y = value)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line() + geom_point() +
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()

#' 
#' ### Find Fp.05
#' 
#' New objects and environments
#' 
#' The F where Blim risk is <= 5% is somewhere between .... We can use `optimise` (a golden section search) to find Fp.05:
#' 
#' ### stochastic
#' 
## ----message = FALSE, warning = FALSE---------------------------------------------------
### find optimum
MSY_stochastic_Fp.05 <- optimise(f = proj_stats, 
                           stk = stk, sr = sr, sr_res = sr_res, 
                           proj_yrs = proj_yrs, Blim = Blim, 
                           objective = "risk",
                           risk_limit = risk_limit, stat_yrs = stat_yrs,
                           trace = TRUE, trace_env = env_stochastic_Fp.05,
                           interval = c(0.3, 0.6),
                           lower = 0.3, upper = 0.6,
                           maximum = TRUE,
                           tol = 0.00001)
### Fp.05 is
MSY_stochastic_Fp.05$maximum

### extract results
runs_stochastic_Fp.05 <- bind_rows(bind_rows(runs_stochastic_Fp.05,
                                       get("res_trace", envir = env_stochastic_Fp.05)))
saveRDS(runs_stochastic_Fp.05, "OM_files/cod4/MSY_runs_stochastic_Fp.05.rds")

### plot
runs_stochastic_Fp.05 %>%
    pivot_longer(SSB:objective) %>%
    ggplot(aes(x = target, y = value)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line() +
    geom_vline(xintercept = MSY_stochastic_Fp.05$maximum, linetype = "dashed") + 
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()
runs_stochastic_Fp.05 %>%
    ggplot(aes(x = target, y = Risk)) +
    geom_line() +
    geom_vline(xintercept = MSY_stochastic_Fp.05$maximum, linetype = "dashed") + 
    geom_hline(yintercept = 0.05, linetype = "dashed", colour = "red") +
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()

#' 
#' ### determinstic
#' 
#' Do the same for the deterministic run
## ----message = FALSE, warning = FALSE---------------------------------------------------
### set up environment for storing results
env_det_Fp.05 <- new.env()
### this is needed because "optimise" does not store results
### run a few values
fs <- seq(0, 1, 0.01)
. <- foreach(target = fs) %do% {
  proj_stats(stk = stk_det, sr = sr_det, sr_res = sr_res_det, proj_yrs = proj_yrs, 
             target = target, objective = "risk",
             Blim = Blim, risk_limit = risk_limit, stat_yrs = stat_yrs,
             trace = TRUE, trace_env = env_det_Fp.05)
}
runs_det_Fp.05 <- bind_rows(get("res_trace", envir = env_det_Fp.05))

#' 
## ----message = FALSE, warning = FALSE---------------------------------------------------
### find optimum
MSY_det_Fp.05 <- optimise(f = proj_stats, 
                           stk = stk_det, sr = sr_det, sr_res = sr_res_det, 
                           proj_yrs = proj_yrs, Blim = Blim, 
                           objective = "risk",
                           risk_limit = risk_limit, stat_yrs = stat_yrs,
                           trace = TRUE, trace_env = env_det_Fp.05,
                           interval = c(0.3, 0.6),
                           lower = 0.3, upper = 0.6,
                           maximum = TRUE,
                           tol = 0.00001)
### Fp.05 is
MSY_det_Fp.05$maximum

### extract results
runs_det_Fp.05 <- bind_rows(bind_rows(runs_det_Fp.05,
                                       get("res_trace", envir = env_det_Fp.05)))
saveRDS(runs_det_Fp.05, "OM_files/cod4/MSY_runs_det_Fp.05.rds")

### plot
runs_det_Fp.05 %>%
    pivot_longer(SSB:objective) %>%
    ggplot(aes(x = target, y = value)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line() +
    geom_vline(xintercept = MSY_det_Fp.05$maximum, linetype = "dashed") + 
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()
runs_det_Fp.05 %>%
    ggplot(aes(x = target, y = Risk)) +
    geom_line() +
    geom_vline(xintercept = MSY_det_Fp.05$maximum, linetype = "dashed") + 
    geom_hline(yintercept = 0.05, linetype = "dashed", colour = "red") +
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()

#' ## Compare stochastic and deterministic OM
## ----message = FALSE, warning = FALSE---------------------------------------------------
bind_rows(runs_det_Fp.05 %>% mutate(MSY = "deterministic"),
          runs_stochastic_Fp.05 %>% mutate(MSY = "stochastic")) %>%
    #pivot_longer(cols = c(SSB, Catch, Rec, Risk, objective)) %>%
    ggplot(aes(x = target, y = Risk, colour = MSY)) +
    geom_line() +
    #facet_wrap(~name, scales = "free_y") +
    geom_vline(data = data.frame(MSY = c("deterministic", "stochastic"),
                                 x = c(MSY_det_Fp.05$maximum, 
                                       MSY_stochastic_Fp.05$maximum)),
               aes(xintercept = x, colour = MSY), linetype = "dashed") + 
    geom_hline(yintercept = 0.05, colour = "grey", linetype = "dashed") +
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0, MSY = NA), 
               aes(target, value)) +
    theme_bw()

