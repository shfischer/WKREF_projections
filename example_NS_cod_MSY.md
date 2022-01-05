NS cod example - find MSY
================

## Background

This document illustrates finding MSY for NS cod with a stochastic OM.

## Load R packages

``` r
library(doParallel)
library(tidyr)
library(dplyr)
library(FLCore)
library(FLash)
library(ggplotFL)

source("funs.R")
```

## Load OM(s)

Load stochastic and deterministic OMs

``` r
### FLStock
stk <- readRDS("OM_files/cod4/stk.rds")
stk_det <- readRDS("OM_files/cod4/stk_det.rds")
### recruitment model and residuals
sr <- readRDS("OM_files/cod4/sr.rds")
sr_det <- readRDS("OM_files/cod4/sr_det.rds")
sr_res <- readRDS("OM_files/cod4/sr_res.rds")
sr_res_det <- readRDS("OM_files/cod4/sr_res_det.rds")
```

## Stochastic OM

### Explore search space

First, run some F values:

``` r
### define projection
proj_yrs <- 2018:2037 ### years used in projection
Blim <- 107000 ### from ICES WGNSSK 2018 and used by WKNSMSE 2019
risk_limit <- 0.05 ### 5% risk limit
stat_yrs <- 2028:2037 ### years in which catch and risk are calculated (last 10)
### set up environment for storing results
env_stochastic <- new.env()
### this is needed because "optimise" does not store results
### run a few values
fs <- seq(0, 1, 0.1)
. <- foreach(target = fs) %do% {
  proj_stats(stk = stk, sr = sr, sr_res = sr_res, proj_yrs = proj_yrs, 
             target = target,
             Blim = Blim, risk_limit = risk_limit, stat_yrs = stat_yrs,
             trace = TRUE, trace_env = env_stochastic)
}
```

    ##    target       SSB     Catch      Fbar       Rec      Risk objective 
    ##       0.0  677613.8       0.0       0.0  190620.4       0.0       0.0 
    ##    target       SSB     Catch      Fbar       Rec      Risk objective 
    ##      0.10 423202.83  33623.45      0.10 190620.36      0.00  33623.45 
    ##    target       SSB     Catch      Fbar       Rec      Risk objective 
    ##      0.20 282107.14  46392.82      0.20 190620.36      0.00  46392.82 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.3000 198758.5237  50770.7195      0.3000 190620.3624      0.0011 
    ##   objective 
    ##  50770.7195 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.4000 146691.0200  51600.1784      0.4000 190620.3624      0.0618 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.5000 113177.8602  51028.9070      0.5000 190620.3624      0.3927 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.6000  90641.2420  49922.9513      0.6000 190620.3624      0.7805 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.7000  74743.2287  48607.1378      0.7000 189907.4603      0.9413 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.8000  62546.4184  46757.4246      0.8000 186286.5832      0.9857 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.9000  51365.7566  43024.2242      0.9000 172831.2026      0.9967 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      1.0000  40447.9242  37499.7991      1.0000 148005.6804      0.9991 
    ##   objective 
    ##     -1.0000

``` r
runs_stochastic <- bind_rows(get("res_trace", envir = env_stochastic))
```

We can visualise the results

``` r
runs_stochastic %>%
    pivot_longer(SSB:objective) %>%
    ggplot(aes(x = target, y = value)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line() + geom_point() +
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()
```

![](example_NS_cod_MSY_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Find MSY

The highest catch where Blim risk is \<= 5% is somewhere between F = 0.2
and F = 0.5. We can use `optimise` (a golden section search) to find
MSY:

``` r
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
```

    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.145898e-01 1.895537e+05 5.104213e+04 3.145898e-01 1.906204e+05 2.100000e-03 
    ##    objective 
    ## 5.104213e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.854102e-01 1.528249e+05 5.161075e+04 3.854102e-01 1.906204e+05 4.080000e-02 
    ##    objective 
    ## 5.161075e+04 
    ##        target           SSB         Catch          Fbar           Rec 
    ##  4.291796e-01  1.354117e+05  5.152836e+04  4.291796e-01  1.906204e+05 
    ##          Risk     objective 
    ##  1.275000e-01 -1.000000e+00 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.503875e-01 1.694787e+05 5.148000e+04 3.503875e-01 1.906204e+05 1.220000e-02 
    ##    objective 
    ## 5.148000e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.834545e-01 1.536710e+05 5.160166e+04 3.834545e-01 1.906204e+05 3.890000e-02 
    ##    objective 
    ## 5.160166e+04 
    ##        target           SSB         Catch          Fbar           Rec 
    ##  4.021286e-01  1.457654e+05  5.158932e+04  4.021286e-01  1.906204e+05 
    ##          Risk     objective 
    ##  6.670000e-02 -1.000000e+00 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.917961e-01 1.500663e+05 5.160670e+04 3.917961e-01 1.906204e+05 4.930000e-02 
    ##    objective 
    ## 5.160670e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.881018e-01 1.516463e+05 5.160850e+04 3.881018e-01 1.906204e+05 4.430000e-02 
    ##    objective 
    ## 5.160850e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.864383e-01 1.523804e+05 5.160891e+04 3.864383e-01 1.906204e+05 4.190000e-02 
    ##    objective 
    ## 5.160891e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.846632e-01 1.531470e+05 5.160380e+04 3.846632e-01 1.906204e+05 4.010000e-02 
    ##    objective 
    ## 5.160380e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.858029e-01 1.526548e+05 5.161393e+04 3.858029e-01 1.906204e+05 4.140000e-02 
    ##    objective 
    ## 5.161393e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.858667e-01 1.526292e+05 5.161403e+04 3.858667e-01 1.906204e+05 4.150000e-02 
    ##    objective 
    ## 5.161403e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ##      0.38589 152619.05408  51614.06969      0.38589 190620.36239      0.04150 
    ##    objective 
    ##  51614.06969 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.860994e-01 1.525244e+05 5.161222e+04 3.860994e-01 1.906204e+05 4.170000e-02 
    ##    objective 
    ## 5.161222e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ##      0.38597 152582.91868  51613.92525      0.38597 190620.36239      0.04150 
    ##    objective 
    ##  51613.92525 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.859205e-01 1.526053e+05 5.161412e+04 3.859205e-01 1.906204e+05 4.150000e-02 
    ##    objective 
    ## 5.161412e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.859394e-01 1.525967e+05 5.161415e+04 3.859394e-01 1.906204e+05 4.150000e-02 
    ##    objective 
    ## 5.161415e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.859511e-01 1.525914e+05 5.161417e+04 3.859511e-01 1.906204e+05 4.150000e-02 
    ##    objective 
    ## 5.161417e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.859583e-01 1.525882e+05 5.161408e+04 3.859583e-01 1.906204e+05 4.150000e-02 
    ##    objective 
    ## 5.161408e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.859466e-01 1.525935e+05 5.161416e+04 3.859466e-01 1.906204e+05 4.150000e-02 
    ##    objective 
    ## 5.161416e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.859544e-01 1.525899e+05 5.161413e+04 3.859544e-01 1.906204e+05 4.150000e-02 
    ##    objective 
    ## 5.161413e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.859511e-01 1.525914e+05 5.161417e+04 3.859511e-01 1.906204e+05 4.150000e-02 
    ##    objective 
    ## 5.161417e+04

``` r
### Fmsy is
MSY_stochastic$maximum
```

    ## [1] 0.3859511

``` r
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
```

![](example_NS_cod_MSY_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Do the same for the “deterministic” OM

``` r
env_det <- new.env()
### this is needed because "optimise" does not store results
### run a few values
fs <- seq(0, 1, 0.1)
. <- foreach(target = fs) %do% {
  proj_stats(stk = stk_det, sr = sr_det, sr_res = sr_res_det, 
             proj_yrs = proj_yrs, 
             target = target,
             Blim = Blim, risk_limit = risk_limit, stat_yrs = stat_yrs,
             trace = TRUE, trace_env = env_det)
}
```

    ##    target       SSB     Catch      Fbar       Rec      Risk objective 
    ##       0.0  664792.9       0.0       0.0  188188.5       0.0       0.0 
    ##    target       SSB     Catch      Fbar       Rec      Risk objective 
    ##      0.10 416102.88  32756.44      0.10 188188.49      0.00  32756.44 
    ##    target       SSB     Catch      Fbar       Rec      Risk objective 
    ##      0.20 276712.12  45444.76      0.20 188188.49      0.00  45444.76 
    ##    target       SSB     Catch      Fbar       Rec      Risk objective 
    ##       0.3  194386.2   49799.7       0.3  188188.5       0.0   49799.7 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.4000 143576.0616  50745.4903      0.4000 188188.4902      0.0382 
    ##   objective 
    ##  50745.4903 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.5000 110671.9125  50175.8490      0.5000 188188.4902      0.4285 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.6000  88716.0226  49138.6379      0.6000 188188.4902      0.8315 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.7000  73433.6955  47941.8220      0.7000 188107.0509      0.9681 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.8000  61766.7075  46325.9349      0.8000 185411.3802      0.9951 
    ##   objective 
    ##     -1.0000 
    ##      target         SSB       Catch        Fbar         Rec        Risk 
    ##      0.9000  51295.0354  43015.9551      0.9000 175040.7900      0.9999 
    ##   objective 
    ##     -1.0000 
    ##    target       SSB     Catch      Fbar       Rec      Risk objective 
    ##      1.00  39820.03  36958.90      1.00 147934.59      1.00     -1.00

``` r
runs_det <- bind_rows(get("res_trace", envir = env_det))
runs_det %>%
    pivot_longer(SSB:objective) %>%
    ggplot(aes(x = target, y = value)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line() + geom_point() +
    labs(x = "F target", y = "") +
    geom_blank(data = data.frame(target = 0, value = 0), aes(target, value)) +
    theme_bw()
```

![](example_NS_cod_MSY_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
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
```

    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.763932e-01 1.535301e+05 5.070678e+04 3.763932e-01 1.881885e+05 1.240000e-02 
    ##    objective 
    ## 5.070678e+04 
    ##        target           SSB         Catch          Fbar           Rec 
    ##  4.236068e-01  1.345661e+05  5.071746e+04  4.236068e-01  1.881885e+05 
    ##          Risk     objective 
    ##  9.420000e-02 -1.000000e+00 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.472136e-01 1.675868e+05 5.052227e+04 3.472136e-01 1.881885e+05 1.700000e-03 
    ##    objective 
    ## 5.052227e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.620270e-01 1.602007e+05 5.062382e+04 3.620270e-01 1.881885e+05 5.300000e-03 
    ##    objective 
    ## 5.062382e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.944272e-01 1.458460e+05 5.076068e+04 3.944272e-01 1.881885e+05 2.860000e-02 
    ##    objective 
    ## 5.076068e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 4.027994e-01 1.424649e+05 5.074636e+04 4.027994e-01 1.881885e+05 4.320000e-02 
    ##    objective 
    ## 5.074636e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.938070e-01 1.461013e+05 5.075829e+04 3.938070e-01 1.881885e+05 2.740000e-02 
    ##    objective 
    ## 5.075829e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.972320e-01 1.447054e+05 5.075975e+04 3.972320e-01 1.881885e+05 3.270000e-02 
    ##    objective 
    ## 5.075975e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.954985e-01 1.454045e+05 5.075329e+04 3.954985e-01 1.881885e+05 3.090000e-02 
    ##    objective 
    ## 5.075329e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.948364e-01 1.456784e+05 5.075549e+04 3.948364e-01 1.881885e+05 2.970000e-02 
    ##    objective 
    ## 5.075549e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.941903e-01 1.459466e+05 5.075929e+04 3.941903e-01 1.881885e+05 2.840000e-02 
    ##    objective 
    ## 5.075929e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.945835e-01 1.457782e+05 5.075885e+04 3.945835e-01 1.881885e+05 2.880000e-02 
    ##    objective 
    ## 5.075885e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.943367e-01 1.458856e+05 5.076053e+04 3.943367e-01 1.881885e+05 2.860000e-02 
    ##    objective 
    ## 5.076053e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.944869e-01 1.458190e+05 5.076018e+04 3.944869e-01 1.881885e+05 2.860000e-02 
    ##    objective 
    ## 5.076018e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.943947e-01 1.458605e+05 5.076096e+04 3.943947e-01 1.881885e+05 2.860000e-02 
    ##    objective 
    ## 5.076096e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.943869e-01 1.458639e+05 5.076095e+04 3.943869e-01 1.881885e+05 2.860000e-02 
    ##    objective 
    ## 5.076095e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.943981e-01 1.458590e+05 5.076093e+04 3.943981e-01 1.881885e+05 2.860000e-02 
    ##    objective 
    ## 5.076093e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.943914e-01 1.458619e+05 5.076099e+04 3.943914e-01 1.881885e+05 2.860000e-02 
    ##    objective 
    ## 5.076099e+04 
    ##       target          SSB        Catch         Fbar          Rec         Risk 
    ## 3.943914e-01 1.458619e+05 5.076099e+04 3.943914e-01 1.881885e+05 2.860000e-02 
    ##    objective 
    ## 5.076099e+04

``` r
### Fmsy is
MSY_det$maximum
```

    ## [1] 0.3943914

``` r
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
```

![](example_NS_cod_MSY_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

## Compare stochastic and deterministic OM

``` r
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
```

![](example_NS_cod_MSY_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
