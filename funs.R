### ------------------------------------------------------------------------ ###
### run projection and get stats ####
### ------------------------------------------------------------------------ ###

proj_stats <- function(stk, sr, sr_res, proj_yrs, target,
                       Blim, risk_limit = 0.05, stat_yrs,
                       trace = FALSE, trace_env) {
  ### check if results exist already
  if (isTRUE(trace)) {
    res_trace_i <- mget("res_trace", envir = trace_env, 
                        ifnotfound = FALSE)$res_trace
    if (isFALSE(res_trace_i)) res_trace_i <- list()
    if (isTRUE(target %in% sapply(res_trace_i, function(x) x$target))) {
      res_list <- res_trace_i[[which(target == sapply(res_trace_i, 
                                                      function(x) x$target))[[1]]]]
      run <- FALSE
    } else {
      run <- TRUE
    }
  } else {
    run <- TRUE
  }
  if (isTRUE(run)) {
    ### set target
    ctrl <- fwdControl(data.frame(year = proj_yrs, 
                                  quantity = "f", 
                                  val = target))
    ### project forward with stochastic OM
    stk_fwd <- fwd(stk, ctrl = ctrl, sr = sr, sr.residuals = sr_res,
                   sr.residuals.mult = TRUE, maxF = 5)
    ### calculate summary statistics
    res_list <- list(
      target = target,
      SSB = median(ssb(stk_fwd)[, ac(stat_yrs)], na.rm = TRUE),
      Catch = median(catch(stk_fwd)[, ac(stat_yrs)], na.rm = TRUE),
      Fbar = median(fbar(stk_fwd)[, ac(stat_yrs)], na.rm = TRUE),
      Rec = median(rec(stk_fwd)[, ac(stat_yrs)], na.rm = TRUE),
      Risk = mean(c(ssb(stk_fwd)[, ac(stat_yrs)]) < Blim, na.rm = TRUE),
      objective = NA)
    ### objective function
    ### maximise catch but restrict risk
    res_list$objective <- ifelse(res_list$Risk <= risk_limit, 
                                 res_list$Catch, -1)
    ### store results
    if (isTRUE(trace)) {
      res_add <- res_list
      res_trace_i <- append(res_trace_i, list(res_add))
      res_trace_i <- unique(res_trace_i)
      assign(value = res_trace_i, x = "res_trace", envir = trace_env)
    }
  }
  print(c(unlist(res_list)))
  ### return objective value for optimisation
  return(res_list$objective)
}

