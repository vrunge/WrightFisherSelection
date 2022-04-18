#-----------------------------------------------------------------------------##
#- This file contains utility functions to : ---------------------------------##
#- (1) simulate fixation time (see simu_fixations) ---------------------------##
#- (2) check the simulations feasability (see simu_feasability) --------------##
#- (3) estimate running time for each simulation (see estimate_running_time) -##
#-----------------------------------------------------------------------------##

library(WrightFisherSelection)

#- simu_feasability ----------------------------------------------------------##
# params        : grid of parameters
# target_nb_rep : expected number of observed fixations
# max_nb_rep    : maximum computable 
simu_feasability <- function(
  params, 
  target_nb_rep, 
  max_nb_rep) {
  
  params$nb_rep_fix1 <- floor(
    target_nb_rep / ((1-exp(-params$alpha*params$p))/(1-exp(-params$alpha)))
  ) # needed for fixation to 1 
  params$nb_rep_fix0 <- floor(
    target_nb_rep / ((1-exp(params$alpha*(1-params$p)))/(1-exp(params$alpha)))
  ) # needed for fixation to 0
  #- feasible ? --------------------------------------------------------------##
  params$search_fix1 <- ifelse(params$nb_rep_fix1<max_nb_rep, TRUE, FALSE)
  params$search_fix0 <- ifelse(params$nb_rep_fix0<max_nb_rep, TRUE, FALSE)
  #- both not feasible => remove / NA ----------------------------------------##
  params             <- params[params$search_fix1+params$search_fix0>0,] 
  #- keep upper bound number of reps -----------------------------------------##
  params$nb_rep <- sapply(
    1:nrow(params),
    function(i){
      param <- params[i,]
      max(
        param$nb_rep_fix0[param$search_fix0], 
        param$nb_rep_fix1[param$search_fix1]
      ) 
    }
  )
  return(params) 
}


#- estimate_running_time -----------------------------------------------------##
# params           : grid of parameters
# nb_rep           : number of reptitions to estimate the running time 
# nb_logical_cores : number of logical cores used by mclapply
estimate_running_time <- function(
  params,
  nb_rep = 10,
  nb_logical_cores = 1) {
  
  params$running_time <- do.call(rbind, mclapply(
    1:nrow(params),
    function(i){
      param <- params[i,]
      start <- Sys.time()
      replicate(
        n    = nb_rep,
        expr = fixation_time_simu(param$N, param$alpha, param$p*param$N)
      )
      end <- Sys.time()
      as.numeric(difftime(
        time1 = end, 
        time2 = start, 
        units = "mins"
      )) / nb_rep # average 
    }, mc.cores = nb_logical_cores
  ))
  params$running_time <- params$running_time * params$nb_rep # scaled 
  params
}


#- simu_fixations ------------------------------------------------------------##
# N           : population size
# alpha       : 2Ns
# p           : initial probability
# nb_rep      : number of obserbed fixations
# search_fix1 : estimate fixation time to 1 (boolean) 
# search_fix0 : estimate fixation time to 0 (boolean)
simu_fixations <- function(
  N, 
  alpha, 
  p, 
  nb_rep,
  target_nb_rep, 
  search_fix1, 
  search_fix0) {
  
  res_simu     <- replicate(
    n    = nb_rep,
    expr = fixation_time_simu(N, alpha, p*N)
  )
  time_fix0     <- NA
  time_fix1     <- NA
  var_time_fix0 <- NA
  var_time_fix1 <- NA
  all_time_fix  <- unlist(res_simu[1,])
  all_type_fix  <- unlist(res_simu[2,])
  #- 2 stands for fixation to 1 ----------------------------------------------##
  #- 1 stands for fixation to 0 ----------------------------------------------##
  
  #- mean and variance of fixation time to 1 ---------------------------------##
  if (search_fix1){
    #- all_type_fix == 1 returns FALSE ---------------------------------------##
    #- answer & solution : ---------------------------------------------------##
    #- https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
    all_time_fix1    <- all_time_fix[sapply(
      all_type_fix, 
      function(x) isTRUE(all.equal(x, 1)))
    ]
    sample_time_fix1 <- sample(
      x     = all_time_fix1, 
      size  = min(length(all_time_fix1), target_nb_rep)
    )
    time_fix1        <- mean(sample_time_fix1)
    var_time_fix1    <- var(sample_time_fix1)
  }
  
  #- mean and variance of fixation time to 0 ---------------------------------##
  if (search_fix0){
    all_time_fix0     <- all_time_fix[sapply(
      all_type_fix, 
      function(x) isTRUE(all.equal(x, 0)))
    ]
    sample_time_fix0  <- sample(
      x     = all_time_fix0, 
      size  = min(length(all_time_fix0), target_nb_rep)
    )
    time_fix0         <- mean(sample_time_fix0)
    var_time_fix0     <- var(sample_time_fix0)
  }
  
  #- mean and variance of fixation time to both ------------------------------##
  sample_time_fix <- sample(
    x     = all_time_fix,
    size  = target_nb_rep
  )
  time_fix        <- mean(sample_time_fix)
  var_time_fix    <- var(sample_time_fix)
  
  data.frame(
    N                      = N,
    alpha                  = alpha,
    p                      = p,
    state                  = c("0", "1", "both"),
    fixation_time_simu     = c(time_fix0, time_fix1, time_fix),
    var_fixation_time_simu = c(var_time_fix0, var_time_fix1, var_time_fix)
  )
}
