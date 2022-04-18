#-----------------------------------------------------------------------------##
#- This file compute fixation times using: -----------------------------------##
#- (1) simulations (see simu_fixations) --------------------------------------##
#- (2) analytic solutions (see fixation_time0, fixation_time1, fixation_time -##
#- (3) approximation with Taylor expansion (see fixation_time_approx) --------##
#- All fixation times are saved in file.path(WD,"data/FILE".rds") ------------##
#- ---------------------------------------------------------------------------##

source("simulations_utils.R")
library(parallel)

#- set working directory and number of logical cores to use during simu ------##
WD <- "/guillem/aliehrmann/Documents/WrightFisherSelection-paper"
NB_LOGICAL_CORES <- 70

##- range of parameters to test ----------------------------------------------##
N                <- c(100,200,300,400,500,600,700,800,900,1000)
p                <- seq(0.05,0.95, by=0.05)
alpha            <- seq(1,30,length=20)

#- zoom on alpha in [1,3] ----------------------------------------------------##
#p      <- seq(0.05,0.95, by=0.05)
#alpha  <- seq(1,3,length=20)
#params <- expand.grid(N, alpha, p)
#N      <- 100
#-----------------------------------------------------------------------------##

params           <- expand.grid(N, alpha, p)
colnames(params) <- c("N", "alpha", "p")
target_nb_rep    <- 1e4

params <- simu_feasability(
  params        = params,
  target_nb_rep = target_nb_rep,
  max_nb_rep    = 1e5
)

params <- estimate_running_time(
  params = params,
  nb_rep = 10,
  nb_logical_cores = NB_LOGICAL_CORES
)
sort(params$running_time)

#- keep only sim that should take less than 6 hours --------------------------##
params <- params[params$running_time/60<6,]

#- fixation time using simu --------------------------------------------------##
res_simu_fixations  <- do.call(rbind, mclapply(
  1:nrow(params),
  function(i) {
    param <- params[i,] 
    simu_fixations(
      N             = param$N, 
      alpha         = param$alpha, 
      p             = param$p, 
      nb_rep        = param$nb_rep,
      target_nb_rep = target_nb_rep, 
      search_fix1   = param$search_fix1, 
      search_fix0   = param$search_fix0
    )
  }, mc.cores = NB_LOGICAL_CORES
))
saveRDS(
  res_simu_fixations, 
  file.path(WD, "/data/res_simu_fixations4.rds")
)

#- fixation time using analytic solutions ------------------------------------##
res_analytic_solutions  <- do.call(rbind, mclapply(
  1:nrow(params),
  function(i) {
    param <- params[i,] 
    data.frame(
      N              = param$N,
      alpha          = param$alpha,
      p              = param$p,
      state          = c("0", "1", "both"),
      fixation_time  = c(
        fixation_time0(param$N, param$alpha, param$p),
        fixation_time1(param$N, param$alpha, param$p),
        fixation_time(param$N, param$alpha, param$p)
      )
    )
  }, mc.cores = NB_LOGICAL_CORES
))
saveRDS(
  res_analytic_solutions, 
  file.path(WD, "/data/res_analytic_solutions4.rds")
)

#- fixation time using approximations ----------------------------------------##

type              <- 1
k                 <- 1:300
N = 100
params2           <- expand.grid(N, alpha, p, k, type)
colnames(params2) <- c("N", "alpha", "p", "k", "type")

res_approximation <- do.call(rbind, mclapply(
  1:nrow(params2),
  function(i) {
    param <- params2[i,]
    data.frame(
      N              = param$N,
      alpha          = param$alpha,
      p              = param$p,
      k              = param$k,
      type           = param$type,
      state          = "both",
      fixation_time_approximation  = fixation_time_approx(
        N     = param$N, 
        alpha = param$alpha , 
        p     = param$p, 
        k     = param$k, 
        type  = param$type
      )
    )
  }, mc.cores = NB_LOGICAL_CORES
))  
saveRDS(
  res_approximation, 
  file.path(WD, "/data/res_approximation4.rds")
)


#- SAVE ----------------------------------------------------------------------##

# ALLSIMUS01 <- function(N, alpha, step, NbSimus)
# {
#   valA <- seq(from = step, to = N - step, by = step)
#   if(N%%step != 0){valA <- 1:(N-1)}

#   df <- data.frame(matrix(nrow = length(valA), ncol = 4))
#   colnames(df) <- c("nbA", "p1", "m0", "m1")

#   for (i in 1:length(valA))
#   {
#     print(i)
#     liste <- mclapply(rep(N, NbSimus), fixation_time_simu,
#                       alpha = alpha,
#                       nbA = valA[i],
#                       mc.cores = 8)
#     temp <- do.call(cbind, liste)
#     time <- unlist(temp[1,])
#     type <- unlist(temp[2,])
#     p1 <- mean(type)
#     m0 <- mean(time[which(type == "0")])
#     m1 <- mean(time[which(type == "1")])
#     df[i ,] <- c(valA[i], p1, m0, m1)
#   }
#   return(df)
# }


# #######
# ##simu
# N <- 100
# alpha <- 70
# step <- N/100
# res <- ALLSIMUS01(N = N, alpha = alpha, step = step, NbSimus = 100)


# ##exact
# stepfix1 <- 0.01
# p <- seq(stepfix1,1-stepfix1,by =stepfix1)
# fix1 <-  fixation_time1(N, alpha, p)

# ##plot
# MAX <- max(c(res$m1, fix1))
# plot(p, fix1,  xlim = c(0,1), ylim = c(0, MAX),
#      type = 'l', col = 2,
#      ylab = "fixation time to 1")
# par(new = TRUE)
# plot(seq(from = step, to = N - step, by = step)/N, res$m1,
#      xlim = c(0,1), ylim = c(0, MAX),
#      xlab = "", ylab = "")
