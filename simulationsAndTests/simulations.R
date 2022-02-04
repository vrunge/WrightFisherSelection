


library(parallel)

###############################################################


ALLSIMUS01 <- function(N, alpha, step, NbSimus)
{
  valA <- seq(from = step, to = N - step, by = step)
  if(N%%step != 0){valA <- 1:(N-1)}

  df <- data.frame(matrix(nrow = length(valA), ncol = 4))
  colnames(df) <- c("nbA", "p1", "m0", "m1")

  for (i in 1:length(valA))
  {
    print(i)
    liste <- mclapply(rep(N, NbSimus), fixation_time_simu,
                      alpha = alpha,
                      nbA = valA[i],
                      mc.cores = 8)
    temp <- do.call(cbind, liste)
    time <- unlist(temp[1,])
    type <- unlist(temp[2,])
    p1 <- mean(type)
    m0 <- mean(time[which(type == "0")])
    m1 <- mean(time[which(type == "1")])
    df[i ,] <- c(valA[i], p1, m0, m1)
  }
  return(df)
}


#######
##simu
N <- 10000
alpha <- 50
step <- N/100
res <- ALLSIMUS01(N = N, alpha = alpha, step = step, NbSimus = 100)


##exact
stepfix1 <- 0.01
p <- seq(stepfix1,1-stepfix1,by =stepfix1)
fix1 <-  fixation_time1(N, alpha, p)

##plot
MAX <- max(c(res$m1, fix1))
plot(p, fix1,  xlim = c(0,1), ylim = c(0, MAX),
     type = 'l', col = 2,
     ylab = "fixation time to 1")
par(new = TRUE)
plot(seq(from = step, to = N - step, by = step)/N, res$m1,
     xlim = c(0,1), ylim = c(0, MAX),
     xlab = "", ylab = "")

