
## Copyright (c) 2022 Vincent Runge

#' Fixation time theoretic curve for different selection coefficients
#'
#' @description This function plots the fixation time curves for a given probability vector and different selection coefficients
#' @param N Number of alleles in the considered population (population size)
#' @param alphas Vector of selection coefficients
#' @param p Vector or initial probabilities (values between 0 and 1)
#' @return the time to fixation
plotFixationTimeSelection <- function(N = 100, alphas = c(0,3,7,15,30), p = seq(0.01,1-0.01,by =0.01))
{
  fix <- vector(mode = "list", length = length(alphas))
  for(i in 1:length(alphas))
  {
    if(alphas[i] != 0){fix[[i]] <- c(0, fixation_time(N, alphas[i], p), 0)}
    else{fix[[i]] <- c(0, -2*N*(p*log(p)+(1-p)*log(1-p)), 0)}
  }
  p <- c(0, p, 1)

  df <- data.frame(proba = p)
  for(i in 1:length(alphas))
  {    df <- cbind(df, fix[[i]])  }
  colnames(df) <- c("proba",alphas)

  df2 <- melt(df, id.vars = "proba")
  colnames(df2) <- c("proba", "alpha", "value")


  ggplot(data = df2, aes(x = proba, y = value, color = alpha)) +
    geom_path(size = 1.4) +
    xlab("initial proportion of allele type A") +
    ylab("time to fixation") +
    theme(axis.text = element_text(size=10),
          axis.title = element_text(size=20),
          legend.title = element_text(size=15),
          legend.text = element_text(size=15))
}





