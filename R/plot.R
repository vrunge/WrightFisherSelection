## Copyright (c) 2022 Vincent Runge

#' Fixation time theoretic curve for different selection coefficients
#'
#' @description This function plots the fixation time curves for a given probability vector and different selection coefficients
#' @param N Number of alleles in the considered population (population size)
#' @param alphas Vector of selection coefficients
#' @param p Vector or initial probabilities (values between 0 and 1)
#' @param type Fixation type: both (fixation to zero and one) or one
#' @return Plot of the time to fixation curve
plotFixationTimeSelection <- function(N = 100,
                                      alphas = c(0, 3, 7, 15, 30),
                                      p = seq(0.01, 1-0.01, by = 0.01),
                                      type = "both")
{
  fix <- vector(mode = "list", length = length(alphas))
  if(type == "both")
  {
    for(i in 1:length(alphas))
    {
      if(alphas[i] != 0){fix[[i]] <- c(0, fixation_time(N, alphas[i], p), 0)}
      else{fix[[i]] <- c(0, -2*N*(p*log(p)+(1-p)*log(1-p)), 0)}
    }
    p <- c(0, p, 1)
  }
  if(type == "one")
  {
    for(i in 1:length(alphas))
    {
      if(alphas[i] == 0){alphas[i] <- 0.001} ## replace 0 by 0.001.
      fix[[i]] <- c(fixation_time1(N, alphas[i], p), 1)
      if(alphas[i] == 0.001){alphas[i] <- 0}
    }
    p <- c(p, 1)
  }

  df <- data.frame(proba = p)
  for(i in 1:length(alphas)){df <- cbind(df, fix[[i]])}
  colnames(df) <- c("proba",alphas)
  df2 <- melt(df, id.vars = "proba")
  colnames(df2) <- c("proba", "alpha", "value")

  pr <- ggplot2::ggplot(data = df2,
    ggplot2::aes(x = df2$proba, y = df2$value, color = df2$alpha)) +
    ggplot2::geom_path(size = 1.4) +
    ggplot2::scale_color_discrete("alpha") +
    ggplot2::xlab("initial proportion of allele type A") +
    ggplot2::ylab("time to fixation") +
    ggplot2::theme(axis.text = ggplot2::element_text(size=10),
                   axis.title = ggplot2::element_text(size=20),
                   legend.title = ggplot2::element_text(size=15),
                   legend.text = ggplot2::element_text(size=15))

  if(type == "both"){pr <- pr + ggplot2::ylab("time to fixation")}
  if(type == "one"){pr <- pr + ggplot2::ylab("time to fixation to one")}
  suppressWarnings(print(pr))
}




