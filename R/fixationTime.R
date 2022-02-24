## Copyright (c) 2022 Vincent Runge


#' Fixation Time Simulation in Wright-Fisher Model with selection
#'
#' @description Simulation of the Wright-Fisher problem with selection coefficient and given initial population in alleles a and A
#' @param N Total number of alleles in the considered population (population size)
#' @param alpha Selection coefficient
#' @param nbA number of allele A
#' @return a list containing the time to fixation and its type (fixation to 0 or 1)
fixation_time_simu <- function(N, alpha, nbA = floor(N/2))
{
  s <- alpha / (2*N)
  p <- nbA / N
  vectA <- nbA

  while((nbA > 0) & (nbA < N))
  {
    nbA <- rbinom(n = 1, size = N, prob = p)
    p <- nbA*(1 + s)/(N + s*nbA)
    vectA <- c(vectA, nbA)
  }
  return(list(time = length(vectA) - 1, type = p))
}




#' Fixation Time in Wright-Fisher Model with selection
#'
#' @description This function computes the time to fixation for a given initial probability and fixed selection coefficient alpha
#' @param N Number of alleles in the considered population (population size)
#' @param alpha Selection coefficient
#' @param p Vector or initial probabilities (values between 0 and 1)
#' @return the time to fixation
fixation_time <- function(N, alpha, p)
{
  gamma <- -digamma(1)
  omega <- function(alpha, p)
  {
    res1 <- (1- exp(-alpha*p))/(1- exp(-alpha))*(-exp(-alpha) * expint_Ei(alpha) + log(abs(alpha)) + gamma)
    res2 <- - exp(-alpha*p) * expint_Ei(alpha*p) + log(abs(alpha*p)) + gamma
    return((res1 - res2)/alpha)
  }
  return(2 * N * (omega(alpha, p) + omega(-alpha, 1 - p)))
}




#' Fixation Time to 1 in Wright-Fisher Model with selection
#'
#' @description This function computes the expected time to fixation conditional to a fixation to 1 for a given initial probability and fixed selection coefficient alpha
#' @param N Number of alleles in the considered population (population size)
#' @param alpha Selection coefficient
#' @param p Vector or initial probabilities (values between 0 and 1)
#' @return the time to fixation to 1
fixation_time1 <- function(N, alpha, p)
{
  gamma <- -digamma(1)

  coeff <- -2*N/(alpha*(1-exp(-alpha)))
  C <- expint_Ei(-alpha)*(1+exp(alpha))/(1-exp(-alpha)) + expint_Ei(alpha)*(2*exp(-alpha))/(1-exp(-alpha)) - (3+exp(-alpha))/(1-exp(-alpha))*(log(abs(alpha)) + gamma)
  D <- expint_Ei(-alpha)*2/(1-exp(-alpha)) + expint_Ei(alpha)*(exp(-alpha)+exp(-2*alpha))/(1-exp(-alpha))  + (log(abs(alpha)) + gamma)*(-1-3*exp(-alpha))/(1-exp(-alpha))
  res2 <-  exp(alpha*(1-p))*expint_Ei(alpha*(p-1)) - log(abs(alpha*(1-p))) - exp(-alpha*p)*expint_Ei(alpha*p) + log(abs(alpha*p))
  res3 <-  exp(-alpha)*expint_Ei(alpha*(1-p)) - exp(-alpha*p)*log(abs(alpha*(1-p))) - expint_Ei(-alpha*p) + exp(-alpha*p)*log(abs(alpha*p))

  u <- (1-exp(-alpha*p))/(1-exp(-alpha))
  return(coeff * (-C*exp(-alpha*p) + D + res2 + res3)/u)
}



#' Fixation Time to 0 in Wright-Fisher Model with selection
#'
#' @description This function computes the expected time to fixation conditional to a fixation to 0 for a given initial probability and fixed selection coefficient alpha
#' @param N Number of alleles in the considered population (population size)
#' @param alpha Selection coefficient
#' @param p Vector or initial probabilities (values between 0 and 1)
#' @return the time to fixation to 0
fixation_time0 <- function(N, alpha, p)
{
  gamma <- -digamma(1)

  coeff <- -2*N/(alpha*(1-exp(alpha)))
  C <- expint_Ei(-alpha)*(1+exp(alpha))/(1-exp(-alpha)) + expint_Ei(alpha)*(2*exp(-alpha))/(1-exp(-alpha)) - (3+exp(-alpha))/(1-exp(-alpha))*(log(abs(alpha)) + gamma)
  D <- expint_Ei(-alpha)*2/(1-exp(-alpha)) + expint_Ei(alpha)*(exp(-alpha)+exp(-2*alpha))/(1-exp(-alpha))  + (log(abs(alpha)) + gamma)*(-1-3*exp(-alpha))/(1-exp(-alpha))
  res2 <-  exp(alpha*(1-p))*expint_Ei(alpha*(p-1)) - log(abs(alpha*(1-p))) - exp(-alpha*p)*expint_Ei(alpha*p) + log(abs(alpha*p))
  res3 <-  exp(-alpha)*expint_Ei(alpha*(1-p)) - exp(-alpha*p)*log(abs(alpha*(1-p))) - expint_Ei(-alpha*p) + exp(-alpha*p)*log(abs(alpha*p))

  u <- 1 - (1-exp(-alpha*p))/(1-exp(-alpha))
  return(coeff * (-D*exp(alpha*(1-p)) + C + res2 + exp(alpha)*res3)/u)
}




################################################################################


#' Fixation Time in Wright-Fisher Model with selection by Taylor expansion
#'
#' @description This function computes the time to fixation for a given initial probability and fixed selection coefficient alpha using a Taylor expansion
#' @param N Number of alleles in the considered population (population size)
#' @param alpha Selection coefficient
#' @param p Vector or initial probabilities (values between 0 and 1)
#' @param k number of elements in the Taylor expansion
#' @param type Type of approximation to use (1 or 2)
#' @return the time to fixation using an approximation by Taylor expansion
fixation_time_approx <- function(N, alpha, p, k = 100, type = "2")
{
  ###intern function omega_approx1
  omega_approx1 <- function(alpha, p, k = k)
  {
    int_log_u_e_u <- function(x, k)  ## integral of log(u) exp u from 0 to x
    {
      res <- 0
      for(i in (k+1):1){res <- (res + log(abs(x)) -  1/i)*(x/i)}
      return(res)
    }


    res1 <- (1- exp(-alpha*p))/(1- exp(-alpha)) * exp(-alpha)* int_log_u_e_u(alpha, k)
    res2 <- (exp(-alpha*p)) * sapply(alpha*p, function(x) int_log_u_e_u(x, k))
    return((res1 - res2)/alpha)
  }

  ###intern function omega_approx2
  omega_approx2 <- function(alpha, p, k = k)
  {
    temp <- 0
    for(i in k:1){temp <- (temp - log(abs(alpha)) + sum(1/(1:i)))*(-alpha)/i}
    res1 <- (1- exp(-alpha*p))/(1- exp(-alpha)) * temp

    temp <- 0
    for(i in k:1){temp <- (temp - log(abs(alpha*p)) + sum(1/(1:i)))*(-alpha*p)/i}
    res2 <- temp
    return((res1 - res2)/alpha)
  }

  if(type == "1"){res <- 2 * N * (omega_approx1(alpha, p, k) + omega_approx1(-alpha, 1-p, k))}
  else{res <- 2 * N * (omega_approx2(alpha, p, k) + omega_approx2(-alpha, 1-p, k))}
  return(res)
}




#' Value of the upper bound majoration
#'
#' @description Upper bound majoring the L1 norm between exact and approximate result for fixation time
#' @param alpha Selection coefficient
#' @param k number of elements in the Taylor expansion
#' @return upper bound value
upper_bound_value <- function(alpha, k)
{
  res <- (exp(alpha)*alpha^k/factorial(k)) * ((2*log(alpha) + exp(-1))/(k+1) + 3)
  return(res)
}



