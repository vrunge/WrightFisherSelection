

#######################################################################################
### fixation time expectation
step <- 0.01
p <- seq(step,1-step,by =step)
plotFixationTimeSelection(p = p)



#######################################################################################
### fixation time curve with decomposition fixation to 0 or 1

alpha <- 30
N <- 100
step <- 0.001
p <- seq(step,1-step,by =step)
u <- (1-exp(-alpha*p))/(1-exp(-alpha))

fix0 <-  fixation_time0(N, alpha, p)
fix1 <-  fixation_time1(N, alpha, p)
fix <- fixation_time(N, alpha, p)

y <- u*fix1+(1-u)*fix0
MAX <- max(c(y, fix))

plot(y, type = 'l', ylim = c(0,MAX))
par(new = TRUE)
plot(fix, col = 2, type = 'l', ylim = c(0,MAX))



###############################################################
#########COMPARING 2 methods : direct + resulution m0 and m1
###############################################################
alpha <- 10
N <- 10000
step <- 0.001
p <- seq(step,1-step,by =step)
u <- (1-exp(-alpha*p))/(1-exp(-alpha))

fix0 <-  fixation_time0(N, alpha, p)
fix1 <-  fixation_time1(N, alpha, p)
fix <- fixation_time(N, alpha, p)


###
MAX <- max(c(fix0, fix1))
plot(fix0, type = 'l', ylim = c(0,MAX))
par(new = TRUE)
plot(fix1, type = 'l', ylim = c(0,MAX))
par(new = TRUE)
plot(rev(fix0), type = 'l', ylim = c(0,MAX), col = 2)
par(new = TRUE)
plot(rev(fix1), type = 'l', ylim = c(0,MAX), col = 3)

###
plot((1-u)*fix0, type = 'l', ylim = c(0,MAX), col = 2)
par(new = TRUE)
plot(u*rev(fix0), type = 'l', ylim = c(0,MAX), col = 3)


#change alpha to -alpha, p to 1-p => u to 1-u
#change p to 1-p => fix0 to fix1, fix1 to fix0

###
y <- u*fix1+(1-u)*fix0
MAX <- max(c(y, fix))
plot(y, type = 'l', ylim = c(0,MAX))
par(new = TRUE)
plot(fix, col = 2, type = 'l', ylim = c(0,MAX))
par(new = TRUE)
plot(u*fix1+(1-u)*rev(fix1), col = 3, type = 'l', ylim = c(0,MAX))



rev(fix0)-fix1


### plot proba de fixation
alpha <- 10
u <-  1 - (1-exp(-alpha*p))/(1-exp(-alpha))
plot(u, type = 'l')
par(new = TRUE)
plot(1-u, col = 2, type = 'l')




#######################################################################################
### découpage fixation time for 0 and 1 and recomposition (u, ft1, ft0 et ft = 4 plots)





#######################################################################################
### matrice de plot : différents alpha, différents N, ft VS simulation results








