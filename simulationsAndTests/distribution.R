
#######################################################################################
### fixation time distribution
N   <- 100
nbA <- 5
alpha   <- 30
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)


#### 2 histograms
dat <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

ggplot(dat,aes(x=time)) +
  geom_histogram(data=subset(dat,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat,type == "1"),fill = "blue", alpha = 0.5, bins = 40)




#
# TO DO :
# un graphe avec différents histogrammmes (différents alpha, différents nbA)
#
#
