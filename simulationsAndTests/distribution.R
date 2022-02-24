
#######################################################################################
### fixation time distribution
N   <- 100
nbA <- 5
alpha   <- 50
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)


#### 2 histograms
library(ggplot2)
dat <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

ggplot(dat,aes(x=time)) +
  geom_histogram(data=subset(dat,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat,type == "1"),fill = "blue", alpha = 0.5, bins = 40)

#######################################################################################
#######################################################################################
# graphe avec différents histogrammmes (différents alpha, différents nbA)
#######################################################################################
#######################################################################################


library(ggplot2)

### fixation time distribution
N   <- 1000

alpha <- 10

nbA <- 100
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat11 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g11 <- ggplot(dat11,aes(x=time)) +
  geom_histogram(data=subset(dat11,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat11,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title="p = 0.1", y = expression(alpha ~ "= 10"))+
  theme(plot.title = element_text(hjust = 0.5))

nbA <- 300
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat12 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g12 <- ggplot(dat12,aes(x=time)) +
  geom_histogram(data=subset(dat12,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat12,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = "")+
  labs(title="p = 0.3")+
  theme(plot.title = element_text(hjust = 0.5))

nbA <- 500
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat13 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g13 <- ggplot(dat13,aes(x=time)) +
  geom_histogram(data=subset(dat13,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat13,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = "")+
  labs(title="p = 0.5")+
  theme(plot.title = element_text(hjust = 0.5))

nbA <- 700
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat14 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g14 <- ggplot(dat14,aes(x=time)) +
  geom_histogram(data=subset(dat14,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat14,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = "")+
  labs(title="p = 0.7")+
  theme(plot.title = element_text(hjust = 0.5))
#####

alpha   <- 5

nbA <- 100
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat21 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g21 <- ggplot(dat21,aes(x=time)) +
  geom_histogram(data=subset(dat21,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat21,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = expression(alpha ~ "= 5"))


nbA <- 300
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat22 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g22 <- ggplot(dat22,aes(x=time)) +
  geom_histogram(data=subset(dat22,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat22,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = "")

nbA <- 500
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat23 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g23 <- ggplot(dat23,aes(x=time)) +
  geom_histogram(data=subset(dat23,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat23,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = "")

nbA <- 700
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat24 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g24 <- ggplot(dat24,aes(x=time)) +
  geom_histogram(data=subset(dat24,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat24,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = "")

#####

alpha   <- 1

nbA <- 100
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat31 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g31 <- ggplot(dat31,aes(x=time)) +
  geom_histogram(data=subset(dat31,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat31,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = expression(alpha ~ "= 1"))

nbA <- 300
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat32 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g32 <- ggplot(dat32,aes(x=time)) +
  geom_histogram(data=subset(dat32,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat32,type == "1"),fill = "blue", alpha = 0.5, bins = 40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = "")

nbA <- 500
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat33 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g33 <- ggplot(dat33,aes(x=time)) +
  geom_histogram(data=subset(dat33,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat33,type == "1"),fill = "blue", alpha = 0.5, bins = 40) +
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = "")

nbA <- 700
res <- replicate(10000, fixation_time_simu(N, alpha, nbA))
hist(unlist(res[1,]), breaks = 50)
dat34 <- data.frame(time = unlist(res[1,]), type = unlist(res[2,]))

g34 <- ggplot(dat34,aes(x=time)) +
  geom_histogram(data=subset(dat34,type == "0"),fill = "red", alpha = 0.5, bins = 40) +
  geom_histogram(data=subset(dat34,type == "1"),fill = "blue", alpha = 0.5, bins = 40) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y = "")





library("cowplot")
plot_grid(g11, g12, g13, g14, g21, g22, g23, g24, g31, g32, g33, g34,
          ncol = 4, nrow = 3)



