#-----------------------------------------------------------------------------##
#- This file plots: ----------------------------------------------------------##
#- (1) the errors between simu and the analytic solutions for a range of -----##
#-     paramters (N,p,alpha,state) -------------------------------------------##
#- (2) the distribution of errors --------------------------------------------##
#- (3) the coefficient of variation for the fixation time --------------------##
#- (4) the smallest k (approximate solution) such that the errors between ----##
#-     approximate solution and analytic solution is lower than 0.01 ---------##
#-----------------------------------------------------------------------------##

library(ggplot2)
#- set working directory -----------------------------------------------------##
WD <- "/guillem/aliehrmann/Documents/WrightFisherSelection-paper"
#- load fixation times -------------------------------------------------------##
res_analytic_solutions <- readRDS(file.path(WD, "/data/res_analytic_solutions4.rds"))
res_approximation      <- readRDS(file.path(WD, "/data/res_approximation4.rds"))
res_simu_fixations     <- readRDS(file.path(WD, "/data/res_simu_fixations4.rds"))



#- (1) -----------------------------------------------------------------------##
#- simu VS analytic solutions ------------------------------------------------##
res <- merge(
  res_analytic_solutions,
  res_simu_fixations,
  c("N","alpha","p","state")
)
#- compute relative distances (errors)  --------------------------------------##
res$dist <- abs(res$fixation_time_simu - res$fixation_time) / res$fixation_time_simu
#- create create error classes  ----------------------------------------------##
res$col <- sapply(
  res$dist, 
  function(x){
    ifelse(
      is.na(x), 
      NA, 
      ifelse(
        x> 0.05, 
        "(5%;+Inf)", 
        ifelse(
          x> 0.04, 
          "(4%;5%]", 
          ifelse(
            x > 0.03, 
            "(3%;4%]", 
            ifelse(
              x > 0.02, 
              "(2%;3%]", 
              ifelse(
                x > 0.01, 
                "(1%;2%]", 
                "[0;1%]"
              )
            )
          )
        )
      )
    )
  }
)
res$col <- factor(
 x = res$col, 
 levels = c(
  "[0;1%]", 
  "(1%;2%]", 
  "(2%;3%]", 
  "(3%;4%]", 
  "(4%;5%]", 
  "(5%;+Inf)"
 )
)
#- improve legends -----------------------------------------------------------##
res$state <- paste0("fixation to ",res$state)
res$N     <- paste0("N = ", res$N)
res$N     <- factor(
  x      = res$N, 
  levels = paste0("N = ", c(1:10*100))
)
#- plot errors ---------------------------------------------------------------##
g_dist <- ggplot(
  res[res$N %in% paste0("N = ",c(100,200,300,1000)),],
  aes(
    x    = p, 
    y    = alpha, 
    fill = col
  )
)+
scale_fill_brewer(
  "relative distance", 
  palette = "YlOrRd"
)+
facet_grid(N~state)+
geom_tile(
  color = "white"
)+
labs(
  y = expression(alpha),
  x = expression(p)  
)+
theme_bw()+
theme(
  text             = element_text(size = 18),
  strip.background = element_rect(fill="grey95"),
  panel.spacing    = unit(1.5, "lines")
)

pdf(
  file   = file.path(
    WD,
    "/figures/relative_distance_sim_vs_analytic_solutions.pdf"
  ),
  width  = 13,
  height = 13,
)
print(g_dist)
dev.off()



#- (2) -----------------------------------------------------------------------##
#- compute median of errors  -------------------------------------------------##
params <- unique(res[,c("N","state")]) 
median_of_errors <- do.call(rbind, lapply(
  1:nrow(params),
  function(x) {
    data.frame(
      median = median(res[
        res$N==params$N[[x]] & 
        res$state==params$state[[x]],
      ]$dist, na.rm=TRUE),
      N      = params$N[[x]],
      state  = params$state[[x]]
    )
  }
))
median_of_errors$threshold <- factor(
  x = ifelse(
    median_of_errors$median < 0.01,
    "median <= 0.01",
    "median > 0.01"
  ), 
  levels = c("median > 0.01", "median <= 0.01")
)
#- plot distribution of errors -----------------------------------------------##
g_dist_distrib <- ggplot(
  res[res$N %in% paste0("N = ",c(100,200,300,1000)),],
  aes(
    x = dist,
  )
)+
scale_x_continuous("relative distance")+
facet_grid(
  N~state, 
  scale = "free_x"
)+
geom_rect(
  aes(
    xmin = -Inf, 
    xmax = Inf, 
    ymin = -Inf, 
    ymax = Inf, 
    fill = threshold
  ),
  data       = median_of_errors[
    median_of_errors$N %in% paste0("N = ",c(100,200,300,1000)),
 ],
 inherit.aes = FALSE,
 alpha       = 0.15
)+
scale_fill_manual(values=c("grey", "gold"))+
geom_histogram(color="white", fill="grey55")+
geom_vline(
  xintercept = 0.01, 
  col        = "red"
)+
geom_vline(
  data = median_of_errors[
    median_of_errors$N %in% paste0("N = ",c(100,200,300,1000)),
  ], 
  aes(xintercept = median), 
  col = "blue"
)+
theme_bw()+
theme(
  text             = element_text(size = 18),
  strip.background = element_rect(fill="grey95"),
  panel.spacing    = unit(1.5, "lines")
)

pdf(
  file   = file.path(
    WD,
    "/figures/distrib_relative_distance_sim_vs_analytic_solutions.pdf"
  ),
  width  = 13,
  height = 13,
)
print(g_dist_distrib)
dev.off()



#- (3) -----------------------------------------------------------------------##
#- compute coeffcient of variation for fixation time  ------------------------##
res$coefficient_of_variation <- sqrt(res$var_fixation_time_simu)/res$fixation_time_simu  
#- plot coeffcient of variation for fixation time ----------------------------##
res$col2 <- sapply(
  res$coefficient_of_variation, 
  function(x){
    ifelse(
      is.na(x), 
      NA, 
      ifelse(
        x> 1.5, 
        "(1.5;2)", 
        ifelse(
          x> 1, 
          "(1;1.5]", 
          ifelse(
            x > 0.5, 
            "(0.5;1]", 
            "[0;0.5]"
            
          )
        )
      )
    )
  }
)

res$col2 <- factor(
  x = res$col2, 
  levels = c(
    "[0;0.5]",
    "(0.5;1]", 
    "(1;1.5]", 
    "(1.5;2)"
  )
)  

g_cv <- ggplot(
  res[res$N %in% paste0("N = ",c(100,200,300,1000)),],
  aes(
    x    = p, 
    y    = alpha, 
    fill = col2
  )
)+
facet_grid(N~state)+
scale_fill_brewer(
  "coefficient of variation", 
  palette = "YlGnBu"
)+
geom_tile(
  color = "white"
)+
labs(
  y = expression(alpha),
  x = expression(p)  
)+
theme_bw()+
theme(
  text             = element_text(size = 18),
  strip.background = element_rect(fill="grey95"),
  panel.spacing    = unit(1.5, "lines")
)

pdf(
  file   = file.path(
    WD,
    "/figures/variance_fixation_sim.pdf"
  ),
  width  = 13,
  height = 12,
)
print(g_cv)
dev.off()

##- coefficient is independant of N ------------------------------------------##
g_cv <- ggplot(
  res[res$N == "N = 1000",],
  aes(
    x    = p, 
    y    = alpha, 
    fill = col2
  )
)+
facet_grid(.~state)+
scale_fill_brewer(
  "coefficient of variation", 
  palette = "YlGnBu"
)+
geom_tile(
  color = "white"
)+
labs(
  y = expression(alpha),
  x = expression(p)  
)+
theme_bw()+
theme(
  text             = element_text(size = 18),
  strip.background = element_rect(fill="grey95"),
  panel.spacing    = unit(1.5, "lines")
)

pdf(
  file   = file.path(
    WD,
    "/figures/variance_fixation_sim2.pdf"
  ),
  width  = 13,
  height = 3.5,
)
print(g_cv)
dev.off()



#- (4) -----------------------------------------------------------------------##
#- analytic solution VS approximation  ---------------------------------------##
res_approximation[
    res_approximation$fixation_time_approximation < 0,
]$fixation_time_approximation <- NA  

res2 <- merge(
  res_analytic_solutions,
  res_approximation,
  c("N","alpha","p","state")
)
res2$dist <- abs(res2$fixation_time_approximation - res2$fixation_time) / res2$fixation_time
threshold_dist <- 0.01
res2 <- do.call(rbind, lapply(
  split(res2, paste0(res2$N, res2$alpha, res2$p, res2$state)),
  function(x){
    target <- which(x$dist < threshold_dist)
    k      <- NA
    if (length(target)>0){
      k <- min(x[target,]$k)
    }
    data.frame(
      N     = x$N[[1]],
      alpha = x$alpha[[1]],
      p     = x$p[[1]],
      state = x$state[[1]],
      k     = k
    )
  }
))

max(res2$k,na.rm=TRUE)
res2$col <- sapply(
  res2$k, 
  function(x){
    ifelse(
      is.na(x),
      NA,
      ifelse(
        x> 60, 
        "(60;85]",
        ifelse(
          x> 40, 
          "(40;60]",
          ifelse(
            x> 20, 
            "(20;40]", 
            ifelse(
              x> 10, 
              "(10;20]", 
              ifelse(
                x > 5, 
                "(5;10]", 
                "[1;5]"
              )
           )
         )
        )
      )
    )
  }
)
res2$col<- factor(
  x      = res2$col, 
  levels = c(
    "[1;5]",
    "(5;10]", 
    "(10;20]", 
    "(20;40]", 
    "(40;60]", 
    "(60;85]"
  )
)

g_approx <- ggplot(
  res2,
  aes(
    x    = p, 
    y    = alpha, 
    fill = col
  )
)+
scale_fill_brewer(
  "k", 
  palette = "Purples"
)+
geom_tile(
  color = "white"
)+
labs(
  y = expression(alpha),
  x = expression(p)  
)+
theme_bw()+
theme(
  text             = element_text(size = 18),
  strip.background = element_rect(fill="grey95")
)

#- zoom on alpha 1 to 3 ------------------------------------------------------##
res_approximation      <- readRDS(file.path(WD, "/data/res_approximation5.rds"))
res_analytic_solutions <- readRDS(file.path(WD, "/data/res_analytic_solutions5.rds"))
res_approximation[
    res_approximation$fixation_time_approximation < 0,
]$fixation_time_approximation <- NA  

res2 <- merge(
  res_analytic_solutions,
  res_approximation,
  c("N","alpha","p","state")
)

res2$dist <- abs(res2$fixation_time_approximation - res2$fixation_time) / res2$fixation_time

threshold_dist <- 0.01
res2 <- do.call(rbind, lapply(
  split(res2, paste0(res2$N, res2$alpha, res2$p, res2$state)),
  function(x){
    target <- which(x$dist < threshold_dist)
    k      <- NA
    if (length(target)>0){
      k <- min(x[target,]$k)
    }
    data.frame(
      N     = x$N[[1]],
      alpha = x$alpha[[1]],
      p     = x$p[[1]],
      state = x$state[[1]],
      k     = k
    )
  }
))

max(res2$k,na.rm=TRUE)
res2$col <- sapply(
  res2$k, 
  function(x){
    ifelse(
      is.na(x),
      NA,
      ifelse(
        x > 5, 
        "(5,11]",
        ifelse(
          x==5, 
          "5",
          ifelse(
            x==4, 
            "4", 
            ifelse(
              x==3, 
              "3", 
              ifelse(
                x==2, 
                "2", 
                "1"
              )
           )
         )
        )
      )
    )
  }
)
res2$col<- factor(
  x      = res2$col, 
  levels = c("1","2", "3", "4", "5", "(5,11]")
)

g_approx2 <- ggplot(
  res2,
  aes(
    x    = p, 
    y    = alpha, 
    fill = col
  )
)+
scale_fill_brewer(
  "k", 
  palette = "Purples"
)+
geom_tile(
  color = "white"
)+
labs(
  y = expression(alpha),
  x = expression(p)  
)+
theme_bw()+
theme(
  text             = element_text(size = 18),
  strip.background = element_rect(fill="grey95")
)

#- plot smallest k with errors under 0.01 ------------------------------------##
pdf(
  file   = file.path(
    WD,
    "/figures/approx.pdf"
  ),
  width  = 10,
  height = 4,
)
print(ggpubr::ggarrange(
  g_approx, 
  g_approx2, 
  labels=c("A","B"),  
  font.label = list(size = 20))
)
dev.off()
