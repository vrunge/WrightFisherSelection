library(ggplot2)

WD <- "/guillem/aliehrmann/Documents/WrightFisherSelection-paper"

#- load fixation times -------------------------------------------------------##
res_approximation <- readRDS(file.path(WD, "/data/res_approximation.rds"))
res_analytic_solutions <- readRDS(file.path(WD, "/data/res_analytic_solutions.rds"))
res_simu_fixations <- readRDS(file.path(WD, "/data/res_simu_fixations.rds"))

#- simu VS analytic solutions ------------------------------------------------##
res <- merge(
  res_analytic_solutions,
  res_simu_fixations,
  c("N","alpha","p","state")
)

#- compute relative distance -------------------------------------------------##
res$dist <- abs(res$fixation_time_simu - res$fixation_time) / res$fixation_time_simu

res$col <- sapply(
  res$dist, 
  function(x){
    ifelse(
      is.na(x), 
      NA, 
      ifelse(
        x> 0.2, 
        "]20%;+Inf[", 
        ifelse(
          x> 0.1, 
          "]10%;20%[", 
          ifelse(
            x > 0.01, 
            "]1%;10%]", 
            ifelse(
              x > 0.001, 
              "]0.1%;1%]", 
              ifelse(
                x > 0.0001, 
                "]0.01%;0.1%]", 
                "[0;0.01%]"
              )
            )
          )
        )
      )
    )
  }
)

#- relative distance matrix plot ---------------------------------------------##
g <- ggplot(
  res,
  aes(
    x    = p, 
    y    = alpha, 
    fill  = col
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
theme_bw()+
theme(
  text             = element_text(size = 15),
  strip.background = element_rect(fill="grey95")
)

pdf(
  file   = file.path(
    WD,
    "/figures/relative_distance_sim_vs_analytic_solutions.pdf"
  ),
  width  = 18,
  height = 10,
)
print(g)
dev.off()

#- variance matrix plot ------------------------------------------------------##
g2 <- ggplot(
  res,
  aes(
    x    = p, 
    y    = alpha, 
    fill = log2(var_fixation_time_simu)
  )
)+
facet_grid(N~state)+
geom_tile(
  color = "white"
)+
theme_bw()+
theme(
  text             = element_text(size = 15),
  strip.background = element_rect(fill="grey95")
)

pdf(
  file   = file.path(
    WD,
    "/figures/variance_fixation_sim.pdf"
  ),
  width  = 18,
  height = 10,
)
print(g2)
dev.off()

res

#- simu VS approximation  ----------------------------------------------------##

# SOMETHING IS BROKEN HERE ---------------------------------------------------##
which(res_approximation$fixation_time_approximation < 0) 
