

for(i in 1:30)
{
  f <- fixation_time(0.5,alpha,5:95/100)
  c(min(f), max(f))
  print((upper_bound_value(i,seq(2,100,by = 2))/min(f) < 0.001)*1 + (upper_bound_value(i,seq(2,100,by = 2))/min(f) < 0.01)*1)
}


#### TEST AND PLOT

M <- matrix(0,30,50)

for(i in 1:30)
{
  f <- fixation_time(0.5,alpha,5:95/100)
  c(min(f), max(f))
  M[i,] <- (upper_bound_value(i,seq(2,100,by = 2))/min(f) < 0.001)*1 + (upper_bound_value(i,seq(2,100,by = 2))/min(f) < 0.01)*1
}

image(M)
