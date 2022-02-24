

library(expint)
x <- seq(from = -3, to = 3, by = 0.01)
plot(x, expint_Ei(x), type = 'l', lwd = 2, main = "Ei(x)")
