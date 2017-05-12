#The given test is ¡°reject H0 if Z > 1.645
mu1 = seq(-1, 0, 0.1)
Power1 = 1-pnorm(1.645-(mu1-0)/(1/sqrt(20)))


mu2 = seq(0, 1, 0.1)
Power2 = pnorm(-1.645+(mu2-0)/(1/sqrt(20)))


mu3 = seq(-1, 1, 0.1)
Power3 = 1-pnorm(1.96-(mu3-0)/(1/sqrt(20)))


plot(mu,xlim = c(-1, 1), Power)
lines(mu2,Power2,lty = 2)
lines(mu3,Power3,lty = 3)




