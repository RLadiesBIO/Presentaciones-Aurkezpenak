IC.z.test <-
function(x, sigma, conf.level=0.95) {
  alpha <- 1 - conf.level
  z.alpha <- qnorm(1-alpha/2)
  n <- length(x)
  SE <- sigma/sqrt(n)
  return(mean(x) + c(-1,1)*z.alpha*SE)
}
