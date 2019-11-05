z.test <-
function(x, mu=0, sigma=1){
  n <- length(x)
  SE <- sigma/sqrt(n)
  z <- (mean(x)-mu)/SE
  p.value <- (1-pnorm(abs(z)))*2
  return(list(media=mean(x), z=z, p.valor=p.value))
}
