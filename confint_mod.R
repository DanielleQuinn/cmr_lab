confint_mod <- function(poptable, cival = 0.95)
{
  highCI <- sum(poptable$n * poptable$M)/poiCI(x = sum(poptable$m), conf.level = cival, type = "exact")[1]
  lowCI <- sum(poptable$n * poptable$M)/poiCI(x = sum(poptable$m), conf.level = cival, type = "exact")[2]
  output <- c(lowCI, highCI)
  names(output) <- c("low CI", "high CI")
  return(output)
}