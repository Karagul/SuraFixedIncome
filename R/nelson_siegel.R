nelson_siegel <- function(betaV , mats)
{
  # betaV = beta1 -3, lambda1
  gam = mats / betaV [4]
  y = betaV [1] + betaV [2] * ((1 - exp(-gam)) / (gam)) + betaV [3] * (((1 - exp(-gam)) / (gam)) - exp(-gam))
  return(y)
}
