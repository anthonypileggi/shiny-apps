library(ggplot2)
library(plotly)

# Draw from the `stopping cost` distribution
estimateStoppingCost <- function(items=400, n=0, x=0, fail_cost=30, labor_cost=20/60, B=2000) {
  
  # draw from posterior distribution of failure probabilities (p)
  #       x|p ~ Binomial(n,p)
  #       p ~ Beta(1,3)      <- weak prior, centered around lower probabiltiies
  p <- rbeta(B,1+x,3+n-x)    # posterior (conjugate)
  
  # drow from binomial distribution -- simulate product testing, conditional on failure probabilities
  failures <- rbinom(B, items-n, prob=p)
  
  # return failure cost (assumes testing is stopped early)
  #     `failure cost` = `cost of selling faulty products` - `cost of labor to test product`
  return(failures*fail_cost - (items-n)*labor_cost)

}

# Estimates:  - current batch failure probability
#             - number of consecutive successes required to stop early
#             - likelihood of stopping early (based on current estimates)
estimateStoppingTime <- function(items=400, n=0, x=0, fail_cost=30, labor_cost=20/60) {
  
  a0 <- 1
  b0 <- 3
  
  n_seq <- n:items
  phat <- ((a0+x)/(a0+b0+n_seq))
  times <- phat*(items-n_seq)*fail_cost - (items-n_seq)*labor_cost
  stop_n <- ifelse(is.na(which(times<0)[1]), items, which(times<0)[1])
  
  return(list(current_phat=phat[1], stop_time = stop_n, stop_prob=1-pgeom(stop_n,phat[1])))
}
