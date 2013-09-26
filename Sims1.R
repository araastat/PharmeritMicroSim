Nsim <- 5000
N <- 2500

tot.handouts <- matrix(0,nrow=N, ncol=Nsim)
probs <- vector('list',3) # One for each day
get.Handouts <- function(p,N){
  nHand <- rep(0,N)
  for(i in 1:nrow(p)){
    nHand <- nHand+rbinom(N,1,p[i,1])*(rbinom(1,1,p[i,2]*rep(1,N)) * 
      rbinom(N,1,p.takehand)
  }
  return(nHand)
}


# Base parameters ---------------------------------------------------------


 
set.seed(1234)

for(i in 1:Nsim){
get.TotHandouts <- function(i){
  p.Day1 = rbeta(1,8,2)
  p.Day2 = rbeta(1,8,2)
  p.Day3 = rbeta(1,3,2)
  p.Plenary = rbeta(1,4,6)
  p.Forum = rbeta(1,4,6)
  p.Panel = rbeta(1,4,6)
  p.Workshop = rbeta(1,7,3)
  p.Research = rbeta(1,7,3)
  phand.Plenary = rbeta(1,19,1)
  phand.Panel = rbeta(1,19,1)
  phand.Forum = rbeta(1,19,1)
  phand.Workshop = rbeta(1,8,2)
  phand.Research = rbeta(1,6,2)
  p.takehand = rbeta(1,6,6)
  
  at.Day <- list(rbinom(N,1,p.Day1),rbinom(N,1,p.Day2), rbinom(N,1,p.Day3))
  
  probs[[1]] <- cbind(c(p.Plenary,p.Panel,p.Research,p.Research,p.Workshop,p.Forum),
                      c(phand.Plenary,phand.Panel, phand.Research,phand.Research,
                        phand.Workshop,phand.Forum))
  probs[[2]] <- probs[[1]]
  probs[[3]] <- cbind(c(p.Workshop,p.Panel,p.Workshop, p.Workshop),
                      c(phand.Workshop,phand.Panel,phand.Workshop,phand.Workshop))
  
  
  nHand <- lapply(probs,get.Handouts, N=N)
  tot.handouts <- rowSums(mapply('*',at.Day,nHand))
  return(tot.handouts)
}
