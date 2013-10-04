Nsim <- 5000
N <- 2500

getbin <- function(N,p){
  sample.int(2,N,replace=T, prob=c(1-p,p))-1
}
tot.handouts <- matrix(0,nrow=N, ncol=Nsim)
probs <- vector('list',3) # One for each day
get.Handouts <- function(p,N,p.takehand){
  nHand <- rep(0,N)
  for(i in 1:nrow(p)){
    nHand <- nHand+getbin(N,p[i,1])*getbin(1,p[i,2]) * 
      getbin(N,p.takehand)
  }
  return(nHand)
}


# Base parameters ---------------------------------------------------------

get.TotHandouts <- function(N){
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
  
  
  probs[[1]] <- cbind(c(p.Plenary,p.Panel,p.Research,p.Research,p.Workshop,p.Forum),
                      c(phand.Plenary,phand.Panel, phand.Research,phand.Research,
                        phand.Workshop,phand.Forum))
  probs[[2]] <- probs[[1]]
  probs[[3]] <- cbind(c(p.Workshop,p.Panel,p.Workshop, p.Workshop),
                      c(phand.Workshop,phand.Panel,phand.Workshop,phand.Workshop))
  at.Day <- list(getbin(N,p.Day1),getbin(N,p.Day2), getbin(1,p.Day3))
  
  nHandouts <- lapply(probs,get.Handouts, N=N, p.takehand=p.takehand)
  tot.handouts <- rowSums(mapply('*',at.Day,nHandouts))
  return(tot.handouts)
}


set.seed(1234)
sims <- function(Nsim,N){
  output <- matrix(0,ncol=Nsim,nrow=N)
  for(i in 1:Nsim){
    output[,i] <- get.TotHandouts(N)
  }
  return(output)
}

# sims.parallel <- function(Nsim,N){
#   require(foreach)
#   require(doParallel) # for Windows/Mac/Linux
#   #require(parallel) # for Linux/Mac
#   cl <- makeCluster(2)
#   registerDoParallel(cl)
#   output <- foreach(icount(Nsim), 
#                     .export=c('getbin','get.Handouts','probs','get.TotHandouts'),
#                     .combine=cbind) %dopar% {
#     get.TotHandouts(N)
#   }
#   stopCluster(cl)
#   return(output)
# }
system.time(res <- sims(Nsim,N))
# system.time(res <- sims.parallel(Nsim,N))
