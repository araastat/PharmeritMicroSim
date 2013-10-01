lambda=1; mu=20
N <- 1000

set.seed(146)
entries <- sort(rexp(N,lambda))
waits <- rexp(N,mu)

exits <- rep(0,length(entries))
exits[1] <- entries[1]+waits[1]
for(i in 2:N){
  if(exits[i-1]> entries[i]){
    exits[i] <- exits[i-1]+waits[i]
  } else {
    exits[i] <- entries[i]+waits[i]
  }
}