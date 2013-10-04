
attended.Day = function(p.Day, size=2500){
  x = sample.int(2, size=size, replace=T,
                 prob=c(1-p.Day,p.Day))-1
  return(x)
}

attended.session = function(p.session, size=2500){
  x = sample.int(2, size=size, 
                 replace=T,
                 prob=c(1-p.session,p.session))-1
  return(x)
} 

is.handout.available = function(p.handout.session, size=1){
  x = sample.int(2, size=size, 
                 replace=T,
                 prob=c(1-p.handout.session,p.handout.session))-1
  return(x)
} 

taking.handout = function(p = p.take.handout, size=2500){
  x = sample.int(2, size=size, replace=T,
                 prob = c(1-p, p)) -1
  return(x)
}

getting.handout.Day.session = function(p.Day, 
                                       p.session, p.handout.session){
  result = attended.Day(p.Day) *
    attended.session(p.session) *
    is.handout.available(p.handout.session) * 
    taking.handout()
  return(result)}


getting.handout.Day.session.v2 = 
  function(probs){
    p.Day = probs[1]; p.session=probs[2];
    p.handout.session = probs[3];
    p.take.handout = probs[4]    
    x = attended.Day(p.Day)*
      attended.session(p.session)*
      is.handout.available(p.handout.session)*
      taking.handout(p.take.handout)
    return(x)
  }


average.handouts <- function(probs){
  got.handout <- lapply(probs, getting.handout.Day.session.v2)
  total.handouts <- Reduce('+',got.handout)
  return(mean(total.handouts))
}


sims <- function(Nsim=50){
  number.of.simulations <- Nsim
  set.seed(1350)
  avg.handout <- numeric(number.of.simulations)
  
  for(i in 1:number.of.simulations){
    p.Day1 = rbeta(1,8,2)
    p.Day2 = rbeta(1,8,2)
    p.Day3 = rbeta(1,3,2)
    
    p.Plenary <-   rbeta(1,4,6) # Plenary
    p.Issues <-   rbeta(1,4,6) # Issues
    p.Research <- rbeta(1,7,3) # Research
    p.Workshop <-   rbeta(1,7,3) # Workshop
    p.Forum <-   rbeta(1,4,6) # Forum
    
    p.handout.Plenary <-   rbeta(1,19,1) # Plenary
    p.handout.Issues <-   rbeta(1,19,1) # Issues
    p.handout.Research <-   rbeta(1,6,2) # Research
    p.handout.Workshop <-   rbeta(1,8,3) # Workshop
    p.handout.Forum <-   rbeta(1,19,1) # Forum
    
    p.take.handout = rbeta(1,6,6)
    probs.for.Day1 = 
      list(c(p.Day1,p.Plenary,p.handout.Plenary,p.take.handout),
           c(p.Day1,p.Issue, p.handout.Issue, p.take.handout),
           c(p.Day1,p.Research,p.handout.Research, p.take.handout),
           c(p.Day1,p.Research, p.handout.Research, p.take.handout),
           c(p.Day1,p.Workshop, p.handout.Workshop, p.take.handout),
           c(p.Day1,p.Forum, p.handout.Forum, p.take.handout))
    
    probs.for.Day2 = 
      list(c(p.Day1,p.Research,p.handout.Research,p.take.handout),
           c(p.Day1,p.Plenary, p.handout.Plenary, p.take.handout),
           c(p.Day1,p.Issue,p.handout.Issue, p.take.handout),
           c(p.Day1,p.Workshop, p.handout.Workshop, p.take.handout),
           c(p.Day1,p.Workshop, p.handout.Workshop, p.take.handout),
           c(p.Day1,p.Forum, p.handout.Forum, p.take.handout))
    
    probs.for.Day3 = 
      list(c(p.Day1,p.Workshop,p.handout.Workshop,p.take.handout),
           c(p.Day1,p.Issue, p.handout.Issue, p.take.handout),
           c(p.Day1,p.Workshop,p.handout.Workshop, p.take.handout),
           c(p.Day1,p.Workshop, p.handout.Workshop, p.take.handout))
    probs.for.all.sessions <- c(probs.for.Day1, probs.for.Day2, probs.for.Day3)
    
    avg.handout[i] <- average.handouts(probs.for.all.sessions)
  }
  return(avg.handout)
}


