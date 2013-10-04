p.Day1 <- p.Day2 <- 0.8
p.Day3 <- 0.6
p.Plenary <- p.Forum <- p.Issues <- 0.4
p.Workshop <- p.Research <- 0.7
p.handout.Plenary <- p.handout.Issues <- p.handout.Forum <- 0.95
p.handout.Workshop <- 0.8
p.handout.Research <- 0.75
p.take.handout <- 0.5

attended.Day1 = sample.int(2, size=2500, 
                           replace=T,
                           prob=c(1-p.Day1,p.Day1))-1 

attended.Plenary.Session = sample.int(2, size=2500, 
                                      replace=T,
                                      prob=c(1-p.Plenary,p.Plenary))-1 
handout.available.Plenary = sample.int(2, size=1, 
                                       prob=c(1-p.handout.Plenary,p.handout.Plenary))-1 
take.handout = sample.int(2, size=2500, 
                          replace=T,
                          prob=c(1-p.take.handout,p.take.handout))-1 


# Functions ---------------------------------------------------------------
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


attended.Day1 = attended.Day(p.Day1)
attended.Day2 = attended.Day(p.Day2)
attended.Day3 = attended.Day(p.Day3)

attended.Plenary.Session = attended.session(p.Plenary)
attended.Forum           = attended.session(p.Forum)
attended.Research.Session = attended.session(p.Research)
attended.Workshop        = attended.session(p.Workshop)
attended.Issues.Panel    = attended.session(p.Issues)

got.handout.Day1.Plenary =
  attended.Day(p.Day1) * attended.session(p.Plenary) * is.handout.available(p.handout.Plenary) * taking.handout()

probs.for.sessions.Day1 = c(
  p.Plenary, p.Issue, p.Research, 
  p.Research,p.Workshop, p.Forum)

probs.for.sessions.Day2 = c(
  p.Research, p.Plenary, p.Issue, 
  p.Workshop,p.Workshop, p.Forum)

probs.for.sessions.Day3 = c(
  p.Workshop, p.Issue, p.Workshop, p.Workshop)

probs.for.handout.sessions.Day1 = c(
  p.handout.Plenary, p.handout.Issue,
  p.handout.Research, p.handout.Research,
  p.handout.Workshop, p.handout.Forum)

probs.for.handout.sessions.Day2 = c(
  p.handout.Research, p.handout.Plenary,
  p.handout.Issue, p.handout.Workshop,
  p.handout.Workshop, p.handout.Forum)

probs.for.handout.sessions.Day3 = c(
  p.handout.Workshop, p.handout.Issue,
  p.Workshop, p.handout.Workshop)



# Looping -----------------------------------------------------------------
probs.Day = list(p.Day1,
                 p.Day2, p.Day3)
probs.for.sessions = list(
  probs.for.sessions.Day1,
  probs.for.sessions.Day2,
  probs.for.sessions.Day3)
probs.for.handout.sessions  <- list(
  probs.for.handout.sessions.Day1,
  probs.for.handout.sessions.Day2,
  probs.for.handout.sessions.Day3)

got.handout = vector('list',3)

set.seed(1234)

t1=proc.time()
for(day in 1:3){
  number.of.sessions = length(probs.for.sessions[[day]])
  
  got.handout[[day]] = matrix(0, nrow=2500,
                              ncol=number.of.sessions)
  
  for(i in 1:number.of.sessions){
    got.handout[[day]][,i] = 
      getting.handout.Day.session(probs.Day[[day]],
                                  probs.for.sessions[[day]][i], 
                                  probs.for.handout.sessions[[day]][i])
  }
}
print(proc.time()-t1)

total.handout = rowSums(do.call(cbind,got.handout))
# Using lapply/sapply -----------------------------------------------------

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

got.handout.Day1 = sapply(probs.for.Day1,
                          getting.handout.Day.session.v2)
got.handout.Day2 = sapply(probs.for.Day2,
                          getting.handout.Day.session.v2)
got.handout.Day3 = sapply(probs.for.Day3,
                          getting.handout.Day.session.v2)

handouts.by.Day = list(got.handout.Day1,
                       got.handout.Day2, got.handout.Day3)
total.handouts.by.day = sapply(handouts.by.Day, rowSums)
total.handouts = rowSums(total.handouts.by.day)


# Quicker with sapply -----------------------------------------------------

probs.for.all.sessions = c(probs.for.Day1,
                           probs.for.Day2, probs.for.Day3)
t1=proc.time()
got.handout = sapply(probs.for.all.sessions,
                     getting.handout.Day.session.v2)
total.handouts = rowSums(got.handout)
print(proc.time()-t1)

# Using lapply ------------------------------------------------------------
t1=proc.time()
got.handout = lapply(probs.for.all.sessions,
                     getting.handout.Day.session.v2)
total.handouts = Reduce('+',got.handout)
print(proc.time()-t1)


# Simulation --------------------------------------------------------------

average.handouts <- function(probs){
  got.handout <- lapply(probs, getting.handout.Day.session.v2)
  total.handouts <- Reduce('+',got.handout)
  return(mean(total.handouts))
}

t1 <- proc.time()
number.of.simulations <- 5000
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

print(proc.time()-t1)

library(ggplot2)
qplot(avg.handout, xlab='Average number of handouts') + 
  ggtitle('5000 simulations')


# Parallel ----------------------------------------------------------------

  require(foreach)
  require(doParallel) # for Windows/Mac/Linux
  cl <- makeCluster(2) 
  registerDoParallel(cl)
  # On windows replace previous 2 lines with 
  # registerDoParallel(cores=2)
  avg.handout <- foreach(icount(number.of.simulations),.combine=c) %dopar% {
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
                      probs.for.all.sessions <- c(probs.for.Day1, probs.for.Day2, probs.for.Day3)
                      
                      return(average.handouts(probs.for.all.sessions))
                    }
  stopCluster(cl)
 