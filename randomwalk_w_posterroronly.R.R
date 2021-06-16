randomwalk_w_posterroronly <- function(v,a,ter=.2,z=.5,t2=1,s=1,dt=.001,ntrials=100,only_post_err){
  
  p_up = .5 * (1 + ((v*sqrt(dt))/s)) #probability of upwards evidence
  data <- matrix(NA,nrow=ntrials,ncol=4)
  
  for(j in 1:ntrials){
    evidence <- a*z;rt1=ter
    
    while(evidence <= a && evidence >= 0){
      evidence <- ifelse(p_up>runif(1),evidence+s*sqrt(dt),evidence-s*sqrt(dt))
      rt1=rt1+dt
    }
  
    acc = 1
    acc[evidence < (z*a)] <- 0 #assuming upper is always correct
    rt2 <- rt1
    
    #post-decision accumulation of error evidence
    for(i in 1:(t2/dt)){
      if(only_post_err){
        if(acc==1){
          evidence <- ifelse(p_up>runif(1),evidence,evidence-s*sqrt(dt)) #error evidence after reaching the upper boundary 
        }else{
          evidence <- ifelse(p_up>runif(1),evidence+s*sqrt(dt),evidence) #error evidence after reaching the lower boundary 
        }
      }else{
        evidence <- ifelse(p_up>runif(1),evidence+s*sqrt(dt),evidence-s*sqrt(dt)) #intact post-decision accumulation
      }
      rt2 <- rt2+dt
    }
    #post-decision evidence should depend on which bound is reached    
    if(acc == 1){
      evidence = evidence-a
    }else{
      evidence = -evidence
    }
    
    #add to DF
    data[j,] <- c(rt1,acc,rt2,evidence)
  }
  
  return(data)
}
