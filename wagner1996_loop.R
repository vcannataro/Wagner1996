
N <- 10
p <- 0.5 #0.1-0.9
c <- 0.5#0.4-1
generations <- 3*N

run.for <- 1e6

s.0 <- rep(-1,N)
for(i in 1:N){
  if(rbinom(1,1,p)==1){
    s.0[i] <- 1
  }
}

s.opt <- rep(-1,N)
for(i in 1:N){
  if(rbinom(1,1,p)==1){
    s.opt[i] <- 1
  }
}

for(qq in 1:run.for){
  w <- matrix(NA,N,N)
  
  for(i in 1:length(w)){
    if(runif(1)<c){
      w[i] <- round(rnorm(1,0,1),3)
    }else{
      w[i] <- 0
    }
  }
  
  
  s.list <- matrix(0,generations,N)
  
  s.list[1,] <- s.0  
  
  
  for(z in 2:generations){
    
    for(i in 1:N){
      
      to.sum <- w[i,1]*s.list[z-1][1]
      
      for(j in 2:N){
        
        to.sum <- to.sum+(w[i,j]*s.list[(z-1),j])
        
      }
      if(to.sum >0){
        s.list[z,i] <- 1
      }else{
        s.list[z,i] <- -1
      }
      if(to.sum==0){
        s.list[z,i] <- 0
      }
      
    }
  }
  
  
#   s.list
  
  if(identical(s.list[generations,],s.list[(generations-1),])){break}
  
  
}



