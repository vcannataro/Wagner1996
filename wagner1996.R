
N <- 10 #number of states in expression state vector 
p <- 0.5 #0.1-0.9 the mean fraction of genes expressed in initial and equilibrium state 
c <- 0.5#0.4-1 the mean fraction of connectivities different from zero 
generations <- 3*N #generations to run the simulation

#initialize gene expression pattern 
s.0 <- rep(-1,N)
for(i in 1:N){
  if(rbinom(1,1,p)==1){
    s.0[i] <- 1
  }
}

#create some optimal gene expression pattern
s.opt <- rep(-1,N)
for(i in 1:N){
  if(rbinom(1,1,p)==1){
    s.opt[i] <- 1
  }
}

#connectivity matrix 
w <- matrix(NA,N,N)

#I decided to draw the values from a normal (like he elaborated on in the appendix). 
for(i in 1:length(w)){
  if(runif(1)<c){
    w[i] <- round(rnorm(1,0,1),3)
  }else{
    w[i] <- 0
  }
}

#s.list stores the expression states over the generations of simulations
s.list <- matrix(0,generations,N)
 
s.list[1,] <- s.0  #first entry is the initial pattern
  
#lets run the simulation
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

#print the matrix showing the expression state patterns over "time" 
s.list


#did you hit an equilibrium? 
identical(s.list[generations,],s.list[(generations-1),])







