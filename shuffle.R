rm( list = ls())

iter=50000 # Number of times shuffle algorithm carried out
x <- 1:60 # Initial ordering of cards wlog

mash <- function(X , b){
# This function takes in an ordering and a bias for a basic mash
  # shuffle and outputs a new ordering. Note this function is only
  # for 60 cards
  
  Y <- rep(0,60) # Final card position after shuffle step
  for (i in 1:60){
    
    # There are 4 groups created in a mash shuffle
    if (i <= (30-b)){
      Y[i] <- X[ 2*i + b ] # Shuffled bit
    }else if (i>=(31+b)){
      Y[i] <- X[ 2*(i-30) - b - 1] # Shuffled bit
    }else if (i<=30){
      Y[i] <- X[ 30+i ] # Unshuffled bit
    }else if (i>=31){
      Y[i] <- X[ i-30 ] # Unshuffled bit
    }else{
      stop( "shuffle fail" )
    }
  }
  return(Y)
}

pile <- function(X){
  # This shuffle is a basic reordering, although writing a neat algebraic function
  # would become icky very fast between the 7 piles
  Y <- c( X[54] , X[47] , X[40] , X[33] , X[26] , X[19] , X[12] , X[5] ,
          X[57] , X[50] , X[43] , X[36] , X[29] , X[22] , X[15] , X[8] , X[1] , 
          X[55] , X[48] , X[41] , X[34] , X[27] , X[20] , X[13] , X[6] , 
          X[60] , X[53] , X[46] , X[39] , X[32] , X[25] , X[18] , X[11] , X[4] , 
          X[56] , X[49] , X[42] , X[35] , X[28] , X[21] , X[14] , X[7] , 
          X[59] , X[52] , X[45] , X[38] , X[31] , X[24] , X[17] , X[10] , X[3] ,
          X[58] , X[51] , X[44] , X[37] , X[30] , X[23] , X[16] , X[9] , X[2]  )
   
  return( Y ) 
}

loc <- matrix( 0 ,ncol=60, nrow=iter) # Dummy matrix to hold all the final shuffled orderings


# Large amount of samples of shuffles
for (j in (1:iter)){
  
  # Place shuffle algorithm here. You could do a for loop but I prefer a simple list.
  # Be very careful to make sure you overwrite Y at each shuffle instead of using X more than once.
  p <- c( 10,11,12,13,14 ) # Possible mash shuffle options

  Y <- mash(x, sample(p,1))
  Y <- mash(Y, sample(p,1))
  Y <- pile(Y)
  Y <- mash(Y, sample(p,1))
  Y <- mash(Y, sample(p,1))
  Y <- mash(Y, sample(p,1))
  Y <- mash(Y, sample(p,1))
  Y <- mash(Y, sample(p,1))
  Y <- mash(Y, sample(p,1))
  Y <- mash(Y, sample(p,1))
  Y <- mash(Y, sample(p,1))
  Y <- mash(Y, sample(p,1))
  Y <- mash(Y, sample(p,1))
  
  for (k in 1:60){
    loc[j,k] <- which( Y == k ) # Collecting the results of the shuffle 
  }
}

Dev <- matrix(0 , nrow=60 , ncol = 60) # Results matrix that tallies up the number of times 
                                        # a final location is reached for a card (distribution)

for (m in 1:60){
  for (n in 1:60){
    Dev[n,m] <- sum( loc[,n] == m )
  }
}

print( min( Dev ) ) # How far off are we?
print( which( Dev == min(Dev) , arr.ind = T)) # Where did we go wrong? [ location , card ]
print( max( Dev ) )
print( which( Dev == max(Dev) , arr.ind = T) )