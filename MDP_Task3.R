# Capacity for each leg
C <- c(20,40)
# Arrival probabilities (including no booking requests)
lambda <- c(0.55, 0.2, 0.15, 0.1)
# Fare prices (including no booking requests)
price <- c(0, 200, 600, 700)
# resource consumption matrix (including no booking requests)
# swapped row and col values
A <- matrix(c(0,0,1,0,0,1,1,1), nrow = 4, ncol = 2)
# Length of time horizon
T <- 100

# 3-D array to store the expected revenues at all states for all stages
all_V <- array(0, c(C[1]+1, C[2]+1, 100))

# Matrix of optimal states at each stage
optimal_policy <- matrix(0, nrow=T, ncol=2)

# Vector of optimal revenues at each stage
max_revenues <- numeric(T)

# 4-D array to store accept/ reject decisions for each leg type (FL, LN, FN)
all_accept_reject <- array(0, c(C[1]+1, C[2]+1, 3, 100))

# Initialize the optimal expected revenue at stage T=100
V <- matrix(0, nrow=21, ncol=41)
all_V[,,T] <- V

# Begin solving the MDP using Bellman's equation
for (t in ((T-1):1)) { # For each stage starting from t=T=100 to t=0
  
  # Initiate the matrix of values for the current stage
  new_V <- matrix(0, nrow=21, ncol=41)
  
  for (x1 in 0:20) { # Iterate through all possible values for Frankfurt to London
    for (x2 in 0:40) { # Iterate through all possible values for London to New York
      
      # Indices of the matrix for the current state
      idx1 <- x1+1
      idx2 <- x2+1
      
      # Decision for each product type (including no request)
      leg1_dec <- c(
        ifelse(x1 >=1, 1, 0)*(price[2] + V[idx1-1,idx2]), V[idx1, idx2]
      )
      
      leg2_dec <- c(
          ifelse(x2 >=1, 1, 0)*(price[3] + V[idx1,idx2-1]), V[idx1, idx2]
        )
      
      leg12_dec <- c(
        ifelse(x1>=1,1,0)*ifelse(x2>=1,1,0)*(price[4] + V[idx1-1,idx2-1]), V[idx1, idx2]
      )
      
      # Store accept/reject decisions as 1/0 for each leg in this stage and state
      all_accept_reject[idx1, idx2, 1, t] <- ifelse(which.max(leg1_dec)==1, 1, 0)
      all_accept_reject[idx1, idx2, 2, t] <- ifelse(which.max(leg2_dec)==1, 1, 0)
      all_accept_reject[idx1, idx2, 3, t] <- ifelse(which.max(leg12_dec)==1, 1, 0)
      
      
      # Calculate the optimal expected revenue of the given state
      new_V[idx1, idx2] <- lambda[1]*(price[1] + V[idx1, idx2]) +
        lambda[2]*max(leg1_dec) +
        lambda[3]*max(leg2_dec) +
        lambda[4]*max(leg12_dec)
    }
  }
  
  # Store the expected revenues at this stage for all states in the state space
  all_V[,,t] <- new_V
  
  # Update the expected revenues for the next stage
  V <- new_V
  
  # Find the index of the optimal expected revenues
  opt_idx <- which.max(V)
  
  # Find and store the optimal state which leads to the maximal revenue
  optimal_state <- arrayInd(opt_idx, dim(V))
  optimal_policy[t,] <- optimal_state - 1
  
  # Store the value of the optimal revenue at this stage
  max_revenues[t] <- max(V)
  
}


# Plotting the optimal policy
plot(seq(1, 100, by=1), optimal_policy[,2], type="l", col="red", lwd=2,
     main = "Availability of seats over time", 
     xlab=expression(Stages ~ italic(t)),
     ylab="Number of available seats")
lines(seq(1, 100, by=1), optimal_policy[,1], col="darkgreen", lwd=2)
legend(10, 15, 
       legend = c("Leg 2: London to New York", "Leg 1: Frankfurt to London"), 
       col = c("red", "darkgreen"), lwd = 2, bty = "n")


# Optimal revenue
max_revenues[1]

