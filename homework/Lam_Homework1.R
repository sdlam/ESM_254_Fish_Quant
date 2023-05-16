

## Question 1
x <- vector(mode = "logical", length = 10) # use the vector() function here # fill in x with number values here
x <- (5:14)

## Question 2
Y <- vector(mode = "logical", length = 100)# use the vector() function
Y <- seq(from = 2, to = 200, by = 2)# fill in the Y vector here
logic.index1 <- Y >= 134 # write a logical for Y values that are greater than or equal to 134
Y[logic.index1] <- NA
logic.index2 <- Y <= 20 # write a logical for Y values that are less than or equal to 20
Y[logic.index2] <- 0
Y

## Question 3

sum100 <- function() {
  
  answer <- sum(1:100)# add code here
  
  return(answer)
}

sum100()

## Question 4

sumN <- function(N) { # add parameter to this line
  
  answer <- sum(1:N)# add code here
  
  return(answer)
}

sumN(N=100) # test your code here; did you get the same answer as sum100()?

## Question 5

# hint: ?matrix

increasing.mat <- function(tot.row, tot.col) { # add parameters to this line
  
  N <- tot.row*tot.col # use this local variable for the data argument of matrix()
  answer <- matrix(c(1:N), nrow = tot.row, ncol = tot.col, byrow = FALSE) # use the matrix() function here
    
  return(answer)
}

increasing.mat(tot.row=4, tot.col=5)

#### Part 2

#install.packages("MQMF")
library(MQMF)

## Section 1: growth curve function

growth.curve <- function(p, age.data) {
  
  Sim.Length.Data <- p[1] * (1 - exp(-p[2] * (age.data - p[3])))
  
  return(Sim.Length.Data)
}

# create an object that defines the p input to growth.curve()
pars <- c(75,0.1,-10.0,3.5) # these will be given to the p argument later
plabels <- c("Linf","K","t0","sigma")


# Section 2:
# let's load in a practice data set

data(minnow)
week <- minnow$week
length <- minnow$length

# plot the experimental data
plot(x=week,
     y=length,
     ylab="Body Length (mm)",
     xlab="Age (Weeks)",
     pch=19,
     main="Minnow")


# Let's look at an example for simulated data using some stand-in parameter values
example.sim.Length <- growth.curve(p=pars[1:3], age.data=week)
lines(x=week, y=example.sim.Length, lwd=2, col="darkgray")


# Section 3: 
# Let's use a statistical model to estimate parameter values for growth.curve() to use

best.growth.est <- nlm(f=negNLL,
                       p=pars,
                       funk=growth.curve,
                       age.data=week,
                       observed=length,  
                       typsize=magnitude(pars))  


# The estimated parameter values:
best.growth.est$estimate[1:3]

# The parameter values we used to draw a line earlier
pars[1:3]


# Section 4 - how well do the estimated parameter values perform?
predicted.Length <- growth.curve(best.growth.est$estimate,0:160)  
lines(x=0:160, y=predicted.Length, lwd=2, col="red")

