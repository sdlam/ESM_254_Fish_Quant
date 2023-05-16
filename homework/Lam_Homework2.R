#### Part 1: Data ####

# add your code here


#### Part 2: Model hypotheses ####

## Model 1: Logistic ####

pars <- c(0.01, 10000)# r and K initial test values

model1 <- function(pars, dat) {
  
  # add your code here
  
  return(predicted_N)
}



# How well do these initial parameter values perform?
# Also, a way to test that our function is working correctly

mod1_predN <- model1(pars=pars, dat=gray)

# add plot code here

#### Statistical Model 1 Code - Estimating best parameter estimates ####
# Written for your review, no need to edit it for this assignment

# Remove rows with NA in the Estimated.abundance column
gray_subset <- gray[!is.na(gray$Estimated.abundance), ]

# Subset catch_data and years to match the subsetted data
catch_data <- gray$Catches[!is.na(gray$Estimated.abundance)]
years <- gray$Year[!is.na(gray$Estimated.abundance)]

# The statistical model! 
model_1 <- nls(Estimated.abundance ~ model1(pars, gray_subset),
             data = gray_subset,
             start = list(pars = pars),
             algorithm = "port",
             lower = c(0, -Inf),
             upper = c(0.118, Inf),
             control = nls.control(maxiter = 1000),
             na.action = na.exclude)

# The best estimates for the two parameters
best.param1 <- summary(model_1)$parameters
best.param1[,1]

# Plot how well these predictions fit to the data when using best param estimates
mod1best_predN <- model1(pars=best.param1[,1], gray)
# add plot code here to add this as a line to your plot


## Model 2: Pella-Tomlinson 0.60K ####

pars <- c(0.01, 10000)# initial test values

model2 <- function(pars, dat) {
  
  # add your code here
  
  return(predicted_N)
}

# How well do these initial parameter values perform?
# Also, a way to test that our function is working correctly

mod2_predN <- model2(pars, gray)

# add your plot code here

#### Statistical Model 2 Code - Estimating best parameter estimates ####
# Written for your review, no need to edit it for this assignment

# Remove rows with NA in the Estimated.abundance column
gray_subset <- gray[!is.na(gray$Estimated.abundance), ]

# Subset catch_data and years to match the subsetted data
catch_data <- gray$Catches[!is.na(gray$Estimated.abundance)]
years <- gray$Year[!is.na(gray$Estimated.abundance)]

# The statistical model! 
model_2 <- nls(Estimated.abundance ~ model2(pars, gray_subset),
             data = gray_subset,
             start = list(pars = pars),
             algorithm = "port",
             lower = c(0, -Inf),
             upper = c(0.118, Inf),
             control = nls.control(maxiter = 1000),
             na.action = na.exclude)

# The best estimates for the two parameters
best.param2 <- summary(model_2)$parameters
best.param2[,1]

# Plot how well these predictions fit to the data when using best param estimates
mod2best_predN <- model2(pars=best.param2[,1], gray$Catches, gray$Year)
lines(x=gray$Year, y=mod2best_predN, lwd=2, col="red")

## Model 3: Pella-Tomlinson 0.5-0.9K ####

pars <- c(0.01, 10000, 35) # initial test values

model3 <- function(pars, dat) {
  
 # add your code here
  
  return(predicted_N)
}

# How well do these initial parameter values perform?
# Also, a way to test that our function is working correctly

mod3_predN <- model3(pars, gray)

# add your plot code here

#### Statistical Model 3 Code - Estimating best parameter estimates ####
# Written for your review, no need to edit it for this assignment

# Remove rows with NA in the Estimated.abundance column
gray_subset <- gray[!is.na(gray$Estimated.abundance), ]

# Subset catch_data and years to match the subsetted data
catch_data <- gray$Catches[!is.na(gray$Estimated.abundance)]
years <- gray$Year[!is.na(gray$Estimated.abundance)]

# The statistical model! 
model_3 <- nls(Estimated.abundance ~ model3(pars, gray_subset),
               data = gray_subset,
               start = list(pars = pars),
               algorithm = "port",
               lower = c(0, -Inf, 1),
               upper = c(0.118, Inf, 35),
               control = nls.control(maxiter = 1000),
               na.action = na.exclude)

# The best estimates for the three parameters
best.param3 <- summary(model_3)$parameters
best.param3[,1]

# Plot how well these predictions fit to the data when using best param estimates
mod3best_predN <- model3(pars=best.param3[,1], gray)
# add your plot code here

## Model 4: Pella-Tomlinson 0.5-0.9K with N0 #### 

pars <- c(0.01, 10000, 35, 10000) # initial test values

model4 <- function(pars, dat) {
  
  # add your code here
  
  return(predicted_N)
}

# How well do these initial parameter values perform?
# Also, a way to test that our function is working correctly

mod4_predN <- model4(pars, gray)

# add your plot code here

#### Statistical Model 4 Code - Estimating best parameter estimates ####
# Written for your review, no need to edit it for this assignment

# Remove rows with NA in the Estimated.abundance column
gray_subset <- gray[!is.na(gray$Estimated.abundance), ]

# Subset catch_data and years to match the subsetted data
catch_data <- gray$Catches[!is.na(gray$Estimated.abundance)]
years <- gray$Year[!is.na(gray$Estimated.abundance)]

# The statistical model! 
model_4 <- nls(Estimated.abundance ~ model4(pars, gray_subset),
               data = gray_subset,
               start = list(pars = pars),
               algorithm = "port",
               lower = c(0, -Inf, 1, -Inf),
               upper = c(0.118, Inf, 35, Inf),
               control = nls.control(maxiter = 1000),
               na.action = na.exclude)

# The best estimates for the 4 parameters
best.param4 <- summary(model_4)$parameters
best.param4[,1]

# Plot how well these predictions fit to the data when using best param estimates
mod4best_predN <- model4(pars=best.param4[,1], gray)
lines(x=gray$Year, y=mod4best_predN, lwd=2, col="red")

## Model 5: Pella-Tomlinson 0.5-0.9K with N0=K #### 

pars <- log(c(.01,1)) # initial parameter values in logspace (makes estimation easier)

model5 <- function(pars, dat) {
  pars <- exp(pars)
  
  r <- pars[1]
  K <- 100670
  z <- pars[2]
 
  # add your code here, I set up the params for you this time
  
  return(predicted_N)
}

# How well do these initial parameter values perform?
# Also, a way to test that our function is working correctly

mod5_predN <- model5(pars,gray)

# add your plot code here

#### Statistical Model 5 Code - Estimating best parameter estimates ####
# Written for your review, no need to edit it for this assignment

# The statistical model! 
fn_opt <- function(pars,tdat=gray) { # short hand for "function to optimize"
  
  pred <- model5(pars, tdat)
  nll <- sum(log((pred/tdat[,2]))^2, na.rm=T)
  
  z <- exp(pars[2])
  if(z >35 | z <1) nll <- Inf
  return(nll)
}

model_5 <- optim(pars, fn_opt)

# The best estimates for the three parameters
best.param5 <- model_5$par
exp(best.param5)

# Plot how well these predictions fit to the data when using best param estimates
mod5best_predN <- model5(best.param5, gray)
# add your plot code here


## Model 6: Pella-Tomlinson with inflated catches ####

pars <- log(c(.01, 1, 10)) # initial param values

model6 <- function(pars, dat) {
  pars <- exp(pars)
  
  r <- pars[1]
  K <- 100670
  z <- pars[2]
  x <- pars[3]
  
 # add your code here
  
  return(predicted_N)
}



# How well do these initial parameter values perform?
# Also, a way to test that our function is working correctly

mod6_predN <- model6(pars,gray)

# add your plot code here

#### Statistical Model 6 Code - Estimating best parameter estimates ####
# Written for your review, no need to edit it for this assignment

# The statistical model! 
fn_opt <- function(pars,tdat=gray) {
  
  pred <- model6(pars, tdat)
  nll <- sum(log((pred/tdat[,2]))^2,na.rm=T)
  
  z <- exp(pars[2])
  if(z >35 | z <1) nll <- Inf
  return(nll)
}

model_6 <- optim(pars, fn_opt)

# The best estimates for the three parameters
best.param6 <- model_6$par
exp(best.param6)

# Plot how well these predictions fit to the data when using best param estimates
mod6best_predN <- model6(best.param6, gray)
# add your plot code here
