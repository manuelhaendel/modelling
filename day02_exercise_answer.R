library(BayesianTools)
library(sensitivity)
?VSEM


# Create input data for the model
PAR <- VSEMcreatePAR(1:1000)
plot(PAR, main = "PAR (driving the model)", xlab = "Day")

# load reference parameter definition (upper, lower prior)
refPars <- VSEMgetDefaults()
tail(refPars)

# create some simulated test data 
# generally recommended to start with simulated data before moving to real data
referenceData <- VSEM(refPars$best[1:11], PAR) # model predictions with reference parameters  
referenceData[,1] = 1000 * referenceData[,1] 

par(mfrow = c(2,2))
plot(referenceData[,1], main = "NEE (Net Primary Productivity)", xlab = "Day")
plot(referenceData[,2], main = "Cv (Above-ground vegetation pool)", xlab = "Day")
plot(referenceData[,3], main = "Cs (Carbon in organic matter)", xlab = "Day")
plot(referenceData[,4], main = "Cr (Below-ground vegetation pool )", xlab = "Day")



vary_pars <- function(value, par, pool = "Cv", par_default = VSEMgetDefaults()$best, PAR = VSEMcreatePAR(1:1000)){
  par_default[par] <- value
  out <- VSEM(par_default, PAR)
  
  return(tail(out[, which(colnames(out) == pool)], 1))
}


sensitivity <- function(pools, pars, measure){
  if(measure == "abs")
    sens <- diff(pools) / diff(pars)
  
  if(measure == "rel")
    sens <- (tail(pools, -1) / head(pools, -1)) / (tail(pars, -1) / head(pars, -1))
  
  return(sens)
}


LUE <- seq(refPars$lower[3], refPars$upper[3], length.out = 20)
out_LUE <- sapply(LUE, vary_pars, par = 3)

par(mfrow = c(1,1))
plot(sensitivity(out_LUE, LUE, "abs"))
plot(sensitivity(out_LUE, LUE, "rel"))

which(rownames(refPars) == "tauV")
tauV <- seq(refPars$lower[5], refPars$upper[5], length.out = 20)
out_tauV <- sapply(tauV, vary_pars, par = 5)

par(mfrow = c(1,1))
plot(sensitivity(out_tauV, tauV, "abs"))
plot(sensitivity(out_tauV, tauV, "rel"))

par(mfrow = c(2,3))
for(par in rownames(refPars)[1:6]){
  index <- which(rownames(refPars) == par)
  pars <- seq(refPars$lower[index], refPars$upper[index], length.out = 20)
  out <- sapply(pars, vary_pars, par = index)
  
  plot(x = head(pars, -1), y = sensitivity(out, pars, "rel"), pch = 20,
       main = paste("Sensitivity above ground biomass: ", par),
       ylab = "Relative sensitivity", xlab = "Parameter values")
}


helper <- function(x, pool = "Cv", par_default = VSEMgetDefaults()$best, PAR = VSEMcreatePAR(1:1000)){
  par_default[1:6] <- x
  out <- VSEM(par_default, PAR)
  
  col <- which(colnames(out) == pool)
  out <- mean(out[,col])
  
  return(out)
}

myFun <- function(mt,  pool = "Cv", par_default = VSEMgetDefaults()$best, PAR = VSEMcreatePAR(1:1000)){
  
  out <- apply(X = mt, MARGIN = 1, FUN = helper, pool = pool, par_default = par_default, PAR = PAR)
  return(out)
}

morris_out <- morris(myFun, factors = rownames(refPars)[1:6], r = 4,
                     design = list(type = "oat", levels = 5, grid.jump = 3),
                     binf = refPars$lower[1:6],
                     bsup = refPars$upper[1:6])


test <- refPars$best
test[3] <- refPars$lower[3]
test_helper <- helper(test[1:6])




lowerLUE <- refPars$best
lowerLUE[3] <- refPars$lower[3]

lowerLUE <- VSEM(lowerLUE, PAR)
dCv_abs <- tail(lowerLUE[,2], 1) - tail(referenceData[,2], 1)
dLUE_abs <- refPars$lower[3] - refPars$best[3]

sens_abs <- dCv_abs / dLUE_abs

dCv_rel <- tail(lowerLUE[,2], 1) / tail(referenceData[,2], 1)
dLUE_rel <- refPars$lower[3] / refPars$best[3]

sens_rel <- dCv_rel / dLUE_rel

