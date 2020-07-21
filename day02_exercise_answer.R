library(BayesianTools)
?VSEM


# Create input data for the model
PAR <- VSEMcreatePAR(1:1000)
plot(PAR, main = "PAR (driving the model)", xlab = "Day")

# load reference parameter definition (upper, lower prior)
refPars <- VSEMgetDefaults()
# this adds one additional parameter for the likelihood standard deviation (see below)
refPars[12,] <- c(2, 0.1, 4) 
rownames(refPars)[12] <- "error-sd"
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






lowerLUE <- refPars$best
lowerLUE[3] <- refPars$lower[3]

lowerLUE <- VSEM(lowerLUE, PAR)
dCv_abs <- tail(lowerLUE[,2], 1) - tail(referenceData[,2], 1)
dLUE_abs <- refPars$lower[3] - refPars$best[3]

sens_abs <- dCv_abs / dLUE_abs

dCv_rel <- tail(lowerLUE[,2], 1) / tail(referenceData[,2], 1)
dLUE_rel <- refPars$lower[3] / refPars$best[3]

sens_rel <- dCv_rel / dLUE_rel


# this adds the error - needs to conform to the error definition in the likelihood
obs <- referenceData + rnorm(length(referenceData), sd = refPars$best[12])
oldpar <- par(mfrow = c(2,2))
for (i in 1:4) plotTimeSeries(observed = obs[,i], 
                              predicted = referenceData[,i], main = colnames(referenceData)[i])

# Best to program in a way that we can choose easily which parameters to calibrate
parSel = c(1:6, 12)