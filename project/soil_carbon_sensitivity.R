library(SoilR)
library(sensitivity)
library(ncdf4)
library(dplyr)
library(lubridate)
library(raster)

setwd("C:/Users/Manuel/Documents/Studium/MSc_Umweltwissenschaften/model_env_sys/project")


# read climate data
## source: WorldClim version 2.1 climate data for 1970-2000. Spatial resolution
## is 2.5 minutes (~21 km2).
paths <- list.files(path = "data/", pattern = "tavg.*\\.tif$", full.names = TRUE)
extract_freiburg <- function(path) extract(raster(path), matrix(c(7.8, 48), ncol=2))
temp <- sapply(paths, extract_freiburg)

paths <- list.files(path = "data/", pattern = "prec.*\\.tif$", full.names = TRUE)
prec <- sapply(paths, extract_freiburg)

data <- data.frame(month = 1:12,
                   temp = temp,
                   prec = prec)
rownames(data) <- NULL


# set model parameters
## evaporation
Evp <- data.frame(Month=1:12,
                  Evp=c(12, 18, 35, 58, 82,
                        90, 97, 84, 54, 31, 14, 10))
## soil thickness in cm
soil <- 25

##  rate modifying factor for moisture 
moist_fact <- fW.RothC(P = data$prec, E = Evp$Evp, S.Thick = soil, bare = TRUE)[,2]

##  rate modifying factor for temperature
temp_fact <- fT.RothC(data$temp)

## external (environmental and/or edaphic) effects on decomposition rates
ext_fx <- moist_fact * temp_fact

## time steps
time <- seq(1/12, 400, 1/12)

## data frame with the external effects for each time step
ext_fx_frame <- data.frame(t = time, fx = rep(ext_fx, 400))

SOC <- 69.7
IOM <- 0.049*SOC^(1.139)

model_output <- RothCModel(t = time, C0 = c(0,0,0,0,IOM),
                           In = 2.7, clay = 48, xi = ext_fx_frame)

carbon <- getC(model_output)

for(i in 1:5){
  if(i == 1){
    plot(time, carbon[,i],
         col = i, ylim = range(carbon), type = "l",
         xlab = "Years", ylab = "Carbon in [Mg/ha]")
    legend(x = 200, y = 15, legend = c("DPM", "RPM", "BIO", "HUM", "IOM"), fill = 1:5)
  } 
  if(i > 1) lines(time, carbon[,i], col = i)
}

## The carbon pools reach a stable state after 300 years. I will use the carbon
## pools after this spin up phase as initial carbon stocks for a default
## parameter list.

init <- c(tail(carbon, 1))
time <- seq(1/12, 400, 1/12)
ext_fx_frame <- data.frame(t = time, fx = rep_len(ext_fx, length.out = length(time)))

default_parms <- list(t = time,
                      ks = c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66,
                             k.HUM = 0.02, k.IOM = 0),
                      C0 = init, In = 2.7, DR = 1.44, clay = 48,
                      xi = ext_fx_frame)

changed_parms <- default_parms
changed_parms[["clay"]] <- 25
changed_parms[["In"]] <- 5
changed_parms[["ks"]]["k.HUM"] <- 0.03
changed_parms[["ks"]]["k.RPM"] <- 0.2

carbon <- getC(do.call(RothCModel, changed_parms))

for(i in 1:5){
  if(i == 1){
    plot(time, carbon[,i],
         col = i, ylim = range(carbon), type = "l",
         xlab = "Years", ylab = "Carbon in [Mg/ha]")
    legend(x = 200, y = 15, legend = c("DPM", "RPM", "BIO", "HUM", "IOM"), fill = 1:5)
  } 
  if(i > 1) lines(time, carbon[,i], col = i)
}



get_output <- function(model_parms = default_parms){
  
  model <- do.call(RothCModel, model_parms)
  carbon <- getC(model)
  # return the sum of the average amount of carbon in each pool during the last
  # 12 months of the simulation.
  return(sum(colMeans(tail(carbon, 12))))
}


change_parms <- function(par_value, par_name, par_default = default_parms){
  # browser()
  ind_ks <- grep(pattern = "^k\\.", x = par_name)
  ind_rest <- grep(pattern = "^k\\.", x = par_name, invert = TRUE)
  
  par_default[["ks"]][par_name[ind_ks]] <- par_value[ind_ks]
  par_default[par_name[ind_rest]] <- par_value[ind_rest]
  
  output <- get_output(par_default)
  return(output)
}

sensitivity <- function(pools, pars, measure){
  if(measure == "abs")
    sens <- diff(pools) / diff(pars)
  
  if(measure == "rel")
    sens <- (diff(pools) / head(pools, -1)) / (diff(pars) / head(pars, -1))
  
  return(sens)
}



stocks <- sapply(seq(5,15, length.out = 20), change_parms, par_name = "k.DPM")
sensitivity(stocks, seq(5,15, length.out = 20), "rel")
plot(x = head(seq(5,15, length.out = 20),-1), y = sensitivity(stocks, seq(5,15, length.out = 20), "rel"))
plot(x = seq(5,15, length.out = 20), y = stocks)


stocks <- sapply(seq(5,15, length.out = 20), change_parms, par_name = "k.DPM")

morris_fun <- function(mt, par_name, par_default = default_parms){
  result <- apply(mt, 1, change_parms, par_name = par_name, par_default = par_default)
  return(result)
}

names_pars <- c("k.DPM", "k.RPM", "k.BIO", "k.HUM")
lower <- c(5, 0.15, 0.33, 0.01)
upper <- c(15, 0.45, 0.99, 0.03)

morris_output <- morris(morris_fun, factors = names_pars, r = 200,
                        design = list(type = "oat", levels = 8, grid.jump = 4),
                        binf = lower, bsup = upper, par_name = names_pars)

plot(morris_output)
saveRDS(morris_output, "data/morris_k_rates.rds")













