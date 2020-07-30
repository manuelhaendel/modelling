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
moist_fact <- fW.RothC(P = data$prec, E = Evp$Evp, S.Thick = soil, pE = 1, bare = FALSE)[,2]

##  rate modifying factor for temperature
temp_fact <- fT.RothC(data$temp)

## external (environmental and/or edaphic) effects on decomposition rates
ext_fx <- moist_fact * temp_fact

## time steps
time <- seq(1/12, 400, 1/12)

## data frame with the external effects for each time step
ext_fx_frame <- data.frame(t = time, fx = rep_len(ext_fx, length.out = length(time)))

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
time <- seq(1/12, 100, 1/12)
ext_fx_frame <- data.frame(t = time, fx = rep_len(ext_fx, length.out = length(time)))

default_parms <- list(t = time,
                      ks = c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66,
                             k.HUM = 0.02, k.IOM = 0),
                      C0 = init, In = 2.7, DR = 1.44, clay = 48,
                      xi = ext_fx_frame)

# changed_parms <- default_parms
# changed_parms[["clay"]] <- 25
# changed_parms[["In"]] <- 5
# changed_parms[["ks"]]["k.HUM"] <- 0.03
# changed_parms[["ks"]]["k.RPM"] <- 0.2
# 
# carbon <- getC(do.call(RothCModel, changed_parms))
# 
# for(i in 1:5){
#   if(i == 1){
#     plot(time, carbon[,i],
#          col = i, ylim = range(carbon), type = "l",
#          xlab = "Years", ylab = "Carbon in [Mg/ha]")
#     legend(x = 200, y = 15, legend = c("DPM", "RPM", "BIO", "HUM", "IOM"), fill = 1:5)
#   } 
#   if(i > 1) lines(time, carbon[,i], col = i)
# }



get_output <- function(model_parms = default_parms){
  
  model <- do.call(RothCModel, model_parms)
  carbon <- getC(model)
  # return the sum of the average amount of carbon in each pool during the last
  # 12 months of the simulation.
  return(sum(colMeans(tail(carbon, 12))))
}


change_parms <- function(par_value, par_name, env_data = data, par_list = default_parms){
  
  # browser()
    
  # change climate data
  for(i in seq_along(par_name)){
    env_data[par_name[i]] <- env_data[par_name[i]] + par_value[i]
  }
  
  #  rate modifying factor for moisture 
  moist_fact <- fW.RothC(P = env_data$prec, E = Evp$Evp, S.Thick = soil, pE = 1, bare = FALSE)[,2]
  
  #  rate modifying factor for temperature
  temp_fact <- fT.RothC(env_data$temp)
  
  # external (environmental and/or edaphic) effects on decomposition rates
  ext_fx <- moist_fact * temp_fact
  
  # time steps
  time <- par_list[["t"]]
  
  # data frame with the external effects for each time step
  ext_fx_frame <- data.frame(t = time, fx = rep_len(ext_fx, length.out = length(time)))
  
  par_list[["xi"]] <- ext_fx_frame
  
  output <- get_output(par_list)
  return(output)
}

sensitivity <- function(pools, pars, measure){
  if(measure == "abs")
    sens <- diff(pools) / diff(pars)
  
  if(measure == "rel")
    sens <- (diff(pools) / head(pools, -1)) / (diff(pars) / head(pars, -1))
  
  return(sens)
}


par_seq <- seq(0,2, length.out = 20)

stocks <- sapply(par_seq, change_parms, par_name = "temp")
sens <- sensitivity(stocks, par_seq, "rel")
plot(x = head(par_seq,-1), y = sens, main = "temp")
plot(x = par_seq, y = stocks)

par_seq <- seq(-15,15, length.out = 20)

stocks <- sapply(par_seq, change_parms, par_name = "prec")
sens <- sensitivity(stocks, par_seq, "rel")
plot(x = head(par_seq,-1), y = sens, main = "prec")
plot(x = par_seq, y = stocks)



morris_fun <- function(mt, par_name, par_list = default_parms){
  result <- apply(mt, 1, change_parms, par_name = par_name, par_list = par_list)
  return(result)
}

names_pars <- c("temp", "prec")
lower <- c(0, -15)
upper <- c(2, 15)

morris_output <- morris(morris_fun, factors = names_pars, r = 20,
                        design = list(type = "oat", levels = 8, grid.jump = 4),
                        binf = lower, bsup = upper, par_name = names_pars)

# saveRDS(morris_output, "data/morris_k_rates.rds")
morris_output <- readRDS("data/morris_k_rates.rds")

plot(morris_output)










