library(SoilR)
library(sensitivity)
library(dplyr)
library(lubridate)
library(raster)

setwd("C:/Users/Manuel/Documents/Studium/MSc_Umweltwissenschaften/model_env_sys/project")


# read climate data
## source: WorldClim version 2.1 climate data for 1970-2000. Spatial resolution
## is 2.5 minutes (~21 km2).
paths <- list.files(path = "data/climate_current", pattern = "tavg.*\\.tif$", full.names = TRUE)
extract_freiburg <- function(path) extract(raster(path), matrix(c(7.8, 48), ncol=2))
temp <- sapply(paths, extract_freiburg)

paths <- list.files(path = "data/climate_current", pattern = "prec.*\\.tif$", full.names = TRUE)
prec <- sapply(paths, extract_freiburg)

data <- data.frame(month = 1:12,
                   temp = temp,
                   prec = prec)
rownames(data) <- NULL

paths_prec <- list.files(path = "data/climate_future", pattern = "prec.*\\.tif$", full.names = TRUE, recursive = TRUE)
extract_freiburg <- function(path) extract(brick(path), matrix(c(7.8, 48), ncol=2))
paths_126 <- grep("ssp126", paths_prec, value = TRUE)
prec_fut_126 <- sapply(paths_126, extract_freiburg)
prec_fut_126 <- rowMeans(prec_fut_126)
paths_585 <- grep("ssp585", paths_prec, value = TRUE)
prec_fut_585 <- sapply(paths_585, extract_freiburg)
prec_fut_585 <- rowMeans(prec_fut_585)

paths_tmax <- list.files(path = "data/climate_future", pattern = "tmax.*\\.tif$", full.names = TRUE, recursive = TRUE)
extract_freiburg <- function(path) extract(brick(path), matrix(c(7.8, 48), ncol=2))
tmax_fut_585 <- sapply(paths_tmax, extract_freiburg)
tmax_fut_585 <- rowMeans(tmax_fut_585)

paths_tmin <- list.files(path = "data/climate_future", pattern = "tmin.*\\.tif$", full.names = TRUE, recursive = TRUE)
extract_freiburg <- function(path) extract(brick(path), matrix(c(7.8, 48), ncol=2))
tmin_fut_585 <- sapply(paths_tmin, extract_freiburg)
tmin_fut_585 <- rowMeans(tmin_fut_585)

tavg_fut_585 <- colMeans(rbind(tmax_fut_585, tmin_fut_585))


png("figures/env_comp.png", width = 20, height = 10, units = "cm", res = 200)
par(mfrow = c(1,2))
# temperature scenario comparison
plot(data$temp, type="l", ylim = range(c(data$temp, tavg_fut_585)), xaxt = "n",
     xlab = "Month", ylab = "Temperature [°C]", main = "a")
axis(1, 1:12, labels = as.character(lubridate::month(1:12, label = TRUE)))
points(data$temp, pch = 20)
lines(tavg_fut_585, col="red")
points(tavg_fut_585, col="red", pch=20)


# precipitation scenario comparison
plot(data$prec, type="l", ylim = range(c(data$prec, prec_fut_126, prec_fut_585)), xaxt = "n",
     xlab = "Month", ylab = "Precipitation [mm]", main = "b")
axis(1, 1:12, labels = as.character(lubridate::month(1:12, label = TRUE)))
points(data$prec, pch = 20)
lines(prec_fut_585, col="red")
points(prec_fut_585, col="red", pch=20)
dev.off()
par(mfrow = c(1,1))



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

default_parms <- list(ks = c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66,
                             k.HUM = 0.02, k.IOM = 0),
                      In = 2.7, clay = 48, xi = ext_fx_frame,
                      t = time, C0 = init, DR = 1.44)

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


# return model output (average total C of last 12 months of model run)
get_output <- function(model_parms = default_parms){

  model <- do.call(RothCModel, model_parms[])
  carbon <- getC(model)
  
  return(sum(colMeans(tail(carbon, 12))))
}


# update external effects table
update_extfx <- function(par_value, par_name, env_data, time){
  
  for(i in seq_along(par_name)){
    if(par_name[i] == "prec_season"){
      env_data["prec"] <- (env_data["prec"]
                                + c(1,1,1,1,-1,-1,-1,-1,-1,1,1,1) * par_value[i])
    }
    else
      env_data[par_name[i]] <- env_data[par_name[i]] + par_value[i]
  }
  
  #  rate modifying factor for moisture 
  moist_fact <- fW.RothC(P = env_data$prec, E = Evp$Evp, S.Thick = soil, pE = 1, bare = FALSE)[,2]
  
  #  rate modifying factor for temperature
  temp_fact <- fT.RothC(env_data$temp)
  
  # external (environmental and/or edaphic) effects on decomposition rates
  ext_fx <- moist_fact * temp_fact
  
  # data frame with the external effects for each time step
  ext_fx_frame <- data.frame(t = time, fx = rep_len(ext_fx, length.out = length(time)))
  
  return(ext_fx_frame)
}

# change individual parameter values and return model output
change_parms <- function(par_value, par_name, env_data = data, par_list = default_parms){
  
  # browser()
  
  # different parameters need to be treated differently, get indeces of the
  # three different parameter groups (climate data, k-rates and rest)
  ind_env <- grep(pattern = "temp|prec", x = par_name)
  ind_ks <- grep(pattern = "^k\\.", x = par_name)
  ind_rest <- which(!(seq_along(par_name) %in% c(ind_env, ind_ks)))
  # grep(pattern = "^k\\.", x = par_name, invert = TRUE)
    
  # changes in climate data input
  if(length(ind_env) != 0)
    par_list[["xi"]] <- update_extfx(par_value, par_name, env_data = env_data, time = par_list[["t"]])
  
  # changes in model parameter set
  if(length(c(ind_ks, ind_rest)) != 0){
    par_list[["ks"]][par_name[ind_ks]] <- par_value[ind_ks]
    par_list[par_name[ind_rest]] <- par_value[ind_rest]
  }
  
  output <- get_output(par_list)
  return(output)
}

# return sensitivity scores
sensitivity <- function(pools, pars, measure){
  if(measure == "abs")
    sens <- diff(pools) / diff(pars)
  
  if(measure == "rel")
    sens <- (diff(pools) / head(pools, -1)) / (diff(pars) / head(pars, -1))
  
  return(sens)
}



par_seq_temp <- seq(0,8, length.out = 20)
stocks_temp <- sapply(par_seq_temp, change_parms, par_name = "temp")
sens_temp <- sensitivity(stocks_temp, par_seq_temp, "abs")

par_seq_prec <- seq(-15,15, length.out = 20)
stocks_prec <- sapply(par_seq_prec, change_parms, par_name = "prec")

par_seq_prec_s <- seq(0,15, length.out = 20)
stocks_prec_s <- sapply(par_seq_prec_s, change_parms, par_name = "prec_season")
sens_prec_s <- sensitivity(stocks_prec_s, par_seq_prec_s, "abs")

png("figures/sens_inputs.png", width = 20, height = 20, units = "cm", res = 200)
par(mfrow = c(2,2))

plot(x = par_seq_temp, y = stocks_temp,  main = "a", pch = 20,
     xlab = "Temperature [K]", ylab = "Carbon stock [Mg/ha]")
plot(x = head(par_seq_temp,-1), y = sens_temp, main = "b", pch = 20,
     xlab = "Temperature [K]", ylab = "Sensitivity")

plot(x = par_seq_prec_s, y = stocks_prec_s,  main = "c", pch = 20,
     xlab = "Precipitation [mm]", ylab = "Carbon stock [Mg/ha]")
plot(x = head(par_seq_prec_s,-1), y = sens_prec_s,  main = "d", pch = 20,
     xlab = "Precipitation [mm]", ylab = "Sensitivity")

dev.off()

par_seq_rpm <- seq(0.15,0.45, length.out = 20)
stocks_rpm <- sapply(par_seq_rpm, change_parms, par_name = "k.RPM")
sens_rpm <- sensitivity(stocks_rpm, par_seq_rpm, "abs")

par_seq_hum <- seq(0.01,0.03, length.out = 20)
stocks_hum <- sapply(par_seq_hum, change_parms, par_name = "k.HUM")
sens_hum <- sensitivity(stocks_hum, par_seq_hum, "abs")

par_seq_In <- seq(1.35,4.05, length.out = 20)
stocks_In <- sapply(par_seq_In, change_parms, par_name = "In")
sens_In <- sensitivity(stocks_In, par_seq_In, "abs")

png("figures/sens_parms.png", width = 20, height = 20, units = "cm", res = 200)
par(mfrow = c(2,2))

plot(x = par_seq_In, y = stocks_In,  main = "a", pch = 20,
     xlab = "Carbon input [Mg/ha]", ylab = "Carbon stock [Mg/ha]")
plot(x = head(par_seq_In,-1), y = sens_In, main = "b", pch = 20, ylim = c(6.5,7.5),
     xlab = "Carbon input [Mg/ha]", ylab = "Sensitivity")

plot(x = par_seq_hum, y = stocks_hum,  main = "c", pch = 20,
     xlab = "Decomposition rate [MgC/(ha*yr)]", ylab = "Carbon stock [Mg/ha]")
plot(x = head(par_seq_hum,-1), y = sens_hum,  main = "d", pch = 20,
     xlab = "Decomposition rate [MgC/(ha*yr)]", ylab = "Sensitivity")

# plot(x = par_seq_rpm, y = stocks_rpm,  main = "e", pch = 20,
#      xlab = "Decomposition rate [MgC/(ha*yr)]", ylab = "Carbon stock [Mg/ha]")
# plot(x = head(par_seq_rpm,-1), y = sens_rpm,  main = "f", pch = 20,
#      xlab = "Decomposition rate [MgC/(ha*yr)]", ylab = "Sensitivity")

dev.off()
par(mfrow=c(1,1))

# return model output for every row of the morris-function-matrix
morris_fun <- function(mt, par_name, par_list = default_parms){
  result <- apply(mt, 1, change_parms, par_name = par_name, par_list = par_list)
  return(result)
}

names_pars <- c("temp", "prec_season")
lower <- c(0, 0)
upper <- c(2, 15)

morris_output_inputs <- morris(morris_fun, factors = names_pars, r = 20,
                        design = list(type = "oat", levels = 8, grid.jump = 4),
                        binf = lower, bsup = upper, par_name = names_pars)
mu <- colMeans(abs(morris_output_inputs$ee))
sigma <- apply(morris_output_inputs$ee, 2, sd)

plot(mu, sigma, pch = 20, xlab = "µ*", ylab = expression(sigma))
text(mu + c(-0.1, 0.1), sigma, labels = c("temp", "prcp"))

# names_pars <- c("temp", "prec")
# lower <- c(0, -15)
# upper <- c(2, 15)

names_pars <- c("k.DPM", "k.RPM", "k.BIO", "k.HUM", "In", "clay")
lower <- c(5, 0.15, 0.33, 0.01, 1.35, 24)
upper <- c(15, 0.45, 0.99, 0.03, 4.05, 72)

morris_output_parameters <- morris(morris_fun, factors = names_pars, r = 20,
                        design = list(type = "oat", levels = 8, grid.jump = 4),
                        binf = lower, bsup = upper, par_name = names_pars)

png("figures/morris.png", width = 30, height = 15, units = "cm", res = 200)
par(mfrow=c(1,2))
mu <- colMeans(abs(morris_output_inputs$ee))
sigma <- apply(morris_output_inputs$ee, 2, sd)

plot(mu, sigma, pch = 20, xlab = "µ*", ylab = expression(sigma), main = "a")
text(mu + c(-0.15, 0.15), sigma, labels = c("temp", "prcp"))

mu <- colMeans(abs(morris_output_parameters$ee))
sigma <- apply(morris_output_parameters$ee, 2, sd)

plot(mu, sigma, pch = 20, xlab = "µ*", ylab = expression(sigma), main = "b")
text(mu + c(1.4, 1.4, 1.4, -1.4, -0.7, 1), sigma + c(0,0,0.15,0,0,0), labels = names_pars)
dev.off()
par(mfrow = c(1,1))

plot(morris_output_parameters)

saveRDS(morris_output, "data/morris_k_rates_In_clay.rds")
morris_output <- readRDS("data/morris_k_rates.rds")












