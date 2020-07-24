library(tidyverse)


# create soil profile
string <- "1 5.0 1.0 - - - - - - aeAh1
2 16.0 1.5 58.5 6.4 5.81 0.67 0.82 7.1 aeAh2
3 37.0 0.0 59.2 4.9 2.04 0.25 1.17 7.6 aeM
4 46.0 0.0 55.2 3.5 1.02 0.12 1.35 7.5 Sw-aeM1
5 53.0 0.0 60.5 1.0 1.60 0.18 1.19 7.4 Sw-aeM1
6 72.0 0.0 57.2 3.1 0.71 0.08 1.38 7.5 IIaelC1
7 86.0 0.0 55.6 7.0 0.49 0.06 1.44 7.6 aelC2
8 109.0 0.0 53.0 3.7 0.40 0.05 1.48 7.6 aelC3
9 124.0 0.0 54.0 4.0 0.37 0.03 1.50 7.6 aelC-Swd
10 143.0 0.0 54.3 2.5 0.39 0.04 1.54 7.7 aeSd
11 155.0 0.0 63.4 1.6 1.89 0.15 1.10 7.5 IIIaeolC-Sd1
12 162.0 0.0 - - - - - - aeolC-Sd2"

string <- gsub("\n", " ", string)
string <- gsub("-", "NA", string)
string <- strsplit(string, " ")[[1]]

layer <- string[seq(1,length(string), by=10)]


profile <- data.frame(layer = string[seq(1,length(string), by=10)],
                      lb = string[seq(2,length(string), by=10)],
                      sc = string[seq(3,length(string), by=10)],
                      clay = string[seq(4,length(string), by=10)],
                      sand = string[seq(5,length(string), by=10)],
                      c_org = string[seq(6,length(string), by=10)],
                      n_tot = string[seq(7,length(string), by=10)],
                      rho = string[seq(8,length(string), by=10)],
                      ph = string[seq(9,length(string), by=10)],
                      name = string[seq(10,length(string), by=10)],
                      stringsAsFactors = FALSE
                      )

profile <- profile %>% 
  mutate_at(1, as.integer) %>% 
  mutate_at(2:9, as.numeric) 
  

# calculate soil hydrolic conductivity
## after cosby
shc_cosby <- function(sand, clay) return(60.96 * 10^(-0.6 + 0.0126*sand - 0.0064*clay))

profile <- profile %>% 
  mutate(shc = shc_cosby(sand, clay))

# calculate field capacity and wilting point
## get van Genuchten parameters
get_vg <- function(clay, sand, c_org, rho){
  phi_res <-  0.015 + 0.005*clay + 0.014*c_org
  phi_sat <-  0.81 - 0.283*rho + 0.001*clay
  alpha <-  exp(-2.486 + 0.025*sand - 0.351*c_org - 2.617*rho - 0.023*clay)
  n <-  exp(0.053 - 0.009*sand - 0.013*clay + 0.00015*sand^2)
  m <- 1
  return(list(phi_res = phi_res, phi_sat = phi_sat, alpha = alpha, n = n, m = m))
}

## function for soil water content
swc <- function(clay, sand, c_org, rho, p_c){
  vg_pars <- get_vg(clay, sand, c_org, rho)
  swc <- (vg_pars$phi_res + (vg_pars$phi_sat - vg_pars$phi_res) /
            (1 + (p_c * vg_pars$alpha)^vg_pars$n)^vg_pars$m)
  return(swc)
}

profile <- profile %>% 
  mutate(field_cap = swc(clay, sand, c_org, rho, 100),
         wilt_point = swc(clay, sand, c_org, rho, 15000))




























