# ============================================
# MES exercise 0727 rice methane
# ============================================

# ---------------------------------------------
library(dplyr) ; library(lubridate) 
library(ggplot2)

# read in data
soil <- read.table("/Volumes/GoogleDrive/我的雲端硬碟/ich/Documents/2020/【ACA】Modelling Environmental Systems/VM/PH_losbanos-icon-wetrice-con_soilchemistry-daily default.txt",
                   header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# get columns of the GHG emissions
cols <- grep("ch4_emis|co2_emis|n2o_emis", names(soil))

# factors to convert elementary to molecular weight
co2_fac <- (12+32)/12
ch4_fac <- (12+4)/12
n2o_fac <- (28+16)/28

# sum up daily to yearly values, convert weights and calculate the global
# warming potential
soil_y <- soil %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime)) %>% 
  group_by(year) %>% 
  summarise_at(cols, sum) %>% 
  ungroup() %>% 
  transmute(year = year,
            gwp_co2 = (dC_co2_emis_auto.kgCha.1. + dC_co2_emis_hetero.kgCha.1.) * co2_fac,
            gwp_ch4 = dC_ch4_emis.kgCha.1. * ch4_fac * 86,
            gwp_n2o = dN_n2o_emis.kgNha.1. * n2o_fac * 268)


tidyr::pivot_longer(soil_y, 2:4) %>% ggplot(aes(x = year, y = value, fill = name)) +
  geom_bar(stat="identity")





# --------------------------------------------------------
# the output of the simulation without fertilization

soil_noFert <- read.table("/Volumes/GoogleDrive/我的雲端硬碟/ich/Documents/2020/【ACA】Modelling Environmental Systems/VM/PH_losbanos-icon-wetrice-con_soilchemistry-daily noFert.txt",
                           header = TRUE, sep = "\t", stringsAsFactors = FALSE)
soil_noFert_y <- soil_noFert %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime)) %>% 
  group_by(year) %>% 
  summarise_at(cols, sum) %>% 
  ungroup() %>% 
  transmute(year = year,
            gwp_co2 = (dC_co2_emis_auto.kgCha.1. + dC_co2_emis_hetero.kgCha.1.) * co2_fac,
            gwp_ch4 = dC_ch4_emis.kgCha.1. * ch4_fac * 86,
            gwp_n2o = dN_n2o_emis.kgNha.1. * n2o_fac * 268)

data.frame(rbind(soil_y , soil_noFert_y) , 
           scenario = c(rep("default" , nrow(soil_y)) , rep("noFert" , nrow(soil_noFert_y)))) %>% 
  tidyr::pivot_longer(2:4) %>% 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ scenario)


