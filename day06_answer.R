library(tidyverse)
library(lubridate)

setwd("C:/Users/Manuel/Documents/Studium/MSc_Umweltwissenschaften/model_env_sys/VM_share/PH_losbanos-icon-wetrice_output/")

# read in data
soil <- read.table("PH_losbanos-icon-wetrice-con_soilchemistry-daily.txt",
                   header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# get columns of the ghg emissions
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


pivot_longer(soil_y, 2:4) %>% ggplot(aes(x = year, y = value, fill = name)) +
  geom_bar(stat="identity")

con <- file("C:/Users/Manuel/Documents/Studium/MSc_Umweltwissenschaften/model_env_sys/VM_share/PH_losbanos-icon-wetrice-con_mana.xml")

events <- readLines(con)

close(con)


open <- grep("<event", events)
close <- grep("</event>", events)
fert <- grep(pattern = "fertilize", events)

get_lines <- function(line){
  start <- open[tail(which(open <= line), 1)]
  end <- close[head(which(close >= line), 1)]
  return(c(start, end))
}

lapply(fert, get_lines)

