library(tidyverse)
library(lubridate)

setwd("C:/Users/Manuel/Documents/Studium/MSc_Umweltwissenschaften/model_env_sys/VM_share")

wetrice <- read.table("PH_losbanos-icon-wetrice_output/PH_losbanos-icon-wetrice-con_physiology-daily.txt",
                      header = TRUE, sep = "\t", stringsAsFactors = FALSE)

dryrice <- read.table("PH_losbanos-icon-mixedrice_output/PH_losbanos-icon-mixedrice-con_physiology-daily.txt",
                      header = TRUE, sep = "\t", stringsAsFactors = FALSE)

mixedmaize <- read.table("PH_losbanos-icon-mixedmaize_output/PH_losbanos-icon-mixedmaize-con_physiology-daily.txt",
                         header = TRUE, sep = "\t", stringsAsFactors = FALSE)

wetrice <- wetrice %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime))

dryrice <- dryrice %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime))

mixedmaize <- mixedmaize %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime))

group_by(wetrice, year) %>% 
  count()

group_by(dryrice, year) %>% 
  count()

group_by(mixedmaize, year) %>% 
  count()


w <- wetrice %>% ggplot() +
  geom_line(aes(x = datetime, y = DW_below.kgDWm.2. + DW_above.kgDWm.2.)) +
  ggtitle("Wetrice") +
  labs(x = "Date", y = "Dry Weight of Biomass [kg/m^2]")
d<- dryrice %>% ggplot() +
  geom_line(aes(x = datetime, y = DW_below.kgDWm.2. + DW_above.kgDWm.2.)) +
  ggtitle("Dryrice") +
  labs(x = "Date", y = "Dry Weight of Biomass [kg/m^2]")
m <- mixedmaize %>% ggplot() +
  geom_line(aes(x = datetime, y = DW_below.kgDWm.2. + DW_above.kgDWm.2.)) +
  ggtitle("Mixedmaize") +
  labs(x = "Date", y = "Dry Weight of Biomass [kg/m^2]")

gridExtra::grid.arrange(w,d,m)
