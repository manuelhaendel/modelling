library(tidyverse)
library(lubridate)

setwd("C:/Users/Manuel/Documents/Studium/MSc_Umweltwissenschaften/model_env_sys/VM_share")

# Task 1
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



# Task 2
soil_w <- read.table("PH_losbanos-icon-wetrice_output/PH_losbanos-icon-wetrice-con_soilchemistry-yearly.txt",
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)

soil_d <- read.table("PH_losbanos-icon-mixedrice_output/PH_losbanos-icon-mixedrice-con_soilchemistry-yearly.txt",
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)

soil_m <- read.table("PH_losbanos-icon-mixedmaize_output/PH_losbanos-icon-mixedmaize-con_soilchemistry-yearly.txt",
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)

harvest_w <- read.table("PH_losbanos-icon-wetrice_output/PH_losbanos-icon-wetrice-con_report-harvest.txt",
                        header = TRUE, sep = "\t", stringsAsFactors = FALSE)

harvest_d <- read.table("PH_losbanos-icon-mixedrice_output/PH_losbanos-icon-mixedrice-con_report-harvest.txt",
                        header = TRUE, sep = "\t", stringsAsFactors = FALSE)

harvest_m <- read.table("PH_losbanos-icon-mixedmaize_output/PH_losbanos-icon-mixedmaize-con_report-harvest.txt",
                        header = TRUE, sep = "\t", stringsAsFactors = FALSE)



soil_w <- soil_w %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime))

soil_d <- soil_d %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime))

soil_m <- soil_m %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime))

harvest_w <- harvest_w %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime))

harvest_d <- harvest_d %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime))

harvest_m <- harvest_m %>% 
  mutate_at("datetime", lubridate::ymd_hms) %>% 
  mutate(year = year(datetime))


n_cols <- grep("aN", names(soil_w), ignore.case = FALSE, value = TRUE)
n_cols <- grep("litter|mineral|nitrify|chemo|uptake|change", n_cols, value = TRUE, invert = TRUE)


soil_w <- select(soil_w, c(n_cols, ncol(soil_w))) %>%
  rename_at(1:length(n_cols), function(name) sub(pattern = ".*?_(.*)\\..*\\..*\\..*", replacement = "\\1", name))
  
soil_d <- select(soil_d, c(n_cols, ncol(soil_d))) %>%
  rename_at(1:length(n_cols), function(name) sub(pattern = ".*?_(.*)\\..*\\..*\\..*", replacement = "\\1", name))

soil_m <- select(soil_m, c(n_cols, ncol(soil_m))) %>%
  rename_at(1:length(n_cols), function(name) sub(pattern = ".*?_(.*)\\..*\\..*\\..*", replacement = "\\1", name))


harvest_w <- harvest_w %>% select(c(13,15,18)) %>% 
  transmute(year = year,
            n_harvest = dN_bud_export.kgNha.1. + dN_straw_export.kgNha.1.) %>% 
  group_by(year) %>% 
  summarise(n_harvest = sum(n_harvest))

harvest_d <- harvest_d %>% select(c(13,15,18)) %>% 
  transmute(year = year,
            n_harvest = dN_bud_export.kgNha.1. + dN_straw_export.kgNha.1.) %>% 
  group_by(year) %>% 
  summarise(n_harvest = sum(n_harvest))

harvest_m <- harvest_m %>% select(c(13,15,18)) %>% 
  transmute(year = year,
            n_harvest = dN_bud_export.kgNha.1. + dN_straw_export.kgNha.1.) %>% 
  group_by(year) %>% 
  summarise(n_harvest = sum(n_harvest))

balance_w <- inner_join(harvest_w, soil_w, by = "year") %>% 
  mutate_at(2:9, function(value) -value) %>% 
  mutate(balance = rowSums(.[2:13])) %>% 
  filter(year %in% 2012:2017) %>% 
  summarise_all(mean) %>% 
  mutate(year = "wetrice")%>%
  rename(system =  year)

balance_d <- inner_join(harvest_d, soil_d, by = "year") %>% 
  mutate_at(2:9, function(value) -value) %>% 
  mutate(balance = rowSums(.[2:13])) %>% 
  filter(year %in% 2012:2017) %>% 
  summarise_all(mean) %>% 
  mutate(year = "dryrice") %>%
  rename(system =  year)

balance_m <- inner_join(harvest_m, soil_m, by = "year") %>% 
  mutate_at(2:9, function(value) -value) %>% 
  mutate(balance = rowSums(.[2:13])) %>% 
  filter(year %in% 2012:2017) %>% 
  summarise_all(mean) %>% 
  mutate(year = "mixedmaize")%>%
  rename(system =  year)

balance <- rbind(balance_w, balance_d, balance_m)

balance %>% pivot_longer(2:14) %>%
  ggplot() +
  geom_bar(aes(x = name, y = value, fill = name), stat = "identity") +
  labs(x = "", y = "Nitrogen flux [kgN/ha]") +
  theme(axis.text.x = element_blank()) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  facet_wrap(vars(system))







