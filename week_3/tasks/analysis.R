library(dplyr)
setwd("C:/Users/Manuel/Documents/Studium/MSc_Umweltwissenschaften/model_env_sys/week_3/tasks/task1")


get_table <- function(path){
  scenario <- sub(pattern = ".*Ltafel_(.*)_\\d{1}\\.txt", x = path,  replacement = "\\1")
  table <- as.tbl(read.table(path, header = TRUE, sep = "\t", skip = 3, stringsAsFactors = FALSE)) %>%
    filter(Age == 102) %>%
    select(Age, d100 = d100..v., mai = dgz, tvp = GWL, ) %>% 
    mutate(scenario = scenario)
}


pines <- bind_rows(lapply(list.files(path = getwd(), pattern = "\\.txt$", full.names = TRUE),
                                     get_table))

pines <- group_by(pines, scenario) %>% 
  summarise_all(mean)

# no difference between light and moderate curve in f-tree thinning, removing
# moderate scenario
pines <- filter(pines, scenario != "ftree_300_4_moderate")

# below > above
# light > moderate (tvp)
# moderate > light (d100)
# all > ftree (tvp)
# ftree > all (d100)