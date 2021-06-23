# Main script

library(psiQC)
library(tidyverse)

################################################################################
# # LEVEL 1

lvl1_process()

## Manually set site prepared for level 2

data_folders <- df_get_data_folders(parent_logger = 'QC')

df_set_status_psi(data_folders[[12]] %>% stringr::str_sub( 6, -1),
                  LVL1 = list(TO_LVL2 = "READY"))

## Reset status of a site (RUN ONLY when changes are applied to received data)
#df_reset_data_status_psi(data_folders[[1]] %>% stringr::str_sub( 6, -1))

################################################################################
# # LEVEL 2
#
#create a db folder with all the individual datasets

lvl2_process()






################################################################################
## CREATE A JOINED DATA SET

folder <- "~/psi_db/0.0.1/RData/"

psi_files <- list.files(folder) %>% gsub(".RData","",x=.)

purrr::map(
  as.list(psi_files),
  function(.x){
    read_psi_data(.x, folder = folder) %>%
      psi_tidyfier()
  }
) %>% bind_rows() -> PSI_DF
