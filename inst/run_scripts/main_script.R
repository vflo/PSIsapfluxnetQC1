# Main script

library(psiQC)
library(tidyverse)


# setup logs
log_psi_setup('Logs/psi.log',
                     logger = 'DataFlow',
                     level = 'DEBUG')

# reports for data in the system
rep_psi_render('received_to_accepted.Rmd',
               output_file = file.path(
                 'Reports', paste(format(Sys.time(), '%Y%m%d%H%M'),
                                  'received_to_accepted.html', sep = '_')
               ),
               output_dir = 'Reports',
               parent_logger = 'DataFlow')

# QC
log_psi_setup('Logs/psi.log', logger = 'QC', level = "DEBUG")

data_folders <- df_get_data_folders(parent_logger = 'QC')

## Loop for every site
lapply(data_folders[[1]], function(folder) {
  code <- stringr::str_sub(folder, 6, -1)
  # log_psi_setup('Logs/psi.log',
  #                      logger = paste('QC', code, sep = '.'),
  #                      level = "DEBUG")
  qc_start_process_psi(file.path(folder, 'Accepted'), rdata = FALSE,
                   parent_logger = paste('QC', code, sep = '.'))
})

## Site prepared for level 2
df_set_status_psi(data_folders[[1]] %>% stringr::str_sub( 6, -1),
                  LVL1 = list(TO_LVL2 = "READY"))

## Reset status of a site (RUN ONLY when changes are applied to received data)
# df_reset_data_status_psi(data_folders[[9]] %>% stringr::str_sub( 6, -1))

# ################################################################################
# # LEVEL 2
#
# log_psi_setup('Logs/psi.log', logger = 'LEVEL2', level = "DEBUG")
#
# # df_flag_to_lvl2_app()
#
# df_lvl1_to_lvl2(parent_logger = 'LEVEL2')
#
# ########
#
# # out_app()
#
# df_warn_to_rem()
#
# # out_confirmation_app()
#
# df_rem_to_units()
