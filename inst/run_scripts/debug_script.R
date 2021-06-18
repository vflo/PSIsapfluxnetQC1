# Libraries
library(psiQC)
library(tidyverse)
library(DT)

params <- list(
  wd = '../',
  md_file = 'Data/ESP_GUA_VAL/Accepted/ESP_GUA_VAL.xlsx',
  psi_data_file = 'Data/ESP_GUA_VAL/Accepted/ESP_GUA_VAL.xlsx',
  code = 'ESP_GUA_VAL'
)
# 
# params <- list(
#   wd = '../',
#   md_file = 'Data/ARG_TRE/Accepted/ARG_TRE.xlsx',
#   psi_data_file = 'Data/ARG_TRE/Accepted/ARG_TRE.xlsx',
#   code = 'ARG_TRE'
# )



logger_name <- 'test'

# Data load
## site_md
site_md <- dl_metadata(params$md_file, 'Data', data_type = "site_md",
                       parent_logger = logger_name)

## plant_md
plant_md <- dl_metadata(params$md_file, 'Data', data_type = "plant_md",
                        si_code_loc = site_md, parent_logger = logger_name)

## psi_data
psi_data <- dl_metadata(params$psi_data_file, 'Data', data_type = "psi_data",
                        si_code_loc = site_md, parent_logger = logger_name)

## questionnaire
questionnaire_md <- dl_metadata(params$md_file, 'Questionnaire',
                                si_code_loc = site_md,
                                parent_logger = logger_name)

################################################################################

#Simplify questions of the questionnaire
questionnaire_simplified <- qc_simplify_questions(questionnaire_md)

################################################################################

# md qc

## metadata columns
md_cols <- bind_rows(
  qc_md_cols(site_md, 'site_md', parent_logger = logger_name),
  qc_md_cols(plant_md, 'plant_md', parent_logger = logger_name),
  qc_md_cols(psi_data, 'psi_data', parent_logger = logger_name),
  qc_md_cols(questionnaire_simplified, 'Questionnaire', parent_logger = logger_name)
)

## factor variables values
factor_values <- qc_factor_values(site_md, plant_md, psi_data,
                                  parent_logger = logger_name)

## email
email_check <- qc_email_check(site_md, parent_logger = logger_name) %>% unique()

## coordinates
site_md_coordfix <- qc_coordinates(site_md, parent_logger = logger_name) %>% unique()

## species
plant_md_spnames <- qc_species_names_info(
  plant_md$pl_species,
  parent_logger = logger_name
) %>%
  mutate(Md = 'pl')

plant_md$pl_species <- qc_species_names(plant_md$pl_species,
                                        parent_logger = logger_name)

plant_md <- qc_measured_sfn(plant_md, parent_logger = logger_name)

## plant treatment check
pl_treatments_check <- qc_pl_treatments(plant_md, parent_logger = logger_name)


################################################################################

# data qc
## timestamp
psi_data_fixed <- qc_as_timestamp(psi_data, site_md, logger_name)


## timestamp NAs
psi_timestamp_nas <- qc_timestamp_nas(psi_data_fixed, logger_name)

## psi NAs
psi_nas <- qc_psi_nas(psi_data_fixed, logger_name)

## psi SE NAs
psi_SE_nas <- qc_psi_SE_nas(psi_data_fixed, logger_name)

## psi N NAs
psi_N_nas <- qc_psi_N_nas(psi_data_fixed, logger_name)

## extraterrestrial ratiation and timestamp
psi_data_fixed <- qc_ext_radiation(psi_data_fixed, site_md,TRUE)




################################################################################
# create the psiData object and save it as a RData file for later use
## psidata_object
psi_data_object <- psi_data_constructor(
  psi_data = psi_data_fixed,
  site_md = site_md_coordfix,
  plant_md = plant_md,
  question_md = questionnaire_simplified,
  parent_logger = logger_name
)

# save it!
assign(params$code, psi_data_object)
save(list = c(params$code),
     file = file.path('Data', params$code, 'Lvl_1',
                      paste(params$code, '.RData', sep = '')),
     envir = environment())


################################################################################
# results md_qc table
qc_md_results_table(md_cols, factor_values, email_check, site_md_coordfix,
                    plant_md_spnames, parent_logger = logger_name)
################################################################################
################################################################################
# table
qc_data_results_table(psi_data_fixed, psi_timestamp_nas,
                      psi_nas, psi_SE_nas, psi_N_nas,
                      parent_logger = logger_name)
################################################################################

# 2.2.6 saving the fixed datasets and the objects created in the level1 folder
df_accepted_to_lvl1_psi(
  params$code, psi_data_fixed,
  site_md_coordfix, plant_md, questionnaire_simplified,
  parent_logger = 'DataFlow'
)

# saving Rdata file with all the objects (just in case)
save(list = ls(all.names = TRUE),
     file = file.path('Data', params$code, 'Lvl_1',
                      paste(params$code, 'objects.RData', sep = '_')),
     envir = environment())
