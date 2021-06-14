################################################################################
#' Variable dictionaries for metadata
#'
#' \code{create_dic} creates a dictionary containing metadata variable names
#' and expected class. It is intended as an internal function.
#'
#' After loading metadata sheets, introduced variables and their classes must be
#' checked in order to ensure data correctness. For that, we need dictionaries
#' containing all the expected variables and their corresponding classes to
#' compare. This function works inside of \code{\link{qc_col_class}}, so there
#' is no need to call it directly
#'
#' @family Quality Checks Functions
#'
#' @param dic_name Name of the metadata sheet of which dictionary is needed.
#'   It must be one of \code{site_md}, \code{plant_md}, \code{psi_data}
#'   or \code{Questionnaire}.
#'
#' @return A named list, variable names being the index and class the value

# START
# Function declaration
create_dic <- function(dic_name, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({
    # STEP 0
    # Argument checking
    # check if dictionary name is one of the five kinds of metadata allowed
    accepted_sheets <- c('site_md', 'plant_md', 'psi_data', 'Questionnaire')

    if (!is.character(dic_name) || !(dic_name %in% accepted_sheets)) {
      stop('Provided dicitonary name is not a character or is not a valid name.',
           ' Please see function help for information about valid dictionary names')
    }

    # STEP 1
    # Get the kind of metadata and populate the dictionary

    # 1.1 site metadata
    if (dic_name == 'site_md') {
      dic <- list(id_sfn = 'character',
                  id_fn = 'character',
                  site_name = 'character',
                  site_country = 'character',
                  lat = c('numeric', 'integer'),
                  lon = c('numeric', 'integer'),
                  elev = c('numeric', 'integer'),
                  contact_firstname = 'character',
                  contact_lastname = 'character',
                  contact_institution = 'character',
                  contact_email = 'character'
                  )

      # 1.1.1 return dic
      return(dic)
    }


    # 1.2 Plant metadata
    if (dic_name == 'plant_md') {
      dic <- list(pl_name = c('character', 'numeric', 'integer'),
                  pl_code = 'character',
                  pl_species = 'character',
                  pl_height = c('numeric', 'integer'),
                  pl_dbh = c('numeric', 'integer'),
                  pl_treatment = 'character',
                  pl_status = 'character',
                  measured_sfn = 'character')

      # 1.2.1 return dic
      return(dic)
    }


    # 1.3 PSI metadata
    if (dic_name == 'psi_data') {
      dic <- list(time_psi = 'character',
                  canopy_position = 'character',
                  method = 'character',
                  organ = 'character',
                  aggregation_level = 'character',
                  remarks = 'character'
                  )

      # 1.3.1 return dic
      return(dic)
    }


    # 1.4 Questionnaire
    if (dic_name == 'Questionnaire') {
      dic <- list(`CloseFluxnetTower` = 'character',
                  `CloseEddyCovarianceTower` = 'character',
                  `ContributorToGlobalDatabase` = 'character',
                  `Dendrometers` = 'character'
                  )

      # 1.4.1 return dic
      return(dic)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'create_dic', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'create_dic', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'create_dic', sep = '.'))})


}

################################################################################
#' Dictionary creation for site_md variables
#'
#' \code{qc_site_dics} function creates a dictionary for the selected variable
#' containing the accepted values for that variable
#'
#' In order to check if factor variables have a valid value or have been bad
#' formatted/introduced in the data template, first it is needed to have a list
#' of accepted values for each variable. This function creates that list to
#' use in the checks.
#'
#' @section Accepted variables:
#' The factor variables in site_md is \code{site_country}.
#'
#' @family Dictionaries
#'
#' @param variable Variable name in which the dictionary is needed as character
#'   vector (e.g. \code{'site_country'}).
#'
#' @return A character vector containing the valid values for the provided
#'   variable
#'
#' @export

# START
# Function declaration
qc_site_dics <- function(variable, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # valid variables for site_md
    accepted_vars <- c('site_country')
    if (!(variable %in% accepted_vars)) {
      stop('Variable provided (', variable,') is not adequate for creating a dictionary.',
           ' Please see "Accepted variables" section of the function help.')
    }

    # STEP 1
    # Get the variable and populate the dictionary
    # 1.1 site_country
      res <- c('AFG', 'ALA', 'ALB', 'DZA', 'ASM', 'AND', 'AGO', 'AIA', 'ATA',
               'ATG', 'ARG', 'ARM', 'ABW', 'AUS', 'AUT', 'AZE', 'BHS', 'BHR',
               'BGD', 'BRB', 'BLR', 'BEL', 'BLZ', 'BEN', 'BMU', 'BTN', 'BOL',
               'BES', 'BIH', 'BWA', 'BVT', 'BRA', 'IOT', 'BRN', 'BGR', 'BFA',
               'BDI', 'CPV', 'KHM', 'CMR', 'CAN', 'CYM', 'CAF', 'TCD', 'CHL',
               'CHN', 'CXR', 'CCK', 'COL', 'COM', 'COG', 'COD', 'COK', 'CRI',
               'CIV', 'HRV', 'CUB', 'CUW', 'CYP', 'CZE', 'DNK', 'DJI', 'DMA',
               'DOM', 'ECU', 'EGY', 'SLV', 'GNQ', 'ERI', 'EST', 'ETH', 'FLK',
               'FRO', 'FJI', 'FIN', 'FRA', 'GUF', 'PYF', 'ATF', 'GAB', 'GMB',
               'GEO', 'DEU', 'GHA', 'GIB', 'GRC', 'GRL', 'GRD', 'GLP', 'GUM',
               'GTM', 'GGY', 'GIN', 'GNB', 'GUY', 'HTI', 'HMD', 'VAT', 'HND',
               'HKG', 'HUN', 'ISL', 'IND', 'IDN', 'IRN', 'IRQ', 'IRL', 'IMN',
               'ISR', 'ITA', 'JAM', 'JPN', 'JEY', 'JOR', 'KAZ', 'KEN', 'KIR',
               'PRK', 'KOR', 'KWT', 'KGZ', 'LAO', 'LVA', 'LBN', 'LSO', 'LBR',
               'LBY', 'LIE', 'LTU', 'LUX', 'MAC', 'MKD', 'MDG', 'MWI', 'MYS',
               'MDV', 'MLI', 'MLT', 'MHL', 'MTQ', 'MRT', 'MUS', 'MYT', 'MEX',
               'FSM', 'MDA', 'MCO', 'MNG', 'MNE', 'MSR', 'MAR', 'MOZ', 'MMR',
               'NAM', 'NRU', 'NPL', 'NLD', 'NCL', 'NZL', 'NIC', 'NER', 'NGA',
               'NIU', 'NFK', 'MNP', 'NOR', 'OMN', 'PAK', 'PLW', 'PSE', 'PAN',
               'PNG', 'PRY', 'PER', 'PHL', 'PCN', 'POL', 'PRT', 'PRI', 'QAT',
               'REU', 'ROU', 'RUS', 'RWA', 'BLM', 'SHN', 'KNA', 'LCA', 'MAF',
               'SPM', 'VCT', 'WSM', 'SMR', 'STP', 'SAU', 'SEN', 'SRB', 'SYC',
               'SLE', 'SGP', 'SXM', 'SVK', 'SVN', 'SLB', 'SOM', 'ZAF', 'SGS',
               'SSD', 'ESP', 'LKA', 'SDN', 'SUR', 'SJM', 'SWZ', 'SWE', 'CHE',
               'SYR', 'TWN', 'TJK', 'TZA', 'THA', 'TLS', 'TGO', 'TKL', 'TON',
               'TTO', 'TUN', 'TUR', 'TKM', 'TCA', 'TUV', 'UGA', 'UKR', 'ARE',
               'GBR', 'USA', 'UMI', 'URY', 'UZB', 'VUT', 'VEN', 'VNM', 'VGB',
               'VIR', 'WLF', 'ESH', 'YEM', 'ZMB', 'ZWE')

      # 1.1.1 return the dic
      return(res)


    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_site_dics', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_site_dics', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_site_dics', sep = '.'))})

}



################################################################################
#' Dictionary creation for plant_md variables
#'
#' \code{qc_plant_dics} function creates a dictionary for the selected variable
#' containing the accepted values for that variable
#'
#' In order to check if factor variables have a valid value or have been bad
#' formatted/introduced in the data template, first it is needed to have a list
#' of accepted values for each variable. This function creates that list to
#' use in the checks.
#'
#' @section Accepted variables:
#' The factor variables in plant_md are \code{pl_status} and \code{measured_sfn}.
#'
#' @family Dictionaries
#'
#' @param variable Variable name in which the dictionary is needed as character
#'   vector (e.g. \code{'pl_status'}).
#'
#' @return A character vector containing the valid values for the provided
#'   variable
#'
#' @export


# START
# Function declaration
qc_plant_dics <- function(variable, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # valid variables for site_md
    accepted_vars <- c('pl_status', 'measured_sfn')
    if (!(variable %in% accepted_vars)) {
      stop('Variable provided (', variable,
           ') is not adequate for creating a dictionary.',
           ' Please see "Accepted variables" section of the function help.')
    }

    # STEP 1
    # Get the variable and populate the dictionary
    # 1.1 pl_status
    if (variable == 'pl_status') {
      res <- c('healthy', 'incipient_stress', 'moderate_stress','intense_stress')

      # 1.1.1 return the dic
      return(res)
    }

    # 1.2 measured_sfn
    if (variable == 'measured_sfn') {
      res <- c('yes', 'no')

      # 1.2.1 return the dic
      return(res)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_plant_dics', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_plant_dics', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_plant_dics', sep = '.'))})

}

################################################################################
#' Dictionary creation for psi_data_md variables
#'
#' \code{qc_psi_dics} function creates a dictionary for the selected variable
#' containing the accepted values for that variable
#'
#' In order to check if factor variables have a valid value or have been bad
#' formatted/introduced in the data template, first it is needed to have a list
#' of accepted values for each variable. This function creates that list to
#' use in the checks.
#'
#' @section Accepted variables:
#' The factor variables in environmental_md are \code{time_psi},
#' \code{canopy_position}, \code{method}, \code{organ} and \code{aggregation_level}.
#'
#' @family Dictionaries
#'
#' @param variable Variable name in which the dictionary is needed as character
#'   vector (e.g. \code{'method'}).
#'
#' @return A character vector containing the valid values for the provided
#'   variable
#'
#' @export

# START
# Function declaration
qc_psi_dics <- function(variable, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # valid variables for site_md
    accepted_vars <- c('time_psi', 'canopy_position', 'method', 'organ',
                       'aggregation_level')
    if (!(variable %in% accepted_vars)) {
      stop('Variable provided (', variable,
           ') is not adequate for creating a dictionary.',
           ' Please see "Accepted variables" section of the function help.')
    }

    # STEP 1
    # Get the variable and populate the dictionary
    # 1.1 time_psi
    if (variable == 'time_psi') {
      res <- c('pre-dawn', 'midday', 'continous', 'other')

      # 1.1.1 return the dic
      return(res)
    }

    # 1.2 canopy position
    if (variable == 'canopy_position') {
      res <- c('top', 'mid', 'bottom')

      # 1.2.1 return the dic
      return(res)
    }

    # 1.3 method
    if (variable == 'method') {
      res <- c('chamber-bagged', 'chamber-unbagged', 'psychometer')

      # 1.3.1 return the dic
      return(res)
    }

    # 1.4 organ
    if (variable == 'organ') {
      res <- c('leaf', 'frond', 'twig', 'stem', 'root', 'other')

      # 1.4.1 return the dic
      return(res)
    }

    # 1.5 aggregation level
    if (variable == 'aggregation_level') {
      res <- c('tree level', 'species level')

      # 1.5.1 return the dic
      return(res)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_env_dics', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_env_dics', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_env_dics', sep = '.'))})

}

################################################################################
#' Metadata columns check
#'
#' \code{qc_md_cols} checks if the columns of the provided metadata return the
#' correct class, as well as indicates any NAs present and the absence of any
#' mandatory variable.
#'
#' After loading the metadata, columns classes and presence/absence of mandatory
#' variables must be checked in order to continue with the quality check
#' workflow. This function returns a summary of the metadata columns and their
#' state.
#'
#' @family Quality Checks Functions
#'
#' @param metadata Data frame containing the data or metadata in which the test
#'   will be made.
#'
#' @param dic Name of the metadata dictionary to use as character. It must be
#'   one of the following: \code{'site_md'}, \code{'plant_md'},
#'   \code{'psi_data'}, or \code{'Questionnaire'}
#'
#' @return A data frame with variable names in one column and result of the
#'   checks in the following columns
#'
#' @export

# START
# Function declaration

qc_md_cols <- function(metadata, dic,
                       parent_logger = 'test') {

  # Using calling handlers for logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # metadata is a data frame?
    if (!is.data.frame(metadata)) {
      stop('Metadata object is not a data frame')
    }

    # check if dictionary name is one of the five kinds of metadata allowed
    accepted_sheets <- c('site_md', 'plant_md', 'psi_data',
                         'Questionnaire')

    if (!is.character(dic) || !(dic %in% accepted_sheets)) {
      stop('Provided dictionary name is not a character or is not a valid name.',
           ' Please see function help for information about valid dictionary names')
    }

    # STEP 1
    # Initialise result objects and dictionary
    dictionary <- create_dic(dic) # dictionary
    md_variables <- names(metadata) # variable names
    presence_res <- vector() # results of presence test
    classes_res <- vector() # results of class test
    det_class_res <- vector() # detected class
    na_res <- vector() # results of all NA test
    na_some_res <- vector() # results of all NA test
    unique_res <- vector() # results of unique values test

    # STEP 2
    # Checks
    for (name in names(dictionary)) {
      # 2.1 Presence test
      p_res <- name %in% md_variables
      presence_res <- c(presence_res, p_res)

      # 2.2 Class test
      d_res <- class(metadata[[name]])
      c_res <- any(dictionary[[name]] == d_res)

      classes_res <- c(classes_res, c_res)
      det_class_res <- c(det_class_res, d_res)

      # 2.3 All NA test
      if (p_res) {
        n_res <- all(is.na(lapply(metadata[[name]], function(x) {
                                    if(x == "NA"|is.na(x)){x <- NA}
                                    return(x)
                                    }
                                  )
                           )
                     )
        na_res <- c(na_res, n_res)
      } else {
        n_res <- NA
        na_res <- c(na_res, n_res)
      }


    # 2.4 Any NA test
    if (p_res) {
      some_res <- any(is.na(lapply(metadata[[name]], function(x) {
                                if(x == "NA"|is.na(x)){x <- NA}
                                return(x)
                                }
                                )
                            )
                      )
      na_some_res <- c(na_some_res, some_res)
    } else {
      some_res <- NA
      na_some_res <- c(na_some_res, some_res)
    }

    # 2.5 Unique value test
    if (p_res) {
      u_res <- ifelse(unique(metadata[[name]]) %>% length() == 1,TRUE,FALSE)
      unique_res <- c(unique_res, u_res)
    } else {
      u_res <- NA
      unique_res <- c(unique_res, u_res)
    }
    }

    # STEP 3
    # Create and return the result object
    result <- data.frame(Variable = names(dictionary),
                         PresenceOK = presence_res,
                         DetectedClass = det_class_res,
                         ClassOK = classes_res,
                         allNA = na_res,
                         anyNA = na_some_res,
                         UniqueValue = unique_res,
                         stringsAsFactors = FALSE)

    return(result)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_md_cols', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_md_cols', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_md_cols', sep = '.'))})

}

################################################################################
#' Metadata factor variables check
#'
#' \code{qc_factor_values} function checks in the provided metadata if the
#' factor variables value is a valid value.
#'
#' Values for factor variables in the metadata must be checked in order to
#' ensure that they are valid (i.e. they are one of the established factor
#' value).
#'
#' @family Quality Checks Functions
#'
#' @param site,plant,psi Data frames with the metadata
#'   to check.
#'
#' @return A data frame with variable names and check result.
#'
#' @export

# START
# Function declaration
qc_factor_values <- function(site = NULL, plant = NULL, psi = NULL,
                             parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    #

    # STEP 1
    # Walk through metadata files and check variables
    # 1.1 site
    if (!is.null(site)) {
      site_names <- c('site_country')
      site_checks <- sapply(site_names, function(x) { site[[x]] %in% qc_site_dics(x) })
    }

    # 1.2 plant
    if(!is.null(plant)) {
      pl_names <- c('pl_status', 'measured_sfn')
      pl_checks <- sapply(pl_names, function(x) {
        plant[[x]] %in% qc_plant_dics(x)
      })
    }

    # 1.3 psi
    if(!is.null(psi)) {
      psi_names <- c('time_psi', 'canopy_position', 'method', 'organ',
                     'aggregation_level')
      psi_checks <- sapply(psi_names, function(x) {
        psi[[x]] %in% qc_psi_dics(x)
        })
    }

    # 2. Generate the results data frame and return it
    if(nrow(data.frame(site_checks))>1){
      res_data <- data.frame(site_checks, pl_checks, psi_checks)
    }else{
      res_data <- bind_cols(site_checks %>% bind_rows(),
                            pl_checks %>% bind_rows(),
                            psi_checks %>% bind_rows())
    }

    return(res_data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_factor_values', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_factor_values', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_factor_values', sep = '.'))})

}
