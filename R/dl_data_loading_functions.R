################################################################################
#' Remove columns with duplicate names
#'
#' \code{remove_dupcols} is an internal function to use inside of the \code{dl_*}
#' functions. It checks for duplicate column names and drop them. It also checks
#' for empty rows (all row NAs, TIMESTAMP included) and delete them.
#'
#' When exporting from excel files, sometimes cell fomatting makes empty columns
#' and rows to be read and loaded in R. If that is the case, any try to transform
#' and to shape the data faces a "duplicate column names error". This can be
#' solved by the correct formatting of the excel files, but this can not be
#' always achieved, hence this function. Also, when excel files with formatting
#' are read sometimes empty rows are added. This function also check for that
#' and fix it if happens.
#'
#' @family Data Loading Functions
#'
#' @param data Data frame in which check for duplicate column names
#'
#' @return \code{remove_dupcols} returns the loaded data with duplicate columns
#'   and wmpty rows removed, if any.
#'
#' @export

# START
# Function declaration

remove_dupcols <- function(data, parent_logger = 'test') {

  # Using callin handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checking
    # data is a data_frame
    if (!is.data.frame(data)) {
      stop('Data is not a data frame')
    }

    # STEP 1
    # Check for duplicate columns and drop them if any
    if (any(duplicated(names(data)))) {
      res <- data[!duplicated(names(data))]
      res <- res[rowSums(is.na(res)) != ncol(res), ]
      return(res)
    } else {
      res <- data[rowSums(is.na(data)) != ncol(data), ]
      return(res)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'remove_dupcols', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'remove_dupcols', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'remove_dupcols', sep = '.'))})
}



################################################################################
#' Check data columns classes and set to numeric if needed
#'
#' Checking and setting data column classes to numeric
#'
#' Loading from csv's with fread can be problematic if some characters are
#' spreaded here and there in the data columns. This function checks for this
#' problem and tries to fix it
#'
#' @family Data Loading Functions
#'
#' @param data Data frame with the sapflow or environmental data to check
#'
#' @return A data frame exactly as the data provided with the variables
#'   changed to numeric if needed
#'
#' @export

# START
# Function declaration
dl_data_col_classes <- function(data, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # is data a data frame?
    if (!is.data.frame(data)) {
      stop('Data provided is not a data frame')
    }

    # STEP 1
    # Loop itereating between variables, checking the class and avoiding
    # the TIMESTAMP
    for (var in names(data[, -1])) {

      # 1.1 if the class is different from numeric, lets transform it!!!
      if (all(!is.numeric(data[[var]]), !is.logical(data[[var]]))) {
        message('Converting to numeric ', var)
        data[, var] <- as.numeric(data[[var]])
      }
    }

    # STEP 2
    # Return the converted data
    return(data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_data_col_classes',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_data_col_classes',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_data_col_classes',
                                                        sep = '.'))})
}


################################################################################
#' Loading metadata from xls/xlsx
#'
#' \code{dl_metadata} function loads the metadata sheets from xls/xlsx file.
#'
#' This function make use of dplyr, tidyr and readxl packages in order to
#' retrieve and format the metadata. It's intended to be used as first step
#' to load the metadata and start the metadata quality check.
#'
#' @family Data Loading Functions
#'
#' @param file_name Character vector indicating the name of the xls/xlsx file
#'   containing the metadata.
#'
#' @param sheet_name Character vector indicating the name of the sheet to be
#'   loaded. It must be one of \code{Data} or \code{Questionnaire}.
#'
#' @param data_type Character vector indicating the name of the data section.
#'  It must be one of \code{site_md}, \code{plant_md}, \code{psi_data} or NA. If
#'  \code{sheet_name} is \code{Questionnaire} then \code{data_type} is not used.
#'
#' @param si_code_loc Name of the object containing the site metadata, in order
#'   to obtain si_code variable to include it in other metadata objects. Default
#'   to \code{NULL}, as the first metadata to load must be the site metadata.
#'
#' @return The function returns a data_frame with the corresponding metadata
#'   in "wide" format, with metadata variables as columns, ready to be feeded
#'   to quality check functions.
#'
#' @importFrom magrittr %>%
#'
#' @export

# START
# Function declaration

dl_metadata <- function(file_name, sheet_name, data_type = NA,
                        si_code_loc = NULL, parent_logger = 'test'){

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checking

    # check if file name is a character and it exist
    if (!is.character(file_name)) {
      stop('File name is not provided as character')
    }

    if (!file_test("-f", file_name)) {
      stop('File does not exist, please check if file name has been correctly provided')
    }

    # check if sheet name is one of the two kinds of metadata allowed
    accepted_sheets <- c('Data', 'Questionnaire')

    if (!is.character(sheet_name) || !(sheet_name %in% accepted_sheets)) {
      stop('Provided sheet name is not a character or is not an accepted sheet. ',
           'Please see function help')
    }

    # check if data_type is one of the three values allowed
    accepted_types <- c('site_md', 'plant_md', 'psi_data')

    if (all(!is.character(data_type) || !(data_type %in% accepted_types), !is.na(data_type))) {
      stop('Provided data_type is not a character or is not an accepted data type. ',
           'Please see function help')
    }

    # check
    if (sheet_name == "Data" & is.na(data_type)) {
      stop('Since sheet name is Data, data_type parameter must be provided. ',
           'Please see function help')
    }

    # check for si_code_loc and if NULL set si_code to NA
    if (is.null(si_code_loc)) {
      si_code_txt <- NA
      message('si_code_loc set to NULL. If loading other metadata than site_md,',
              ' please indicate the object containing the site metadata.')
    } else {
      si_code_txt <- si_code_loc$id_sfn[1]
    }

    # STEP 1
    # Load xlsx sheet and tidy it

    # 1.1 If sheet is Data and data_type is site_md.
    if (all(sheet_name == 'Data', data_type == "site_md")) {
      # read the sheet
      res <- readxl::read_excel(file_name, sheet_name, skip = 0) %>%
        # check for duplicate columns
        remove_dupcols() %>%
        # column selection
        dplyr::select('id_sfn','id_fn', 'site_name', 'site_country', 'lat', 'lon',
               'elev', 'contact_firstname', 'contact_lastname',
               'contact_institution',	'contact_email')

      # 1.1.2 return the Data sheet
      return(res)
    }

    # 1.2 If sheet is Data and data_type is plant_md.
    if (all(sheet_name == 'Data', data_type == "plant_md")) {
      # read the sheet
      res <- readxl::read_excel(file_name, sheet_name, skip = 0) %>%
        # check for duplicate columns
        remove_dupcols() %>%
        # column selection
        dplyr::select('pl_name','pl_code', 'pl_species', 'pl_height', 'pl_dbh',
        'pl_treatment', 'pl_status','measured_sfn') %>%
        dplyr::mutate(pl_name = as.character(pl_name),
                     pl_code = as.character(pl_code),
                     pl_height = suppressWarnings(as.numeric(pl_height)),
                     pl_dbh = suppressWarnings(as.numeric(pl_dbh)))

      # 1.2.2 return the Data sheet
      return(res)
    }

    # 1.3 If sheet is Data and data_type is psi_data.
    if (all(sheet_name == 'Data', data_type == "psi_data")) {
      # read the sheet
      res <- readxl::read_excel(file_name, sheet_name, skip = 0) %>%
        # check for duplicate columns
        remove_dupcols() %>%
        # rename psi columns
        dplyr::rename(psi = 25, psi_SE = 26, psi_N = 27) %>%
        # column selection
        dplyr::select('timestamp','pl_name','pl_code', 'time_psi', 'canopy_position', 'method',
                      'organ', 'psi','psi_SE', 'psi_N', 'aggregation_level',
                      'remarks')

      # 1.3.2 return the Data sheet
      return(res)
    }


    # 1.4 If sheet is Questionnaire we need to take extra steps to tidy the data
    if (sheet_name == 'Questionnaire') {
      # read the sheet
      res <- readxl::read_excel(file_name, sheet_name) %>%
        # check for duplicate columns
        remove_dupcols() %>%
        # select only 4 rows
        dplyr::slice(c(1:4)) %>%
        tidyr::pivot_wider(values_from = "Answer", names_from = "Question")

      # 1.4.2 return the plant metadata
      return(res)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'dl_metadata', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'dl_metadata', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'dl_metadata', sep = '.'))})

}


################################################################################
#' Get the site code and the names of the data files
#'
#' Look at the data folder provided and get the code and the names of the files
#' with the metadata, and psi data, in order to use them
#' as parameters in the automated reports
#'
#' @family Data Loading Functions
#'
#' @param folder Route to folder in which are the code and the file names to
#'   retrieve
#'
#' @return A list. The first element is the site code, the second one the
#'   metadata file route, the third the psi data file route.
#'
#' @export

# START
# Function declaration
dl_get_si_code_psi <- function(folder = '.', parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # is folder a valid and existent folder?
    if (!file_test("-d", folder)) {
      stop('Folder does not exist, please check if folder name has been correctly provided')
    }

    # STEP 1
    # catch the files
    files <- list.files(folder,
                        pattern = ".xls(x)?$")
    complete_files <- list.files(folder,
                                 pattern = ".xls(x)?$",
                                 full.names = TRUE)

    # 1.1 Check if there is files, to avoid waste time
    if (length(files) < 1) {
      stop('There is no files matching data names pattern')
    }

    # STEP 2
    # Extract the si_code, is needed in the returned object
    code <- unique(stringr::str_replace(
      files, ".xls(x)?$", ""
    ))

    # 2.1 check if there is more than one site code, which is a problem
    if (length(code) > 1) {
      stop('There is more than one site code in the folder, please revise manually the folder')
    }

    # STEP 3
    # How many files?

    # 3.1 if more than one files which ends up by .xlsx, then stop
    if (length(files) > 1) {
      stop('There is more than one data files, please revise manually the folder')
    } else {
      # 3.3 set files names
      metadata <- complete_files[grep('.xls(x)?$', complete_files)]
      psi <- metadata
    }

    # STEP 4
    # now, lets make the results object, a list
    res <- list(
      site_code = code,
      md_file = metadata,
      psi_file = psi
    )

    # STEP 5
    # Return it
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'dl_get_si_code_psi', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'dl_get_si_code_psi', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'dl_get_si_code_psi', sep = '.'))})

}
