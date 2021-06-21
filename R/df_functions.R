################################################################################
# DATA FLOW FUNCTIONS                                                          #
#                                                                              #
# Functions to perform data flows and to create data folder architecture       #
################################################################################

################################################################################
#' Initial project folder structure
#'
#' Create the folder tree skeleton for PSI database project
#'
#' This functions only need to be called once, as it creates the needed folder
#' structure to recreate PSI database project in any environment.
#'
#' @family Data Flow
#'
#' @param parent_dir Character vector indicating the parent directory where
#'   the structure must be built. Default to current directory.
#'
#' @return Nothing, dirs are created if they do not exist, or a warning is raised
#'   if they already exist.
#'
#' @export

# START
# Function declaration
df_folder_structure <- function(parent_dir = '.', parent_logger = 'test') {

  # Use calling handlers to logging to files
  withCallingHandlers({

    # STEP 0
    # Argument checking
    # Is parent directory a character vector?
    if (!is.character(parent_dir)) {
      stop('Provided parent directory is not a character vector')
    }
    # Is parent directory a valid and existent directory?
    if (!dir.exists(parent_dir)) {
      stop('Provided parent directory does not exist')
    }

    # STEP 1
    # Check if dirs already exists and if not, create it (all with dir.create)
    dir.create(file.path(parent_dir, 'Data'), showWarnings = TRUE)
    dir.create(file.path(parent_dir, 'Logs'), showWarnings = TRUE)
    dir.create(file.path(parent_dir, 'Reports'), showWarnings = TRUE)
    dir.create(file.path(parent_dir, 'Templates'), showWarnings = TRUE)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_folder_structure',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_folder_structure',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_folder_structure',
                                                        sep = '.'))})
}

################################################################################
#' Move from received to accepted data folders
#'
#' Check if data file/s already exists and create the folder structure needed to
#' store them.
#'
#' The first step after receiving a data set is to move it from received folder
#' to accepted folder (\code{Data/site_code/site_code_accepted/}).
#'
#' @family Data Flow
#'
#' @param remove Logical indicating if files in received folder are dropped after
#'   the file transfer. Default to FALSE.
#'
#' @return
#'
#' @export

# START
# Function declaration
df_received_to_accepted_psi <- function(remove = FALSE, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checking
    # is remove logical?
    if (!is.logical(remove)) {
      stop('remove parameter provided but not logical (TRUE or FALSE)')
    }

    # STEP 1
    # Obtaining the file names in the received folder.
    files <- list.files('received_data',
                        pattern = ".xls(x)?$")

    # STEP 2
    # Extract site code from file names
    codes <- unique(stringr::str_replace(
      files, ".xls(x)?$", ""
    ))

    # STEP 3
    # Check if folder with site code already exists
    for (code in codes) {

      # message indicating which code is moving
      message('Copying files corresponding to ', code, ' site.')

      # 3.1 path and file names
      path <- file.path('Data', code)
      path_accepted <- file.path(path, 'Accepted')
      from_files <- list.files('received_data',
                               pattern = paste(code, '.xls(x)?$', sep = ''),
                               full.names = TRUE)
      file_names <- stringr::str_replace(from_files, 'received_data', path_accepted)

      # 3.2 check presence
      if (dir.exists(path)) {
        warning('Folder ', path, ' already exists! ',
                'This means that site is already in the system')
        if (any(file.exists(file_names))) {
          warning('One or more files already exist. ',
                  'Not copying any file, manual intervention needed.')
          next
        } else {

          # 2.3 copy the files if directory exists but files don't
          file.copy(from = from_files, to = path_accepted, overwrite = FALSE)
        }
      } else {

        # STEP 4
        # Creating the data folder structure for site
        # dir.create(path)
        dir.create(path_accepted, recursive = TRUE)
        dir.create(file.path(path, 'Lvl_1'))
        # dir.create(file.path(path, 'Lvl_2'))

        # STEP 5
        # Copy data to accepted folder
        file.copy(from = from_files, to = path_accepted, overwrite = FALSE)
      }

      # STEP 6
      # Check if copy has been done correctly by comparing file md5sums
      md5_ok <- tools::md5sum(from_files) == tools::md5sum(file_names)

      if (!all(md5_ok, na.rm = TRUE)) {
        warning('md5sums are not the same for original and copied files. ',
                'Please revise the files manually.',
                ' Skipping to the next site code (if any)')
        next
      }

      # STEP 7
      # Remove from files
      if (remove & all(md5_ok, na.rm = TRUE)) {
        message('Removing the received files for site ', code, '.')
        file.remove(from_files)
        message('Remove DONE!')
      } else {
        warning('remove argument set to FALSE, data will be left ',
                'in Received Data folder')
      }

      # STEP 8
      # Indicating which files were copied
      files_copied <- vapply(dir(path_accepted),
                             function(x){paste(x, '\n', sep = '')},
                             character(1))

      message("List of files copied to ", path_accepted, ':\n', files_copied)

    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_received_to_accepted_psi',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_received_to_accepted_psi',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_received_to_accepted_psi',
                                                        sep = '.'))})
}

################################################################################
#' Initialise an empty status file
#'
#' Initialise an empty status file in yaml format, using the yaml package
#'
#' Before creating an empty file, \code{df_start_status_psi} checks if an status
#' file already exists, in order to avoid accidental rewriting of the file.
#'
#' @family Data Flow
#'
#' @param si_code Character vector indicating the site code
#'
#' @return Invisible TRUE if no errors were encountered, invisible FALSE if
#'   there was errors. Also, status file is created in the corresponding folder.
#'
#' @export

# START
# Function declaration
df_start_status_psi <- function(si_code, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is si_code a character?
    if (!is.character(si_code)) {
      stop('si_code is not a character')
    }

    # STEP 1
    # Check if status file already exists
    filename <- file.path('Data', si_code, paste(si_code, '_status.yaml', sep = ''))

    if (file.exists(filename)) {

      # 1.1 if exists, raise a warning and return false
      warning('Status file for ', si_code, 'already exists, skipping creation of empty file')
      return(invisible(FALSE))

      # 1.2 if not, continue to step 2
    } else {

      # STEP 2
      # Create the file

      # 2.1 create the content
      content <- list(
        QC = list(DONE = FALSE, DATE = NULL),
        LVL1 = list(STORED = FALSE, DATE = NULL,
                    TO_LVL2 = 'FREEZE')
      )

      # 2.2 create the yaml object
      yaml_content <- yaml::as.yaml(content)

      # 2.3 create the file with the yaml object
      cat(yaml_content, file = filename, append = FALSE)

      # 2.4 return true (invisible)
      return(invisible(TRUE))
    }

    # END FUNCTION

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_status_start_psi',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_status_start_psi',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_status_start_psi',
                                                        sep = '.'))})

}

################################################################################
#' Get the status file info
#'
#' Retrieve the status file info as a list, with the yaml package
#'
#' \code{yaml} file is parsed by yaml.load_file and a list object is returned.
#'
#' @family Data Flow
#'
#' @param si_code Character vector indicating the site code
#'
#' @return a list object with the info contained in the status file
#'
#' @export

# START
# Function declaration

df_get_status <- function(si_code, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is si_code a character?
    if (!is.character(si_code)) {
      stop('si_code is not a character')
    }

    # STEP 1
    # Check if status file exists
    filename <- file.path('Data', si_code, paste(si_code, '_status.yaml', sep = ''))
    if (!file.exists(filename)) {

      # 1.1 if don't exist, raise a warning and return false
      warning('Status file for ', si_code, 'does not exist, unable to retrieve info')
      return(invisible(FALSE))
    } else {

      # STEP 2
      # Get the yaml object
      res <- yaml::yaml.load_file(filename)

      # 2.1 and return it
      return(res)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_get_status',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_get_status',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_get_status',
                                                        sep = '.'))})
}

################################################################################
#' Set the status file info
#'
#' Change and update status file info using yaml package
#'
#' \code{QC} and \code{LVL1} must be lists. In the case of
#' \code{QC}, a list with two elements, \code{DONE} and \code{DATE}. In the case
#' of \code{LVL1} a list with two elements, \code{STORED} and
#' \code{DATE}. \code{DONE} and \code{STORED} are logical, whereas \code{DATE}
#' is always a character or NULL.
#'
#' @family Data Flow
#'
#' @param si_code Character vector indicating the site code
#'
#' @param QC List with the QC info to be updated
#'
#' @param LVL1 List with the LVL1 info to be updated
#'
#' @return Invisible TRUE if changes to status file were correctly made,
#'   invisible FALSE if changes were not made. Also, the status file for the site
#'   will be replaced with the new one.
#'
#' @export

# START
# Function declaration
df_set_status_psi <- function(si_code,
                          QC = NULL,
                          LVL1 = NULL,
                          parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is si_code a character?
    if (!is.character(si_code)) {
      stop('si_code is not a character')
    }

    # STEP 1
    # Check if the file already exists
    filename <- file.path('Data', si_code, paste(si_code, '_status.yaml', sep = ''))
    if (!file.exists(filename)) {

      # 1.1 if don't exist, raise a warning and return false
      warning('Status file for ', si_code, 'does not exist, unable to retrieve info')
      return(invisible(FALSE))
    } else {

      # STEP 2
      # Modify the status file

      # 2.1 get the original
      original_yaml <- df_get_status(si_code, parent_logger = parent_logger)

      # 2.2 modify original with new no NULL elements
      if (!is.null(QC)) {
        purrr::walk(names(QC), function(x) {
          original_yaml[["QC"]][[x]] <<- QC[[x]]
        })
      }

      if (!is.null(LVL1)) {
        purrr::walk(names(LVL1), function(x) {
          original_yaml[["LVL1"]][[x]] <<- LVL1[[x]]
        })
      }

      # STEP 3
      # Convert to yaml and rewrite the status file
      res <- yaml::as.yaml(original_yaml)

      # 3.1 rewrite
      cat(res, file = filename, append = FALSE)

      # return invisible TRUE
      return(invisible(TRUE))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_set_status_psi', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_set_status_psi', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_set_status_psi', sep = '.'))})
}




################################################################################
#' Reports folders creation
#'
#' Function to create the report subfolder for the site to analyze
#'
#' @family Data Flow
#'
#' @param si_code Character indicating the site code to create the reports folder
#'
#' @return Invisible TRUE if the folder is created correctly, invisible FALSE if
#'   folder is not created.
#'
#' @export

# START
# Function declaration
df_report_folder_creation <- function(si_code, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # is si_code a character?
    if(!is.character(si_code)) {
      stop('si_code provided is not a character')
    }

    # STEP 1
    # Check if folder already exists
    folder <- file.path('Reports', si_code)

    if (dir.exists(folder)) {
      message(folder, ' folder already exists. skipping creation')
      return(invisible(FALSE))
    } else {

      # 1.1 If folder does not exist, create it
      dir.create(folder, showWarnings = TRUE)
      return(invisible(TRUE))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_report_folder_creation',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_report_folder_creation',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_report_folder_creation',
                                                        sep = '.'))})
}


################################################################################
#' Function to save the psi_data created
#'
#' This function saves the RData files with the psi_data objects
#'
#' @export

# START FUNCTION
# Funtion declaration
write_psi_data <- function(psi_data, folder, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    si_code <- psiQC::get_si_code(psi_data)
    si_code <- unique(si_code)
    path <- file.path(folder, paste0(si_code, '.RData'))

    print(paste0('Writing ', si_code))

    assign(si_code, psi_data)
    save(list = si_code, file = path)

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'write_psi_data',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'write_psi_data',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'write_psi_data',
                                                        sep = '.'))})
}

################################################################################
#' psi_data2csv function
#'
#' This function is used in lvl2_process to write the csv files for each psi_data object
#' slots in the corresponding folder of the database
#'
#' @export
psi_data2csv <- function(psi_data, csv_folder) {

  # get the slots and store them. In the case of data and flags, add the solar timestamp
  # also
  psi_data_df <- psiQC::get_psi(psi_data) %>%
    dplyr::mutate(solar_TIMESTAMP = psiQC::get_solar_timestamp(psi_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  psi_flags <- psiQC::get_psi_flags(psi_data) %>%
    dplyr::mutate(solar_TIMESTAMP = psiQC::get_solar_timestamp(psi_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  site_md <- psiQC::get_site_md(psi_data)
  plant_md <- psiQC::get_plant_md(psi_data)
  question_md <- psiQC::get_question_md(psi_data)
  si_code <- psiQC::get_si_code(psi_data)

  psi_data_name <- file.path(csv_folder, paste0(si_code, '_psi_data.csv'))
  psi_flags_name <- file.path(csv_folder, paste0(si_code, '_psi_flags.csv'))
  site_md_name <- file.path(csv_folder, paste0(si_code, '_site_md.csv'))
  plant_md_name <- file.path(csv_folder, paste0(si_code, '_plant_md.csv'))
  question_md_name <- file.path(csv_folder, paste0(si_code, '_question_md.csv'))

  readr::write_csv(psi_data_df, psi_data_name)
  readr::write_csv(psi_flags, psi_flags_name)
  readr::write_csv(site_md, site_md_name)
  readr::write_csv(plant_md, plant_md_name)
  readr::write_csv(question_md, question_md_name)
}



################################################################################
#' Get the folders to feed \code{\link{qc_start_process}}
#'
#' Get the folders to be able to start QC process
#'
#' No parameters (except for parent_logger) are needed, as the functions simply
#' collects the folders present in \code{Data}.
#'
#' @family Data Flow
#'
#' @return a character vector with the folders route
#'
#' @export

# START
# Function declaration
df_get_data_folders <- function(parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Collect the data folders names
    folder_names <- list.dirs('Data', recursive = FALSE)

    # STEP 2
    # Return the folder names
    return(folder_names)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_get_data_folders',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_get_data_folders',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_get_data_folders',
                                                        sep = '.'))})
}

################################################################################
#' Save the fixed datasets (metadata and data) in the LVL1 folder
#'
#' This function performs three actions: 1) write csv files with the fixed data
#' in the LVL1 folder. 2) Update the status files to indicate that data is in
#' LVL1 and the date of the move. 3) Save the objects generated in the QC to
#' make easy the creation of dashboards without the need of run all QC tests
#' again.
#'
#' @family Data Flow
#'
#' @param si_code Character with the site code
#'
#' @param psi_data Data frame with the fixed psi data
#'
#' @param site_md Data frame with the fixed site metadata
#'
#' @param plant_md Data frame with the fixed plant metadata
#'
#' @param question_md Data frame with the fixed questionnaire data
#'
#' @return Nothing
#'
#' @export

# START
# Function declaration
df_accepted_to_lvl1_psi <- function(si_code, psi_data = NULL, site_md = NULL,
                                    plant_md = NULL, question_md = NULL,
                                    parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # if any of the data is NULL (does not exists), stop and report
    if(any(is.null(psi_data), is.null(site_md), is.null(plant_md),
           is.null(question_md))) {
      stop('One or more datasets were not provided')
    }
    # are datasets dataframes?
    if(any(!is.data.frame(psi_data), !is.data.frame(site_md),
           !is.data.frame(plant_md), !is.data.frame(question_md))) {
      stop('One or more datasets provided are not data frames')
    }
    # is si_code a character string?
    if(!is.character(si_code)) {
      stop('site code provided is not a character string')
    }
    # if files exist before the function execution, stop and inform
    if (all(
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'psi_data.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'site_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'plant_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'question_md.csv', sep = '_')))
    )) {
      stop('csv files already exist in Lvl_1 folder. Please revise manually')
    }

    # STEP 1
    # Writing csv files
    write.csv(psi_data,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'psi_data.csv', sep = '_')),
              row.names = FALSE)
    write.csv(site_md,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'site_md.csv', sep = '_')),
              row.names = FALSE)
    write.csv(plant_md,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'plant_md.csv', sep = '_')),
              row.names = FALSE)
    write.csv(question_md,
              file.path('Data', si_code, 'Lvl_1',
                        paste(si_code, 'question_md.csv', sep = '_')),
              row.names = FALSE)


    # STEP 2
    # Updating status file
    # only if the files have been created
    if (all(
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'psi_data.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'site_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'plant_md.csv', sep = '_'))),
      file.exists(file.path('Data', si_code, 'Lvl_1',
                            paste(si_code, 'question_md.csv', sep = '_')))
    )) {
      df_set_status_psi(si_code,
                    LVL1 = list(STORED = TRUE, DATE = as.character(Sys.Date())))
    } else {
      stop('One or more files has been not created in Lvl_1 folder, please revise manually')
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_accepted_to_lvl1_psi',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_accepted_to_lvl1_psi',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_accepted_to_lvl1_psi',
                                                        sep = '.'))})
}

################################################################################
#' Save the Rmd templates and the running scripts to their corresponding folders
#'
#' Copy the Rmd templates for file transfer and quality check to Template folder and
#' the template for shiny web app to parent directory, and running scripts to parent
#' directory.
#'
#' If it is the first time to set the PSI project, execute this function
#' after the function \code{\link{df_folder_structure}} with the argument
#' \code{first} set to \code{TRUE}
#'
#' @family Data Flow
#'
#' @param first Logical indicating if it is the first time the files are copied
#'
#' @return Logical indicating if all files have been copied or overwritten
#'
#' @export

# START
# Function declaration
df_copy_templates_psi <- function(first = FALSE, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Does 'Templates' directory exists?
    if (!dir.exists('Templates')) {
      stop('Templates directory does not exist')
    }

    # STEP 2
    # If it is the first time, just copy the files
    if(first){

      # Copy templates for file transfer and quality check to Template folder
      file.copy(
        system.file('Rmd_templates', 'received_to_accepted.Rmd',
                    package = 'psiQC'),
        file.path('Templates'), overwrite = FALSE
      )
      file.copy(
        system.file('Rmd_templates', 'QC_report.Rmd',
                    package = 'psiQC'),
        file.path('Templates'), overwrite = FALSE
      )

      # Copy template for shiny web app to parent directory
      # file.copy(
      #   system.file('Rmd_templates', 'psi_monitor.Rmd',
      #               package = 'psiQC'),
      #   file.path('.'), overwrite = TRUE
      # )

      # Copy scripts to parent directory
      file.copy(
        system.file('run_scripts', 'main_script.R',
                    package = 'psiQC'),
        file.path('.'), overwrite = FALSE
      )
      file.copy(
        system.file('run_scripts', 'debug_script.R',
                    package = 'psiQC'),
        file.path('.'), overwrite = FALSE
      )

      return(invisible(TRUE))
    }

    # STEP 3
    # Checks when first is set to FALSE

    # Get the time of last modification for all the files previous to overwritting
    pre_time <- file.mtime(c(file.path('Templates','received_to_accepted.Rmd'),
                             file.path('Templates','QC_report.Rmd'),#'psi_monitor.Rmd',
                             'main_script.R','debug_script.R'))

    # Give an error if modification time of the files can not be obtained
    if(any(is.na(pre_time))){
      stop('Check whether templates and running scripts already exist. ',
           'If some of them do not exist, delete those that exist (if any) ',
           'and run again the function with parameter first set to TRUE')
    }

    # STEP 4
    # Copy templates for file transfer and quality check to Template folder
    file.copy(
      system.file('Rmd_templates', 'received_to_accepted.Rmd',
                  package = 'psiQC'),
      file.path('Templates'), overwrite = TRUE
    )
    file.copy(
      system.file('Rmd_templates', 'QC_report.Rmd',
                  package = 'psiQC'),
      file.path('Templates'), overwrite = TRUE
    )

    # Copy template for shiny web app to parent directory
    # file.copy(
    #   system.file('Rmd_templates', 'psi_monitor.Rmd',
    #               package = 'psiQC'),
    #   file.path('.'), overwrite = TRUE
    # )

    # Copy scripts to parent directory
    file.copy(
      system.file('run_scripts', 'main_script.R',
                  package = 'psiQC'),
      file.path('.'), overwrite = TRUE
    )
    file.copy(
      system.file('run_scripts', 'debug_script.R',
                  package = 'psiQC'),
      file.path('.'), overwrite = TRUE
    )

    # STEP 5
    # Check that all times of last modification have changed
    post_time <- file.mtime(c(file.path('Templates','received_to_accepted.Rmd'),
                              file.path('Templates','QC_report.Rmd'),#'psi_monitor.Rmd',
                              'main_script.R','debug_script.R'))

    if(all(post_time != pre_time)){
      return(invisible(TRUE))
    } else {
      warning("Some Rmd templates or running scripts have not been copied")
      return(invisible(FALSE))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_copy_templates_psi',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_copy_templates_psi',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_copy_templates_psi',
                                                        sep = '.'))})
}

################################################################################
#' Reset the data folder and status file for the si_code given.
#'
#' After manual changes, update the satus file to indicate that QC is needed.
#' Also rename Accepted and Lvl1 data to avoid conflicts with manually
#' changed data.
#'
#' A fast way of reset any site data folder when needed, usually after manual
#' changes of the files.
#' \code{level} parameter allows for selecting to which level the status must
#' be reset. "LVL1" indicates that only LVL1 data and status will be
#' reset. "all" resets all data and status to the initial state for the site.
#'
#' @family Data Flow
#'
#' @param si_code Character vector indicating the site code to reset
#'
#' @param level Character string indicating which level must be reset. Valid
#'   values are "LVL1" and "all"
#'
#' @return Nothing
#'
#' @export

# START
# Function declaration
df_reset_data_status_psi <- function(si_code, level = 'all', parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.character(si_code)) {
      stop('si_code provided is not a character')
    }

    # STEP 1
    # Setting the status

    # 1.1 status lists
    QC = list(DONE = FALSE, DATE = NULL)
    LVL1 = list(STORED = FALSE, DATE = NULL, TO_LVL2 = 'FREEZE')

    # 1.2 set status depending on the level argument
    if (level == 'all') {
      df_set_status_psi(si_code, QC = QC, LVL1 = LVL1,
                    parent_logger = parent_logger)

      # STEP 2
      # Renaming data

      # 2.1 file names, old and new
      old_files <- c(
        list.files(file.path('Data', si_code, 'Accepted'), full.names = TRUE),
        list.files(file.path('Data', si_code, 'Lvl_1'),
                   full.names = TRUE, recursive = TRUE)
      )
    }



    if (level == 'LVL1') {
      df_set_status_psi(si_code, LVL1 = LVL1, parent_logger = parent_logger)

      # STEP 2
      # Renaming data

      # 2.1 file names, old and new
      old_files <- c(
        list.files(file.path('Data', si_code, 'Lvl_1'), full.names = TRUE)
      )
    }

    # 2.1.1 new names, substituting extension for _time.bak
    new_files <- stringr::str_replace_all(
      old_files,
      pattern = "(.csv|.RData|.xlsx|.txt)",
      replace = paste0('_', format(Sys.time(), '%Y%m%d%H%M'), '.bak')
    )

    # 2.2 Rename step
    file.rename(
      from = old_files,
      to = new_files
    )

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_reset_data_status_psi',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_reset_data_status_psi',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_reset_data_status_psi',
                                                        sep = '.'))})
}



################################################################################
#' Load psiData
#'
#' Accesory function to load an specified psiData object
#'
#' Given a site code and a level description, \code{df_read_psiData} will return
#' the selected psiData object from the selected location
#'
#' @family Data Flow
#'
#' @param si_code Site code as a character string
#'
#' @param level Level to read from as a character string
#'
#' @return A psiData object.
#'
#' @export

# START
# Function declaration
df_read_psiData <- function(
  si_code,
  level = c("Lvl_1", "Accepted"),
  parent_logger = 'test'
) {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checking (done by match.arg)
    level <- match.arg(level)


    # STEP 1
    # load the file
    file_name <- file.path('Data', si_code, level,
                           paste0(si_code, '.RData'))

    if (!file.exists(file_name)) {
      stop('psiData for ', si_code, ' and ', level, ' does not exist.')
    } else {
      load(file = file_name)

      # 1.1 Return the psiData object
      return(eval(as.name(si_code)))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_load_psiData',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_load_psiData',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_load_psiData',
                                                        sep = '.'))})
}


################################################################################
#' Who is ready the desired level?
#'
#' Check the site status files to list who is ready to move to target
#'
#' This function check the status files of all sites and list the status
#' for the desired level.
#' \code{filter} parameter can be used to filter the results by any status:
#'
#'   \itemize{
#'     \item{\code{all} retrieves all the statuses}
#'     \item{\code{ready} retrieves only those sites marked to pass to level 2}
#'     \item{\code{freeze} retrieves only those sites freezed in level 1 yet}
#'   }
#'
#' @family Data Flow
#'
#' @param level string indicating the level to check. Accepted values are
#'   "lvl2".
#'
#' @param filter character vector indicating by which status results must
#'   been filtered. Accepted values are "all" (default), "ready" or "freeze"
#'   (see details)
#'
#' @return A list with length equal to the number of sites containing the
#'   \code{TO_[level]} flag of the status files.
#'
#' @export

# START
# Function declaration
df_whos_ready_to <- function(level = c('lvl2'),
                             filter = c('all', 'ready', 'freeze'),
                             parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Checking arguments (match.arg throws an error if not matching)
    level <- match.arg(level)
    level <- switch(level,
                    lvl2 = c('LVL1', 'TO_LVL2'))

    filter <- match.arg(filter)
    filter <- switch(filter,
                     all = 'all',
                     ready = 'READY',
                     freeze = 'FREEZE')

    # STEP 1
    # Getting the site codes to pass to df_get_status
    site_folders <- df_get_data_folders(parent_logger = parent_logger) %>%
      stringr::str_sub(6, -1)

    # STEP 2
    # Get the statuses
    whos_ready <- site_folders %>%
      purrr::map(df_get_status, parent_logger = parent_logger) %>%
      # STEP 3
      # Get the TO_LVL2 flag
      purrr::modify_depth(1, level, .null = NA)

    # STEP 3
    # Prepare the results
    # 3.1 Name the list elements
    names(whos_ready) <- site_folders
    # 3.2 filter the results
    if (filter != 'all') {
      whos_ready <- whos_ready[whos_ready == filter]
    }

    # STEP 4
    # Return the list
    return(whos_ready)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_whos_ready_to',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_whos_ready_to',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_whos_ready_to',
                                                        sep = '.'))})
}


################################################################################
#' Create a PsiData object from results of Quality Checks
#'
#' This function gather all the data and metadata generated in the QC process
#' and build an PsiData class object
#'
#' \code{psi_data_constructor} function generates a PsiData object containing
#' all relevant data and metadata for a site. Add flags to psi_data as
#' \code{NA_ADDED}. Original NAs are also flagged as \code{NA_PRESENT}. For
#' info about the available slots see \code{\link{PsiData}},
#' \code{\link{psi_get_methods}} and \code{\link{psi_replacement}}
#'
#' @family Data Flow
#'
#' @param psi_data Data frame with metadata and psi data after QC process
#'
#' @param question_md Data frame with questionnaire data after QC process
#'
#' @param site_md Data frame with site metadata after QC process
#'
#' @param plant_md Data frame with plant metadata after QC process
#'
#' @param methods_md Data frame with methods metadata after QC process
#'
#' @param solar_timestamp Vector with solar timestamp
#'
#' @return A PsiData object with all the data and metadata of the site
#'
#' @export

# START
# Function declaration
psi_data_constructor <- function(psi_data = NULL, site_md = NULL,
                                 plant_md = NULL, question_md = NULL,
                                 solar_timestamp = NULL, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (any(
      !is.data.frame(psi_data), !is.data.frame(question_md)
    )) {
      stop('Data and/or metadata objects provided are not data.frames')
    }

    if (site_md$site_name %>% unique() %>% length() > 1) {
      stop('There is more than one different site name')
    }

    # STEP 1
    # 1.1 Get timestamp from psi_data
    psi_timestamp <- psi_data %>% dplyr::select(timestamp)


    # 1.2 flags indicating the pre-existent NAs and the new added NAs
    .psi_flags <- psi_data[,-1] %>%
      is.na() %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(~replace(., which(. == TRUE), "NA_PRESENT")) %>%
      dplyr::mutate_all(~replace(., which(. == FALSE), ""))


    # 1.3 empty solar timestamp
    if (is.null(solar_timestamp)) {
      .solar_timestamp <- as.POSIXct(rep(NA, length(psi_timestamp[[1]])))
    } else {
      .solar_timestamp <- solar_timestamp
    }

    # STEP 2
    # Build the PsiData object and return it
    res <- psiData(
      psi_data = psi_data[, -1],
      psi_flags = .psi_flags,
      timestamp = psi_timestamp[[1]],
      solar_timestamp = .solar_timestamp,
      si_code = rep(site_md$id_sfn %>% unique(), length(psi_timestamp[[1]])),
      site_md = site_md,
      plant_md = plant_md,
      question_md = question_md
    )

    # 2.1 Return it!!
    return(res)
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'psi_data_constructor',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'psi_data_constructor',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'psi_data_constructor',
                                                        sep = '.'))})
}
