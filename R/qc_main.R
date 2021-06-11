################################################################################
#' Main function to resume Metadata QC in one data frame
#'
#' Metadata QC codified results in one data frame
#'
#' @family Quality Checks Functions
#'
#' @param md_cols
#'
#' @param factor_values
#'
#' @param email_check
#'
#' @param site_md_coordfix
#'
#' @param plant_md_spnames
#'
#' @return A data frame with the highlights of the QC
#'
#' @export

# START
# Function declaration
qc_md_results_table <- function(md_cols, factor_values,
                                email_check, site_md_coordfix, plant_md_spnames,
                                parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 1
    # Create the result vectors
    step <- vector()
    status <- vector()
    description <- vector()

    # STEP 2
    # Metadata columns
    # 2.1 Presence
    if (any(!md_cols$PresenceOK)) {
      step <- c(step, 'Metadata variables presence')
      status <- c(status, 'ERROR')
      description <- c(description, 'One or more variables are missing from metadata')
    } else {
      step <- c(step, 'Metadata variables presence')
      status <- c(status, 'PASS')
      description <- c(description, 'All metadata variables are present')
    }

    # 2.2 ClassOK
    if (any(is.na(md_cols$anyNA)) | any(is.na(md_cols$ClassOK))) {
      step <- c(step, 'Metadata variables expected class')
      status <- c(status, 'WARNING')
      description <- c(description, 'One or more variables are missing from metadata and class check is unfeasible')
    } else {
      if (any(!md_cols$ClassOK & !md_cols$anyNA)) {
        step <- c(step, 'Metadata variables expected class')
        status <- c(status, 'ERROR')
        description <- c(description, 'One or more variables have the wrong class')
      } else {
        step <- c(step, 'Metadata variables expected class')
        status <- c(status, 'PASS')
        description <- c(description, 'All metadata variables have the correct class')
      }
    }

    # 2.3 NAs
    if (any(is.na(md_cols$anyNA))) {
      step <- c(step, 'Metadata variables NA presence')
      status <- c(status, 'WARNING')
      description <- c(description, 'One or more variables are missing from metadata and NA check is unfeasible')
    } else {
      if (any(md_cols$anyNA)) {
        step <- c(step, 'Metadata variables NA presence')
        status <- c(status, 'INFO')
        description <- c(description, 'Some variables have no value')
      } else {
        step <- c(step, 'Metadata variables NA presence')
        status <- c(status, 'PASS')
        description <- c(description, 'No NAs in metadata')
      }
    }

    # 2.4 Unique values
    if (any(is.na(md_cols$UniqueValue))) {
      step <- c(step, 'Metadata variables unique value')
      status <- c(status, 'PASS')
      description <- c(description, 'All metadata variables have a unique value')
    } else {
      if (any(md_cols$UniqueValue)) {
        step <- c(step, 'Metadata variables unique value')
        status <- c(status, 'INFO')
        description <- c(description, 'Some variables have no value')
      } else {
        step <- c(step, 'Metadata variables unique value')
        status <- c(status, 'WARNING')
        description <- c(description, 'There is some metadata variables with more than one unique value')
      }
    }

    # STEP 3
    # Metadata factor values
    # 3.1 Wrong value
    if (any(!factor_values)) {
      step <- c(step, 'Metadata factor variable values')
      status <- c(status, 'ERROR')
      description <- c(description, 'One or more metadata factor variables have values not accepted')
    } else {
      step <- c(step, 'Metadata factor variable values')
      status <- c(status, 'PASS')
      description <- c(description, 'All factor variables have accepted values')
    }

    # STEP 4
    # Metadata email
    if (all(is.na(email_check$Is_correct)) | any(!email_check$Is_correct, na.rm = TRUE)) {
      step <- c(step, 'Email check')
      status <- c(status, 'WARNING')
      description <- c(description, 'Email is missing or in wrong format')
    } else {
      step <- c(step, 'Email check')
      status <- c(status, 'PASS')
      description <- c(description, 'Correct email format')
    }

    # STEP 5
    # Coordinates check
    if (!site_md_coordfix$is_inside_country) {
      step <- c(step, 'Site coordinates')
      status <- c(status, 'WARNING')
      description <- c(description, 'Site provided coordinates are incorrect and not fixable')
    } else {
      step <- c(step, 'Site coordinates')
      status <- c(status, 'PASS')
      description <- c(description, 'Site provided correct or fixable coordinates')
    }



    # 6.2 plant md
    if (any(!plant_md_spnames$Concordance)) {
      step <- c(step, 'Species names spelling (plant_md)')
      status <- c(status, 'WARNING')
      description <- c(description, 'Species names in Plant metadata are mispelled')
    } else {
      step <- c(step, 'Species names spelling (plant_md)')
      status <- c(status, 'PASS')
      description <- c(description, 'No mispelling in species names')
    }


    # STEP 9
    # Create the results data frame
    res <- data.frame(QC_Step = step, Status = status, Description = description,
                      stringsAsFactors = FALSE)

    # STEP 10
    # Return the datatable
    res_table <- DT::datatable(
      res, class = 'display', rownames = FALSE,
      extensions = c('Scroller'),
      caption = 'Metadata Quality Check Summary',
      options = list(dom = 't',
                     columnDefs = list(list(className = 'dt-center',
                                            targets = 1),
                                       list(className = 'dt-right',
                                            targets = 0),
                                       list(width = '45%',
                                            targets = c(0, 2)),
                                       list(width = '10%',
                                            targets = 1)),
                     scrollY = 750, scrollCollapse = TRUE)
    ) %>%
      DT::formatStyle('Status',
                      backgroundColor = DT::styleEqual(c('PASS', 'INFO',
                                                         'WARNING', 'ERROR'),
                                                       c('#26a65b', '#89c4f4',
                                                         '#f39c12', '#d91e18')))

    return(res_table)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_md_results_table',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_md_results_table',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_md_results_table',
                                                        sep = '.'))})


}

################################################################################
#' Main function to resume Data QC in one data frame
#'
#' Data QC codified results in one data frame
#'
#' @family Quality Checks Functions
#'
#' @param psi_data_fixed
#'
#' @param psi_timestamp_nas
#'
#' @export

# START
# Function declaration
qc_data_results_table <- function(psi_data_fixed, psi_timestamp_nas,
                                  parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Create the result vectors
    step <- vector()
    status <- vector()
    description <- vector()

    # STEP 2
    # Timestamps
    # 2.1 correct format psi
    if (!qc_is_timestamp(psi_data_fixed, FALSE, parent_logger = parent_logger)) {
      step <- c(step, 'TIMESTAMP Format psi data')
      status <- c(status, 'ERROR')
      description <- c(description, 'TIMESTAMP format is incorrect and unfixable')
    } else {
      step <- c(step, 'TIMESTAMP Format psi data')
      status <- c(status, 'PASS')
      description <- c(description, 'TIMESTAMP format is correct or has been fixed')
    }

    # 2.2 TIMESTAMP NAs psi
    if (is.logical(psi_timestamp_nas)) {
      step <- c(step, 'psi TIMESTAMP NAs presence')
      status <- c(status, 'PASS')
      description <- c(description, 'No NAs detected in TIMESTAMP')
    } else {
      step <- c(step, 'psi TIMESTAMP NAs presence')
      status <- c(status, 'ERROR')
      description <- c(description, 'TIMESTAMP has NAs')
    }

    # 2.3 psi out of range
    is_out_range <- any(psi_data_fixed$psi <= (-10))
    is_positive <- any(psi_data_fixed$psi > 0)

    if (all(!is_out_range, !is_positive)) {
      step <- c(step, 'psi values within range')
      status <- c(status, 'PASS')
      description <- c(description, 'No positive or extremely low psi values')
    } else{
        if (is_positive){
          step <- c(step, 'psi positive values')
          status <- c(status, 'ERROR')
          description <- c(description, 'There is at least one positive psi value')
        } else{
          step <- c(step, 'psi values extremely low')
          status <- c(status, 'WARNING')
          description <- c(description, 'extremely low psi values')
        }
    }

    # FINAL STEP
    # create the results object
    res <- data.frame(
      QC_Step = step, Status = status, Description = description,
      stringsAsFactors = FALSE
    )

    # return the table
    res_table <- DT::datatable(
      res, class = 'display', rownames = FALSE,
      extensions = c('Scroller'),
      caption = 'Data Quality Check Summary',
      options = list(dom = 't',
                     columnDefs = list(list(className = 'dt-center',
                                            targets = 1),
                                       list(className = 'dt-right',
                                            targets = 0),
                                       list(width = '45%',
                                            targets = c(0, 2)),
                                       list(width = '10%',
                                            targets = 1)),
                     scrollY = 750, scrollCollapse = TRUE, scroller = TRUE)
    ) %>%
      DT::formatStyle('Status',
                      backgroundColor = DT::styleEqual(c('PASS', 'INFO',
                                                         'WARNING', 'ERROR'),
                                                       c('#26a65b', '#89c4f4',
                                                         '#f39c12', '#d91e18')))

    return(res_table)

    # END FUNCTION

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_data_results_table',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_data_results_table',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_data_results_table',
                                                        sep = '.'))})
}

################################################################################
#' Start QC process
#'
#' Start QC process from the site code.
#'
#' This function check the status of the site, starts it if it does not exists,
#' and start the QC process
#'
#' @family Quality Checks Functions
#'
#' @param folder Character string with the route to the folder to start QC
#'   process
#'
#' @param rdata Logical indicating if objects created in the QC must be saved in
#'   a file
#'
#' @return Invisible TRUE if all the process is ok, and invisible FALSE if there
#'   was some error in the process
#'
#' @export

# START
# Function declaration
qc_start_process_psi <- function(folder = '.', rdata = TRUE,
                             parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if(!is.character(folder)) {
      stop('folder provided is not a character string')
    }

    # STEP 1
    # Get the files names, code and status of the site
    code_and_files <- dl_get_si_code_psi(folder, parent_logger = parent_logger)
    status <- df_get_status(code_and_files[['site_code']],
                            parent_logger = parent_logger)

    # 1.1 Info message
    message('Starting process for ', code_and_files[['site_code']], ' site')

    # STEP 2
    # 2.1 if status exists and QC is DONE, don't do anything
    if (!is.logical(status)) {
      if(status$QC$DONE) {
        message(code_and_files[['site_code']],
                ' already passed QC, not doing anything else')
        return(invisible(FALSE))
      } else {

        # 2.2 if status exists but QC is not DONE, lets do it
        # 2.2.1 report folder
        df_report_folder_creation(code_and_files[['site_code']],
                                  parent_logger = parent_logger)

        # 2.2.2 report
        rep_psi_render('QC_report.Rmd',
                       output_file = file.path(
                         paste(format(Sys.time(), '%Y%m%d%H%M'),
                               code_and_files[['site_code']],
                               'QC_report.html', sep = '_')
                       ),
                       output_dir = file.path('Reports',
                                              code_and_files[['site_code']]),
                       parent_logger = parent_logger,
                       md_file = code_and_files[['md_file']],
                       psi_data_file = code_and_files[['psi_file']],
                       code = code_and_files[['site_code']],
                       rdata = rdata)

        # 2.2.3 set status
        df_set_status_psi(code_and_files[['site_code']],
                      QC = list(DONE = TRUE, DATE = as.character(Sys.Date())),
                      parent_logger = parent_logger)

        # 2.2.4 return invisible TRUE
        return(invisible(TRUE))
      }

    } else {

      # 2.3 If status does not exist, create it and perform the QC
      # 2.3.1 start status
      df_start_status_psi(code_and_files[['site_code']], parent_logger = parent_logger)

      # 2.3.2 log setup
      log_psi_setup('Logs/psi.log',
                           logger = code_and_files[['site_code']],
                           level = "DEBUG")

      # 2.3.3 report folder
      df_report_folder_creation(code_and_files[['site_code']],
                                parent_logger = parent_logger)

      # 2.3.4 report
      rep_psi_render('QC_report.Rmd',
                     output_file = file.path(
                       paste(format(Sys.time(), '%Y%m%d%H%M'),
                             code_and_files[['site_code']],
                             'QC_report.html', sep = '_')
                     ),
                     output_dir = file.path('Reports',
                                            code_and_files[['site_code']]),
                     parent_logger = parent_logger,
                     md_file = code_and_files[['md_file']],
                     sapf_data_file = code_and_files[['sapf_file']],
                     env_data_file = code_and_files[['env_file']],
                     code = code_and_files[['site_code']],
                     rdata = rdata)

      # 2.3.5 set status
      df_set_status_psi(code_and_files[['site_code']],
                    QC = list(DONE = TRUE, DATE = as.character(Sys.Date())),
                    parent_logger = parent_logger)

      # 2.3.8 return invisible TRUE
      return(invisible(TRUE))

    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_start_process',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_start_process',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_start_process',
                                                        sep = '.'))})
}
