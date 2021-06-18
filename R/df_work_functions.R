#' Read psi_data from disk
#'
#' Given a site code and a route, \code{read_psi_data} will return the selected
#' psi_data object
#'
#' @param site_code A character vector with one site code
#'
#' @param folder Route to the folder containing the \code{.RData} file. Default
#'   to working directory.
#'
#' @return an psi_data object with the selected site data.
#'
#' @examples
#' # Let's access the data in "folder". This typically is the folder where the
#' # psi data is, but in this example we will create a temporal folder with some
#' # data to test the function
#' folder <- tempdir()
#' save(ARG_TRE, file = file.path(folder, 'ARG_TRE.RData'))
#' save(ARG_MAZ, file = file.path(folder, 'ARG_MAZ.RData'))
#'
#' # now we read a single site
#' ARG_TRE_test <- read_psi_data('ARG_TRE', folder)
#' ARG_TRE_test
#'
#'
#' @export

read_psi_data <- function(site_code, folder = '.') {

  # if more than one site we need to map the call
  if (length(site_codes) > 1) {
    stop("site_code must be unique")
  }

  # one site, we need to find it and load it
  file_name <- file.path(folder, paste0(site_code, '.RData'))

  if (!file.exists(file_name)) {
    stop(folder, ' folder does not contain any file called ', site_code, '.RData')
  } else {
    load(file = file_name)

    # load will load in the function environment a SITE_CODE object,
    # we need to access to it to return it
    return(eval(as.name(site_code)))
  }

}




################################################################################
#' QC2 function, cleaning a little
#'
#' Function to final clean the data and generate the psiQC::psi_data
#' objects
#'
#' This function looks for LVL1 completed data and performs
#' the last cleaning and the psi_data construction. See as_psi_data for more
#' details
#'
#' @export

lvl2_process <- function(version = '0.0.1', parent_logger = 'test') {

  # get the sites ready to lvl2
  sites <- names(psiQC::df_whos_ready_to('lvl2', 'ready'))

  # folders
  folder_plant <- file.path('..', 'psi_db', version, 'RData')
  csv_folder <- file.path('..', 'psi_db', version, 'csv')


  # big loop
  for (site in sites) {


    df_read_psiData(
        site, 'Lvl_1', parent_logger = parent_logger
      ) -> psi_data

      write_psi_data(psi_data, folder = folder_plant)
      psi_data2csv(psi_data, csv_folder = csv_folder_plant)

    # set status
    df_set_status(site, LVL1 = list(TO_LVL2 = 'DONE'))

  }

}

