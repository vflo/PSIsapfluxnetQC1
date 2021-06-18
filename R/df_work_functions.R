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


