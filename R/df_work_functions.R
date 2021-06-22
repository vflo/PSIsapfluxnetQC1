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
  if (length(site_code) > 1) {
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
#' QC1 function
#'
#' Function to perform level 1 data and generate reports
#'
#' This function looks for sites without LVL1 process done and performs
#' level 1 QC process
#'
#' @export


lvl1_process <- function(){

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

  status <- purrr::map(data_folders, function(y){
    name <- gsub("Data/", "", y)
    status <- df_get_status(name)

    df <- tibble(site_code = name,
                 status = status$QC$DONE)
  }) %>% bind_rows()

  if(status[!status$status,] %>% pull(site_code) %>% length() == 0){
    stop('There is no site to perform level 1 process. All sites have level 1 process done.')
  }

  message("Available datasets to perform lvl 1 process:\n",
          paste0(capture.output(status[!status$status,] %>% pull(site_code)),
                 collapse = "\n"))

  ## data set selection by user
  fun <- function() {
    ANSWER <- readline(
      prompt="Please write the site code to start process or write ALL to start process on all available data sets:")
    while(all(!ANSWER %in% (status[!status$status,] %>% pull(site_code)), !grepl("ALL", ANSWER, ignore.case=TRUE))){
      cat("The provided site code is not valid")
      ANSWER <- readline(
        prompt="Please write the site code to start process or write ALL to start process on all available data sets:")
    }
    return(ANSWER)

  }

  fun() -> answer

  ## Data set filter
  if(grepl("ALL", answer, ignore.case=TRUE)){
    folders <- data_folders
  }else{
    folders <- data_folders[grepl(answer, status %>% pull(site_code))]
  }

  ## Loop for every site
  lapply(folders, function(folder) {
    code <- stringr::str_sub(folder, 6, -1)
    # log_psi_setup('Logs/psi.log',
    #                      logger = paste('QC', code, sep = '.'),
    #                      level = "DEBUG")
    qc_start_process_psi(file.path(folder, 'Accepted'), rdata = FALSE,
                         parent_logger = paste('QC', code, sep = '.'))
  })



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
  folder <- file.path('..', 'psi_db', version, 'RData')
  csv_folder <- file.path('..', 'psi_db', version, 'csv')


  # big loop
  for (site in sites) {


    df_read_psiData(
        site, 'Lvl_1', parent_logger = parent_logger
      ) -> psi_data_obj

      write_psi_data(psi_data_obj, folder = folder)
      psi_data2csv(psi_data_obj, csv_folder = csv_folder)

    # set status
    df_set_status_psi(site, LVL1 = list(TO_LVL2 = 'DONE'))

  }

}






################################################################################
#' PSI tidyfier
#'
#' Function to transform psiData objects to tidy format
#'
#' @param psi_data A psiData object
#'
#' @param include_flags A logical object. Set to TRUE if data flags should be
#' included in the returned object
#'
#' @param include_question A logical object. Set to TRUE if data sets questionnaire
#' data should be included in the returned object
#'
#' @return tibble object
#'
#' @export

psi_tidyfier <- function(psi_data, include_flags = FALSE,
                         include_question = FALSE, parent_logger = 'test') {

  # get psi data
  psi <- get_psi(psi_data)

  # get site data
  site <- get_site_md(psi_data)

  # get plant data
  plant <- get_plant_md(psi_data)

  # get flags?
  if(include_flags){
    flags <- get_psi_flags(psi_data)
  }else{flags <- NULL}

  # get question?
  if(include_question){
    question <- get_question_md(psi_data)
    }else{question <- NULL}


  res <- cbind(psi, plant %>% dplyr::select(-pl_code), site, row.names = NULL)

  if(length(flags) > 0) {
    cbind(res, flags, row.names = NULL)
  }

  if(length(question) > 0) {
    cbind(res, question, row.names = NULL)
  }

  return(res)

  }

