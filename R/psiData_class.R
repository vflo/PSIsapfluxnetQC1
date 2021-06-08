#' An S4 class for psi data
#'
#' @slot psi_data A data frame with the psi data
#'
#' @slot psi_flags A data frame with the same dimensions of \code{psi_data}
#'   with the flag info for each tree/TIMESTAMP combination
#'
#' @slot si_code A character vector of length \code{nrow(psi_data)} indicating
#'   the site code
#'
#' @slot timestamp A POSIXct vector of length \code{nrow(psi_data)} with the
#'   timestamp
#'
#' @slot solar_timestamp A POSIXct vector of length \code{nrow(psi_data)} with
#'   the solar timestamp
#'
#' @slot site_md A data frame containing the site metadata
#'
#' @slot plant_md A data frame containing the plant metadata
#'
#' @import methods
#' @export psiData
#' @exportClass psiData

psiData <- setClass(
  'psiData',
  slots = list(
    psi_data = "data.frame",
    psi_flags = "data.frame",
    si_code = "character",
    timestamp = "POSIXt",
    solar_timestamp = "POSIXt",
    site_md = "data.frame",
    plant_md = "data.frame",
    question_md = "data.frame"
  )
)
