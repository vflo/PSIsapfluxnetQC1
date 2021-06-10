################################################################################
#' Solar time conversion
#'
#' Calculate the Extraterrestrial Radiation from the TIMESTAMP
#'
#' This function uses several functions from \code{solaR} package in order to
#' obtain the mean solar time, the equation of time for each day included in the
#' TIMESTAMP and the extraterrestrial radiation for each step of the TIMESTAMP.
#'
#' @section Apparent (Real) Solar Time:
#' The Apparent Solar Time is calculated as:
#' \deqn{Apparent Solar Time = Mean Solar Time + Equation of Time}
#' The Equation of Time is calculated for each day, whereas the Mean Solar Time
#' is calculated for each step of the TIMESTAMP.
#'
#' @family Quality Checks Functions
#'
#' @param data Environmental data frame containing the TIMESTAMP variable.
#'
#' @param site_md Data frame containing the latitude and longitude variables of
#'   the site (\code{lat} and \code{lon})
#'
#' @param add_solar_ts Logical indicating if solar timestamp must be added to
#'   the environmental data frame.
#'
#' @return A data frame exactly as \code{data}, but with an additional column
#'   containing the extraterrestrial radiation in W/m2, and optionally another
#'   column containing apparent solar timestamp.
#'
#' @export

# START
# Function declaration
qc_ext_radiation <- function(data, site_md, add_solar_ts = FALSE,
                             parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Are data and site_md data frames?
    if (any(!is.data.frame(data), !is.data.frame(site_md))) {
      stop('data and/or site_md are not data frames')
    }
    # have data the timestamp variable?
    if (is.null(data$timestamp)) {
      stop('data has not a timestamp variable')
    }
    # have metadata objects the mandatory variables?
    if (any(is.null(site_md$lat), is.null(site_md$lon))) {
      stop('site_md has not the needed variables. ',
           'See function help (?qc_solar_timestamp)')
    }
    # Is type a valid value?
    if (!is.logical(add_solar_ts)) {
      stop('add_solar_ts must be either TRUE or FALSE')
    }

    # STEP 1
    # Retrieve the accessory info
    lat <- site_md$lat %>% unique()
    long <- site_md$lon %>% unique()
    timestamps <- data$timestamp

    purrr::map(timestamps, function(timestamp){

    # STEP 2
    # Intermediate objects
    # 2.2 Mean Solar Time
    mst <- solaR::local2Solar(timestamp, long)

    # 2.2.1 warning if solartimestamp has repeated values (due to rounding)
    if (length(mst) != length(unique(mst))) {
      warning('solar mean time generates repeated timestamps. ',
              'Please revise the lat and lon for repeated values.')
    }

    # STEP 3
    # Calculating Apparent Solar Time (Mean Solar Time + Equation of Time)
    # 2.1 Equation of time
    solD <- solaR::fSolD(lat, mst)
    EoT <- solaR::r2sec(solD$EoT)

    ast <- lapply(as.Date(strptime(zoo::index(EoT), format = '%Y-%m-%d')),
                  function(id, vect)
                    (vect[as.Date(vect) == id] +
                       zoo::coredata(EoT)[which(as.Date(strptime(zoo::index(EoT),
                                                                 format = '%Y-%m-%d')) == id)]),
                  vect = mst)

    ast <- do.call("c", ast)

    solD <- solaR::fSolD(lat, ast)
    solI <- zoo::coredata(solaR::fSolI(solD, BTi = ast)$Bo0)

    res <- data.frame(solI,ast)

    return(res)

    }) %>% bind_rows() -> res

    data$ext_rad <- res$solI


    if (add_solar_ts) {
      data$solartimestamp <- res$ast
    }

    # STEP 4
    # Return data frame with new columns
    return(data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_ext_radiation',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_ext_radiation',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_ext_radiation',
                                                        sep = '.'))})
}

