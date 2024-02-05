################################################################################
#' TimeStamp format check
#'
#' Function to check if the format of the timestamp is the correct one
#'
#' Template timestamp is requiered to comply POSIXct format. This function
#' checks if the contributor followed the mandatory format.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the data (sapflow or environmental). Also
#'   a vector with the timestamp values. If data frame, \bold{it must contain} a
#'   timestamp variable
#'
#' @param verbose Logical indicating if messages of success and warnings of
#'   failures must be presented. Default is \code{TRUE}. In order to use
#'   inside another function, is recommended to set \code{verbose = FALSE}.
#'
#' @return A message/warning indicating if timestamp is correct or not. Also
#'   an invisible logical object is returned, indicating success (TRUE) or
#'   failure (FALSE).
#'
#' @import lubridate
#'
#' @export

# START
# Function declaration
qc_is_timestamp <- function(data, verbose = TRUE,
                            parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checking
    # is data a data frame?
    if(!is.data.frame(data) & !is.vector(data) & class(data)[1] != 'POSIXct') {
      stop('Data provided is not a data frame or a vector')
    }

    # STEP 1
    # Data frame
    if (is.data.frame(data)) {
      # have data a timestamp variable?
      if(is.null(data$timestamp)) {
        stop('timestamp variable is missing in the data provided')
      }

      # is all timestamp NAs?
      if (all(is.na(data$timestamp))) {
        if (verbose) {warning('WARNING: timestamp is all NAs')}
        return(invisible(FALSE))
      }

      # Check timestamp format
      if (lubridate::is.POSIXt(data$timestamp)) {
        if (verbose) {message('timestamp is in the correct format')}
        return(invisible(TRUE))
      } else {
        if (verbose) {warning('WARNING: timestamp is NOT in the correct format')}
        return(invisible(FALSE))
      }
    } else {

      # STEP 2
      # Vector
      # is all vector NA?
      if (all(is.na(data))) {
        if (verbose) {warning('WARNING: timestamp is all NAs')}
        return(invisible(FALSE))
      }

      if (lubridate::is.POSIXt(data)) {
        if (verbose) {message('timestamp is in the correct format')}
        return(invisible(TRUE))
      } else {
        if (verbose) {warning('WARNING: timestamp is NOT in the correct format')}
        return(invisible(FALSE))
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_is_timestamp', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_is_timestamp', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_is_timestamp', sep = '.'))})

}

################################################################################
#' Timezones from coordinates
#'
#' Get timezones from coordinates using lutz package and tranforms timezone ISO
#' code to character vector compatible with lubridate and POSIXct
#'
#' GMT time zones are used, as they are day saving light time (DST) agnostic,
#' and in that way the DST can setted if the metadata says so. GMT are sign
#' exchanged to be compatible with ISO.
#'
#' @family Quality Checks Functions
#'
#' @param coord_lat  numeric vector of latitudes
#'
#' @param coord_lon  numeric vector of longitudes the same length as x
#'
#' @return A character vector with the timezone code compatible with lubridate
#'   and as.POSIXct
#'
#' @import lutz
#'
#' @export

# START
# Function declaration
qc_get_timezone_coord <- function(coord_lat, coord_lon, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checking
    if (any(is.na(coord_lat) | is.null(coord_lat) |
        is.na(coord_lon) | is.null(coord_lon))) {
      stop('Some or all coordinates are not provided in metadata')
    }

    # STEP 1
    # Create the list with the codes

    timezone <- lutz::tz_lookup_coords(coord_lat, coord_lon, method = "accurate")

    # STEP 2
    # Return the timezone name compatible with lubridate
    return(timezone)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_get_timezone', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_get_timezone', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_get_timezone', sep = '.'))})
}

################################################################################
#' Set the timezone of the timestamp
#'
#' Brute force convert of timezone
#'
#' When reading data from xlsx or csv, timestamp is readed as POSIXct and by
#' default the timezone is UTC. With this function timezone can be changed
#' without change the timestamp. This is made with the \code{force_tz} function
#' of the lubridate package.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame with the timestamp variable to set, or a POSIXct
#'   vector.
#'
#' @param tz Character vector with the compatible name of the timezone, as the
#'   one provided by the \code{\link{qc_get_timezone}} function.
#'
#' @return A data frame as the \code{data} provided, with the timestamp variable
#'   associated to the timezone specified
#'
#' @import lubridate
#'
#' @export

# START
# Function declaration
qc_set_timezone <- function(data, tz, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # is data a data frame?
    if (!is.data.frame(data) & !is.vector(data) & class(data)[1] != 'POSIXct') {
      stop('data is not a data frame or a POSIX vector')
    }

    # STEP 1
    # Data frame
    if (is.data.frame(data)) {

      # 1.1 has data a timestamp variable
      if (is.null(data)) {
        stop('data has not a timestamp variable')
      }

      # 1.2 Force the timezone
      data$timestamp <- lubridate::force_tzs(data$timestamp, tz, tzone_out = "UTC")

      # 1.3 Return the data
      return(data)

    } else {
      # STEP 2
      # Vector

      # 2.1 force the timezone
      data <- lubridate::force_tzs(data, tz, tzone_out = "UTC")

      # 2.2 return the results
      return(data)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_set_timezone', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_set_timezone', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_set_timezone', sep = '.'))})


}

################################################################################
#' Convert known bad formats to correct timestamp format and set timezone
#'
#' Converting known bad timestamp formats to POSIXt and setting the correct
#' timezone
#'
#' When loading data from csv files, depending on the office version and
#' workflow to introduce timestamp and data, timestamp can result in the wrong
#' format (lost of seconds, for example). This function checks for known
#' formatting errors and try to fix them.
#'
#' @section Timezone:
#' This function also set the timezone attribute to the POSIXt timestamp.
#' It uses \code{\link{qc_get_timezone}} and \code{\link{qc_set_timezone}}
#' functions internally to get the timezone from the \code{env_time_zone}
#' variable, transforming it to a compatible timezone name and set it as a
#' POSIXt attribute.
#'
#' @family Data Loading Functions
#'
#' @param data Data frame containing timestamp variable. Also it can be a vector
#'   with timestamp values
#'
#' @return An object of the same type of input (data frame or vector) with the
#'   fixed values of timestamp. If timestamp is already in format, a message
#'   appears and none fix is made, returning data as entered.
#'   If timestamp can not be fixed, an error is raised.
#'
#' @import lubridate
#'
#' @export

# START
# Function declaration
qc_as_timestamp <- function(data, site_data, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checking
    # Data is a vector or a data frame
    if (!is.data.frame(data) & !is.vector(data) & class(data)[1] != 'POSIXct') {
      stop('Data is not a data frame or a vector')
    }

    # STEP 1
    # Data frame
    if (is.data.frame(data)) {
      # Data contains a timestamp variable?
      if (is.null(data$timestamp)) {
        stop('Data have no timestamp variable')
      }
      timestamp <- data$timestamp
      timezone <- qc_get_timezone_coord(site_data$lat %>% unique(),
                                        site_data$lon %>% unique(),
                                        parent_logger = parent_logger)

      # 1.1 if already in format, inform and return the data unaltered
      if (qc_is_timestamp(data, verbose = FALSE,
                          parent_logger = parent_logger)) {

        # 1.1.1 Set the timezone
        res <- qc_set_timezone(data, timezone, parent_logger = parent_logger)

        message(paste('timestamp already in format. Timezone set to ',
                      timezone, sep = ''))
        return(res)
      } else {

        # 1.2 If not in format, try to fix it using the known bad formats
        res <- lubridate::parse_date_time(
          timestamp,
          # orders, now we dont need to indicate no seconds, we can use truncate
          c('dmY HMS', 'Ymd HMS'), truncated = 1,
          tz = timezone
          )

      }

      # 1.3 Check if the fix worked. If yes, message and return data
      # with the new timestamp
      if (qc_is_timestamp(res, verbose = FALSE,
                          parent_logger = parent_logger)) {
        message('timestamp succesfully fixed. A sample: ', res[1])
        data$timestamp <- res
        return(data)
      } else {
        stop('Unable to format correctly the timestamp, please ',
              'revise manually.')
      }
    } else {

      # STEP 2
      # Vector
      # 2.1 If already in format, inform and return the data unaltered
      timezone <- qc_get_timezone_coord(site_data$lat %>% unique(),
                                        site_data$lon %>% unique(),
                                        parent_logger = parent_logger)

      if (qc_is_timestamp(data, verbose = FALSE)) {

        # 2.2 set the timezone
        res <- qc_set_timezone(data, timezone, parent_logger = parent_logger)

        message(paste('timestamp already in format. Timezone set to ',
                      timezone, sep = ''))
        return(res)
      } else {

        # 2.3 If not in format, try to fix it using the known bad formats
        res <- lubridate::parse_date_time(
          data,
          # orders, now we dont need to indicate no seconds, we can use truncate
          c('dmY HMS', 'Ymd HMS'), truncated = 1,
          tz = timezone
        )
      }

      # 1.3 Check if the fix worked. If yes, message and return data
      # with the new timestamp
      if (qc_is_timestamp(res, verbose = FALSE, parent_logger = parent_logger)) {
        message('timestamp succesfully fixed. A sample: ', res[1])
        return(res)
      } else {
        stop('Unable to format correctly the timestamp, please ',
              'revise manually.')
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_as_timestamp', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_as_timestamp', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_as_timestamp', sep = '.'))})

}


################################################################################
#' Checking for NAs in the timestamp
#'
#' Simple function to check for NAs in the timestamp.
#'
#' NAs in timestamp generates problems in the further steps of QC, a function
#' checking for NAs and info about the location is needed in order to be
#' able to fix it
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the timestamp variable
#'
#' @return A data frame with the NAs info
#'
#' @import dplyr
#'
#' @export

# START
# Function declaration
qc_timestamp_nas <- function(data, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(data)) {
      stop('Data provided is not a data frame')
    }
    if (is.null(data$timestamp)) {
      stop('Data provided has not a timestamp variable')
    }

    # STEP 1
    # Retrieving info about NAs in timestamp
    if (!any(is.na(data$timestamp))) {

      # 1.1 If no NAs, return TRUE
      return(invisible(TRUE))
      message("Nice!! no NA's found in the timestamp.")
    } else {

      # 1.2 If NAs, return the NAs
      res_df <- data %>%
        dplyr::mutate(row_number = row.names(data)) %>%
        dplyr::filter(is.na(timestamp))

      # STEP 2
      # Return the res_df
      return(res_df)
    }
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_timestamp_nas', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_timestamp_nas', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_timestamp_nas', sep = '.'))})
}




################################################################################
#' Checking for NAs in the psi
#'
#' Simple function to check for NAs in psi
#'
#' checking for NAs and info about the location is needed in order to be
#' able to fix it
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the psi variable
#'
#' @return A data frame with the NAs info
#'
#' @import dplyr
#'
#' @export

# START
# Function declaration
qc_psi_nas <- function(data, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(data)) {
      stop('Data provided is not a data frame')
    }
    if (is.null(data$psi)) {
      stop('Data provided has not psi variable')
    }

    # STEP 1
    # Retrieving info about NAs in psi
    if (!any(is.na(data$psi))) {

      # 1.1 If no NAs, return TRUE
      return(invisible(TRUE))
      message("Nice!! no NA's found in psi.")
    } else {

      # 1.2 If NAs, return the NAs
      res_df <- data %>%
        dplyr::mutate(row_number = row.names(data)) %>%
        dplyr::filter(is.na(psi))

      # STEP 2
      # Return the res_df
      return(res_df)
    }
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_psi_nas', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_psi_nas', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_psi_nas', sep = '.'))})
}




################################################################################
#' Checking for NAs in the psi SE
#'
#' Simple function to check for NAs in psi SE
#'
#' checking for NAs and info about the location is needed in order to be
#' able to fix it
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the psi SE variable
#'
#' @return A data frame with the NAs info
#'
#' @import dplyr
#'
#' @export

# START
# Function declaration
qc_psi_SE_nas <- function(data, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(data)) {
      stop('Data provided is not a data frame')
    }
    if (is.null(data$psi_SE)) {
      stop('Data provided has not psi SE variable')
    }

    # STEP 1
    # Retrieving info about NAs in psi
    if (!any(is.na(data$psi_SE))) {

      # 1.1 If no NAs, return TRUE
      return(invisible(TRUE))
      message("Nice!! no NA's found in psi SE.")
    } else {

      # 1.2 If NAs, return the NAs
      res_df <- data %>%
        dplyr::mutate(row_number = row.names(data)) %>%
        dplyr::filter(is.na(psi_SE))

      # STEP 2
      # Return the res_df
      return(res_df)
    }
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_psi_SE_nas', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_psi_SE_nas', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_psi_SE_nas', sep = '.'))})
}



################################################################################
#' Checking for NAs in the psi N
#'
#' Simple function to check for NAs in psi N
#'
#' checking for NAs and info about the location is needed in order to be
#' able to fix it
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the psi N variable
#'
#' @return A data frame with the NAs info
#'
#' @import dplyr
#'
#' @export

# START
# Function declaration
qc_psi_N_nas <- function(data, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(data)) {
      stop('Data provided is not a data frame')
    }
    if (is.null(data$psi_N)) {
      stop('Data provided has not psi N variable')
    }

    # STEP 1
    # Retrieving info about NAs in psi
    if (!any(is.na(data$psi_N))) {

      # 1.1 If no NAs, return TRUE
      return(invisible(TRUE))
      message("Nice!! no NA's found in psi N.")
    } else {

      # 1.2 If NAs, return the NAs
      res_df <- data %>%
        dplyr::mutate(row_number = row.names(data)) %>%
        dplyr::filter(is.na(psi_N))

      # STEP 2
      # Return the res_df
      return(res_df)
    }
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_psi_N_nas', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_psi_N_nas', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_psi_N_nas', sep = '.'))})
}

################################################################################
#' Function to simplify questionnaire section
#'
#' Psi questions simplification
#'
#' This function simplifies the questions metadata that were requested to
#' the contributors
#'
#' @family Quality Checks Functions
#'
#' @param question_md Data frame of the questionnaire metadata object
#'
#' @return Data frame with the questionnaire metadata with simple questions
#'
#' @export

qc_simplify_questions <- function(question_md, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Set new and simplified questions
    simple_questions <- c(
      "CloseFluxnetTower",
      "CloseEddyCovarianceTower",
      "ContributorToGlobalDatabase",
      "Dendrometers"
    )

    # STEP 1
    # Change questions

    names(question_md) <- simple_questions

    return(question_md)

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_rem_to_units',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_rem_to_units',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_rem_to_units',
                                                        sep = '.'))})
}

