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
    lat <- site_md$lat
    long <- site_md$lon
    timestamp <- data$timestamp

    # STEP 2
    # Intermediate objects
    # 2.2 Mean Solar Time
    mst <- solaR::local2Solar(timestamp, long)

    # 2.2.1 warning if solartimestamp has repeated values (due to rounding)
    if (length(mst) != length(unique(mst))) {
      warning('solar mean time generates repeated timestamps. ',
              'Please revise the original timestamp for repeated values.')
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

    data$ext_rad <- solI

    if (add_solar_ts) {
      data$solartimestamp <- ast
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


################################################################################
#' Presence of variables needed for transformations summary
#'
#' Summary table for variables needed for unit and any other kind of conversions
#'
#' This function generates a table (data frame) with information about the
#' presence of the variables needed for unit, radiation, solar time and others
#' data transformations/conversions.
#'
#' @family Unit conversion
#'
#' @param psidata psiData object for the site containing all the information
#'
#' @return A data frame with the following columns:
#'
#'   \itemize{
#'     \item{Variable: Variable name}
#'     \item{Location: Variable location (i.e. env_data or env_md)}
#'     \item{Trasformation: Tranformation/Conversion for whihc the variable is needed}
#'     \item{Presence: Logical indicating if the variable is present}
#'   }
#'
#' @export

# START
# Function declaration
qc_transformation_vars <- function(psidata, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Checking arguments
    # Is psidata a psiData object?
    if (!is(psidata, "psiData")) {
      stop('Data provided is not a psiData object')
    }

    # STEP 1
    # Extraterrestrial radiation
    exr_vars <- c('timestamp', 'lat', 'lon')
    exr_loc <- c('psi_data', 'site_md', 'site_md')
    exr_transf <- rep('solar_time', length(exr_vars))
    exr_presence <- c(
      !all(is.na(get_env(psidata)$timestamp)),
      !all(is.na(get_site_md(psidata)$lat)),
      !all(is.na(get_site_md(psidata)$lon))
    )

    # STEP n
    # combining vector for each transformation in a data frame
    vars <- c(exr_vars)
    loc <- c(exr_loc)
    transf <- c(exr_transff)
    presence <- c(exr_presence)

    # n.1 data frame
    res <- data.frame(
      Variable = vars,
      Location = loc,
      Transformation = transf,
      Presence = presence,
      stringsAsFactors = FALSE
    )

    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_transformation_vars',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_transformation_vars',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_transformation_vars',
                                                        sep = '.'))})
}

################################################################################
#' Transformations list
#'
#' Show all the transformations indicating which ones can be done with the data
#' provided.
#'
#' The data frame returned by this function is intended to use it in the next
#' level, allowing automatized transformations depending on the available
#' variables.
#'
#' @family Unit conversion
#'
#' @param transf_info Data frame with info about the variables needed for
#'   transformations as generated by \code{\link{qc_transformation_vars}}
#'
#' @return A data frame with the following columns:
#'
#'   \itemize{
#'     \item{Transformation: Transformation/Conversion name}
#'     \item{Available: Logical indicating if the transformation is possible}
#'   }
#'
#' @export

# START
# Function declaration
qc_transf_list <- function(transf_info, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(transf_info)) {
      stop('trans_info provided is not a data.frame')
    }

    # STEP 1
    # Radiation conversion
    rad_transf <- 'radiation_conversion'
    rad_info <- transf_info %>%
      dplyr::filter(Transformation == 'radiation_conversion')
    if (all(rad_info$Presence) | all(!rad_info$Presence)) {
      rad_avail <- FALSE
    } else {
      rad_avail <- TRUE
    }

    # STEP 2
    # Extraterrestrial radiation
    exr_trasnf <- 'solar_time'
    exr_info <- transf_info %>%
      dplyr::filter(Transformation == 'solar_time')
    if (all(exr_info$Presence)) {
      exr_avail <- TRUE
    } else {exr_avail <- FALSE}

    # STEP 3
    # psi unit conversions
    sfu_info <- transf_info %>%
      dplyr::filter(Transformation == 'psi_units')

    # 3.0 get the units for plant and the units for sapwood
    sapwood_level_units <- c(
      '“cm3 cm-2 h-1”',
      '“cm3 m-2 s-1”',
      '“dm3 dm-2 h-1”',
      '“dm3 dm-2 s-1”',
      '“mm3 mm-2 s-1”',
      '“g m-2 s-1”',
      '“kg m-2 h-1”',
      '“kg m-2 s-1”'
    )

    plant_level_units <- c(
      '“cm3 s-1”',
      '“cm3 h-1”',
      '“dm3 h-1”',
      '“g h-1”',
      '“kg h-1”'
    )

    # 3.1 plant level
    # 3.1.1 if origin units are plant level,
    #       automatically the conversion is available
    if (sfu_info[4, 'Location'] %in% plant_level_units) {
      sfu_plant_avail <- TRUE
    } else {
      sfu_plant_avail <- all(sfu_info$Presence[1:2])
    }
    sfu_plant_transf <- 'psi_units_to_plant'

    # sfu_info_plant <- sfu_info %>%
    #   dplyr::filter(Variable != 'pl_leaf_area')
    # sfu_plant_trasnf <- 'psi_units_to_plant'
    # sfu_plant_avail <- all(sfu_info_plant$Presence)

    # 3.2 sapwood level (is the same that for plant)
    # 3.2.1 if origin units are sapwood level,
    #       automatically the conversion is available
    if (sfu_info[4, 'Location'] %in% sapwood_level_units) {
      sfu_sapw_avail <- TRUE
    } else {
      sfu_sapw_avail <- all(sfu_info$Presence[1:2])
    }
    sfu_sapw_transf <- 'psi_units_to_sapwood'

    # sfu_sapw_trasnf <- 'psi_units_to_sapwood'
    # sfu_sapw_avail <- all(sfu_info_plant$Presence)

    # 3.3 leaf area level
    # 3.3.1 depending on the origin units level we need one or another
    if (sfu_info[4, 'Presence'] %in% plant_level_units) {
      sfu_leaf_avail <- all(sfu_info$Presence[c(1,3)])
    } else {
      sfu_leaf_avail <- all(sfu_info$Presence[c(1:3)])
    }
    sfu_leaf_transf <- 'psi_units_to_leaf_area'

    # sfu_info_leaf <- sfu_info
    # sfu_leaf_transf <- 'psi_units_to_leaf_area'
    # sfu_leaf_avail <- all(sfu_info_leaf$Presence)

    # STEP 4
    # VPD calculation
    vpd_info <- transf_info %>%
      dplyr::filter(Transformation == 'vpd_and_rh_calc')
    vpd_transf <- 'VPD_calculation'

    if (vpd_info[3, 'Presence']) {
      vpd_avail <- FALSE
    } else {
      if (!vpd_info[1, 'Presence'] | !vpd_info[2, 'Presence']) {
        vpd_avail <- FALSE
      } else {
        vpd_avail <- TRUE
      }
    }

    # STEP 5
    # rh calculation
    rh_info <- vpd_info
    rh_transf <- 'rh_calculation'

    if (rh_info[1, 'Presence']) {
      rh_avail <- FALSE
    } else {
      if (!rh_info[2, 'Presence'] | !rh_info[3, 'Presence']) {
        rh_avail <- FALSE
      } else {
        rh_avail <- TRUE
      }
    }

    # STEP n
    # build res data frame and return it
    transf <- c(rad_transf, exr_trasnf, vpd_transf, rh_transf, sfu_plant_transf,
                sfu_sapw_transf, sfu_leaf_transf)
    avail <- c(rad_avail, exr_avail, vpd_avail, rh_avail, sfu_plant_avail,
               sfu_sapw_avail, sfu_leaf_avail)

    res <- data.frame(
      Transformation = transf,
      Available = avail,
      stringsAsFactors = FALSE
    )

    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_transf_list',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_transf_list',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_transf_list',
                                                        sep = '.'))})
}

################################################################################
#' Units process
#'
#' This function checks for available transformations and perform them if needed
#'
#' List of available transformations are obtained from \code{\link{qc_transf_list}}
#'
#' @family Unit conversion
#'
#' @param psidata psiData object to perform the conversions
#'
#' @return An psiData object with the newly calculated variables included
#'
#' @export

# START FUNCTION
# Function declaration
qc_units_process <- function(psidata, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({
    # STEP 0
    # Argument checks
    if (!is(psidata, "psiData")) {
      stop('Data provided is not a psiData object')
    }

    # progress message
    message(
      'Unit conversion for ', get_si_code(psidata)[1]
    )

    # STEP 1
    # Get the transformation list
    transf_list <- qc_transf_list(
      qc_transformation_vars(psidata, parent_logger = parent_logger),
      parent_logger = parent_logger
    )

    rownames(transf_list) <- transf_list[['Transformation']]

    # STEP 2
    # Radiation conversion
    if (transf_list['radiation_conversion', 'Available']) {

      # progress message
      message(
        'Radiation units'
      )

      env_data <- get_env(psidata)
      env_modf <- qc_rad_conversion(env_data, parent_logger = parent_logger)
      env_flags <- get_env_flags(psidata)
      vars_names <- names(env_modf)[!(names(env_modf) %in% names(env_flags))]
      vars_to_create <- as.list(rep('CALCULATED', length(vars_names)))
      names(vars_to_create) <- vars_names
      env_flags_modf <- env_flags %>%
        dplyr::mutate(!!! vars_to_create) %>%
        dplyr::select(names(env_modf))

      get_env(psidata) <- env_modf[,-1]
      get_env_flags(psidata) <- env_flags_modf[,-1]
    }

    # STEP 3
    # VPD
    if (transf_list['VPD_calculation', 'Available']) {

      # progress message
      message(
        'VPD'
      )

      env_data <- get_env(psidata)
      env_modf <- qc_vpd(env_data, parent_logger = parent_logger)
      env_flags <- get_env_flags(psidata)
      vars_names <- names(env_modf)[!(names(env_modf) %in% names(env_flags))]
      vars_to_create <- as.list(rep('CALCULATED', length(vars_names)))
      names(vars_to_create) <- vars_names
      env_flags_modf <- env_flags %>%
        dplyr::mutate(!!! vars_to_create) %>%
        dplyr::select(names(env_modf))

      # 3.1 modify the env_data from the psidata
      get_env(psidata) <- env_modf[,-1]
      get_env_flags(psidata) <- env_flags_modf[,-1]
    }

    # STEP 4
    # rh
    if (transf_list['rh_calculation', 'Available']) {

      # progress message
      message(
        'Relative humidity units'
      )

      env_data <- get_env(psidata)
      env_modf <- qc_rh(env_data, parent_logger = parent_logger)
      env_flags <- get_env_flags(psidata)
      vars_names <- names(env_modf)[!(names(env_modf) %in% names(env_flags))]
      vars_to_create <- as.list(rep('CALCULATED', length(vars_names)))
      names(vars_to_create) <- vars_names
      env_flags_modf <- env_flags %>%
        dplyr::mutate(!!! vars_to_create) %>%
        dplyr::select(names(env_modf))

      # 4.1 modify the env_data from the psidata
      get_env(psidata) <- env_modf[,-1]
      get_env_flags(psidata) <- env_flags_modf[,-1]
    }

    # STEP 5
    # Solar Time
    if (transf_list['solar_time', 'Available']) {

      # progress message
      message(
        'Extraterrestrial radiation and solartimestamp'
      )

      env_data <- get_env(psidata)
      site_md <- get_site_md(psidata)
      env_modf <- qc_ext_radiation(
        env_data, site_md, add_solar_ts = TRUE,
        parent_logger = parent_logger
      )

      env_flags <- get_env_flags(psidata)
      env_flags_modf <- env_flags %>%
        dplyr::mutate(ext_rad = 'CALCULATED')

      # 5.1 add the solar timestamp to the psiData
      get_solar_timestamp(psidata) <- env_modf[['solartimestamp']]

      # 5.2 modify the env_data from the psidata
      get_env(psidata) <- env_modf %>%
        dplyr::select(-timestamp, -solartimestamp) %>%
        as.data.frame(stringsAsFactors = FALSE)

      get_env_flags(psidata) <- env_flags_modf %>%
        dplyr::select(-timestamp)
    }

    # STEP 6
    # psi_units
    # 6.1 get the sapwood metadata
    sapw_md <- get_plant_md(psidata) %>%
      qc_get_sapw_md(parent_logger = parent_logger) %>%
      qc_sapw_area_calculator(parent_logger = parent_logger)

    # 6.2 to plant
    if (transf_list['psi_units_to_plant', 'Available']) {

      # progress message
      message(
        'psi plant level'
      )

      # 6.2.1 get the psi_modif
      psi_modf <- get_psi(psidata) %>%
        qc_sapw_conversion(
          sapw_md, output_units = 'plant', parent_logger = parent_logger
        )

      # 6.2.2 get the plant_md
      plant_md <- get_plant_md(psidata)

      # 6.2.3 modify the psi data and the plant md to add the units
      psidata_plant <- psidata

      get_psi(psidata_plant) <- psi_modf %>%
        dplyr::select(-timestamp) %>%
        as.data.frame(stringsAsFactors = FALSE)

      get_plant_md(psidata_plant) <- plant_md %>%
        dplyr::mutate(pl_sap_units_orig = pl_sap_units,
                      pl_sap_units = "“cm3 h-1”")

      # 6.2.4 write the plant psiData object
      df_write_psiData(psidata_plant, 'unit_trans', 'plant',
                       parent_logger = parent_logger)
    }

    # 6.3 to sapwood
    if (transf_list['psi_units_to_sapwood', 'Available']) {

      # progress message
      message(
        'psi sapwood level'
      )

      # 6.3.1 get the psi_modif
      psi_modf <- get_psi(psidata) %>%
        qc_sapw_conversion(
          sapw_md, output_units = 'sapwood', parent_logger = parent_logger
        )

      # 6.3.2 get the plant md
      plant_md <- get_plant_md(psidata)

      # 6.3.3 modify the psi data from the psidata
      psidata_sapwood <- psidata

      get_psi(psidata_sapwood) <- psi_modf %>%
        dplyr::select(-timestamp) %>%
        as.data.frame(stringsAsFactors = FALSE)

      get_plant_md(psidata_sapwood) <- plant_md %>%
        dplyr::mutate(pl_sap_units_orig = pl_sap_units,
                      pl_sap_units = "“cm3 cm-2 h-1”")

      # 6.3.4 write the plant psiData object
      df_write_psiData(psidata_sapwood, 'unit_trans', 'sapwood',
                       parent_logger = parent_logger)
    }

    # 6.4 to leaf
    if (transf_list['psi_units_to_leaf', 'Available']) {

      # progress message
      message(
        'psi leaf level'
      )

      # 6.4.1 get the psi_modif
      psi_modf <- get_psi(psidata) %>%
        qc_sapw_conversion(
          sapw_md, output_units = 'leaf', parent_logger = parent_logger
        )

      # 6.4.2 get the plant md
      plant_md <- get_plant_md(psidata)

      # 6.4.2 modify the psi data from the psidata
      psidata_leaf <- psidata

      get_psi(psidata_leaf) <- psi_modf %>%
        dplyr::select(-timestamp) %>%
        as.data.frame(stringsAsFactors = FALSE)

      get_plant_md(psidata_leaf) <- plant_md %>%
        dplyr::mutate(pl_sap_units_orig = pl_sap_units,
                      pl_sap_units = "“cm3 cm-2 h-1”")

      # 6.4.3 write the plant psiData object
      df_write_psiData(psidata_leaf, 'unit_trans', 'leaf',
                       parent_logger = parent_logger)
    }
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_units_process',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_units_process',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_units_process',
                                                        sep = '.'))})

}
