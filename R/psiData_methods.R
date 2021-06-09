#' psiData get methods
#'
#' Methods to get the info from the psiData class slots
#'
#' \code{get_psi} method retrieve psi data and timestamp to create a functional
#' dataset to work with.
#'
#' \code{get_psi_flags} method retrieve sapflow or environmental flags also
#' with the timestamp.
#'
#' \code{get_timestamp} method retrieve only the timestamp as POSIXct vector.
#'
#' \code{get_si_code} method retrieve a character vector with length(timestamp)
#' containing the site code.
#'
#' \code{get_site_md}, and \code{get_plant_md} methods retrieve the corresponding
#' metadata.
#'
#' @param object Object of class psiData from which data is retrieved
#'
#' @param solar Logical indicating if the timestamp to return in the get_psi
#'   and get_psi_flags methods
#'
#' @name psi_get_methods
#' @include psiData_class.R psiData_generics.R
NULL

#' @rdname psi_get_methods
#' @export
setMethod(
  "get_psi", "psiData",
  function(object, solar = FALSE) {
    # data
    .psi <- slot(object, "psi_data")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .psi)

    # return
    return(res)
  }
)


#' @rdname psi_get_methods
#' @export
setMethod(
  "get_psi_flags", "psiData",
  function(object, solar = FALSE) {
    .psi_flags <- slot(object, "psi_flags")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .psi_flags)

    # return
    return(res)
  }
)

#' @rdname psi_get_methods
#' @export
setMethod(
  "get_timestamp", "psiData",
  function(object) {
    slot(object, "timestamp")
  }
)

#' @rdname psi_get_methods
#' @export
setMethod(
  "get_solar_timestamp", "psiData",
  function(object) {
    slot(object, "solar_timestamp")
  }
)

#' @rdname psi_get_methods
#' @export
setMethod(
  "get_si_code", "psiData",
  function(object) {
    slot(object, "si_code")
  }
)

#' @rdname psi_get_methods
#' @export
setMethod(
  "get_site_md", "psiData",
  function(object) {
    slot(object, "site_md")
  }
)

#' @rdname psi_get_methods
#' @export
setMethod(
  "get_question_md", "psiData",
  function(object) {
    slot(object, "questionnaire_md")
  }
)


#' @rdname psi_get_methods
#' @export
setMethod(
  "get_plant_md", "psiData",
  function(object) {
    slot(object, "plant_md")
  }
)


#' Show method for psiData
#'
#' @param object psiData object to show
#' @export
setMethod(
  "show", "psiData",
  definition = function(object) {
    # object class
    cat(class(object), " object\n", sep = "")
    # site code
    cat("Data from ", unique(get_si_code(object)), " site/s\n\n", sep = "")
    # number of trees
    cat("Sapflow data: ", nrow(slot(object, "psi_data")), " observations of ",
        length(names(slot(object, "psi_data"))), " trees/plants\n\n")
    # env_vars
    cat("Environmental data: ", nrow(slot(object, "env_data")), " observations.\n",
        "Env vars: ", paste(names(slot(object, "env_data"))), "\n\n")
    # timestamp span
    cat("TIMESTAMP span, from ", as.character(head(get_timestamp(object), 1)),
        "to ", as.character(tail(get_timestamp(object), 1)), "\n\n")

    # solar_timestamp
    cat("Solar TIMESTAMP available: ", !is.null(get_solar_timestamp(object)),
        "\n\n")

    # psi_flags
    psi_flags <- unique(unlist(stringr::str_split(unlist(lapply(slot(object, "psi_flags"), unique)), '; ')))
    psi_flags_table <- vapply(psi_flags, function(flag){sum(stringr::str_count(as.matrix(slot(object, "psi_flags")), flag))}, numeric(1))
    psi_flags_table <- psi_flags_table[names(psi_flags_table) != '']
    cat("Sapflow data flags:\n")
    if (length(psi_flags_table)) {
      print(sort(psi_flags_table))
    } else {cat("No flags present")}
    cat("\n")

    # env_flags
    env_flags <- unique(unlist(stringr::str_split(unlist(lapply(slot(object, "env_flags"), unique)), '; ')))
    env_flags_table <- vapply(env_flags, function(flag){sum(stringr::str_count(as.matrix(slot(object, "env_flags")), flag))}, numeric(1))
    env_flags_table <- env_flags_table[names(env_flags_table) != '']
    cat("Environmental data flags:\n")
    if (length(env_flags_table)) {
      print(sort(env_flags_table))
    } else {cat("No flags present")}
    cat("\n")

  }
)

#' Sub-setting operation
#'
#' @param i data row index
#' @param j psi data column index
#' @param object psiData object
#'
#' @export
setMethod(
  "[", signature(x = "psiData", i = "numeric", j = "ANY", drop = "missing"),
  function(x, i, j) {

    # subsetting the slots for subset
    .psi <- slot(x, "psi_data")[i, j]

    # if no flags, create an empty data.frame
    if (nrow(get_psi_flags(x)) < 1) {
      .psi_flags <- data.frame()
    } else {
      .psi_flags <- slot(x, "psi_flags")[i, j]
    }


    TIMESTAMP <- slot(x, "timestamp")[i]
    .solar_timestamp <- slot(x, "solar_timestamp")[i]
    .si_code <- slot(x, "si_code")[i]

    # create the psiData object, the metadata slots remain without modifications
    # as well as si_code
    psiData(
      psi_data = .psi,
      psi_flags = .psi_flags,
      timestamp = TIMESTAMP,
      solar_timestamp = .solar_timestamp,
      si_code = .si_code,
      site_md = slot(x, "site_md"),
      plant_md = slot(x, "plant_md"),
      question_data = slot(x, "question_md")
    )
  }
)

#' plot psiData method
#'
#' @param object psiData object
#' @param type what to plot
#' @param solar use solarTIMESTAMP?
#'
#' @export
setMethod(
  'plot', c('psiData', 'missing'),
  function(x,
           type = c('psi','psiSE','psiN'),
           solar = FALSE) {
    # get the type with match argument
    type <- match.arg(type)

    # psi
    if (type == 'psi') {
      data <- get_psi(x, solar)

      # actual plot
      res_plot <- data %>%
        tidyr::gather(pl_code, psi, -timestamp) %>%
        ggplot(aes(x = timestamp, y = psi, colour = pl_code)) +
        geom_point(alpha = 0.4) +
        labs(y = expression(Psi*"[MPa]")) +
        scale_x_datetime() +
        facet_wrap('pl_code', ncol = 3, scale = 'fixed')
    }

    # psiSE
    if (type == 'psiSE') {
      data <- get_psi(x, solar)

      # actual plot
      res_plot <- data %>%
        tidyr::gather(pl_code, psiSE, -timestamp) %>%
        ggplot(aes(x = timestamp, y = psiSE, colour = pl_code)) +
        geom_point(alpha = 0.4) +
        labs(y = expression(Psi*"[MPa]")) +
        scale_x_datetime() +
        facet_wrap('pl_code', ncol = 3, scale = 'fixed')
    }

    # psiN
    if (type == 'psiN') {
      data <- get_psi(x, solar)

      # actual plot
      res_plot <- data %>%
        tidyr::gather(pl_code, psiN, -timestamp) %>%
        ggplot(aes(x = timestamp, y = psiN, colour = pl_code)) +
        geom_point(alpha = 0.4) +
        labs(y = expression(Psi*"[MPa]")) +
        scale_x_datetime() +
        facet_wrap('pl_code', ncol = 3, scale = 'fixed')
    }


    return(res_plot)
  }
)

#' Replacement methods
#'
#' Methods for replacing the slots with new data or metadata
#'
#' The replacement object must be a valid object for that slot, i.e. for psilow
#' data slot a data frame with the same dimensions and without TIMESTAMP variable
#' is needed. A validity check is done before returning the replaced psiData
#' object and an error is returned if this check fails.
#'
#' @return The same psiData object with the corresponding slot changed to the
#'   value provided. An error if the value provided generates an invalid
#'   psiData object.
#'
#' @name psi_replacement
NULL

#' @export
#' @rdname psi_replacement
setReplaceMethod(
  "get_psi", "psiData",
  function(object, value) {
    slot(object, "psi_data") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)


#' @export
#' @rdname psi_replacement
setReplaceMethod(
  "get_psi_flags", "psiData",
  function(object, value) {
    slot(object, "psi_flags") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)


#' @export
#' @rdname psi_replacement
setReplaceMethod(
  "get_timestamp", "psiData",
  function(object, value) {
    slot(object, "timestamp") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname psi_replacement
setReplaceMethod(
  "get_solar_timestamp", "psiData",
  function(object, value) {
    slot(object, "solar_timestamp") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname psi_replacement
setReplaceMethod(
  "get_si_code", "psiData",
  function(object, value) {
    slot(object, "si_code") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname psi_replacement
setReplaceMethod(
  "get_site_md", "psiData",
  function(object, value) {
    slot(object, "site_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)


#' @export
#' @rdname psi_replacement
setReplaceMethod(
  "get_plant_md", "psiData",
  function(object, value) {
    slot(object, "plant_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)


#' Validity method for psiData class
#'
#' @name psi_validity
setValidity(
  "psiData",
  function(object) {
    # initial values
    info <- NULL
    valid <- TRUE


    # check dimensions
    if (any(
      nrow(slot(object, "psi_data")) != length(slot(object, "timestamp")),
      nrow(slot(object, "psi_data")) != length(slot(object, "si_code")),
      length(slot(object, "timestamp")) != length(slot(object, "si_code")),
      length(slot(object, "timestamp")) != length(slot(object, "solar_timestamp")),
      nrow(slot(object, "psi_flags")) != nrow(slot(object, "psi_data")),,
      nrow(slot(object, "psi_flags")) != length(slot(object, "timestamp")),
      nrow(slot(object, "psi_flags")) != length(slot(object, "si_code"))
    )) {
      valid <- FALSE
      info <- c(info, 'dimensions are incorrect, they must fulfill "nrow(psi_data) == length(timestamp) == length(si_code)"')
    }

    # check if si_code is empty
    if (any(slot(object, "si_code") == '')) {
      valid <- FALSE
      info <- c(info, 'si_code slot can not be an empty string')
    }

    # check for metadata presence
    if (any(nrow(slot(object, "site_md")) < 1, nrow(slot(object, "stand_md")) < 1,
            nrow(slot(object, "plant_md")) < 1)) {
      valid <- FALSE
      info <- c(info, 'metadata slots can not be empty data frames')
    }

    # check for timestamp presence
    if (length(slot(object, "timestamp")) < 1) {
      valid <- FALSE
      info <- c(info, 'TIMESTAMP must be of length >= 1')
    }

    # check for si_code presence
    if (length(slot(object, "si_code")) < 1) {
      valid <- FALSE
      info <- c(info, 'si_code must be of length >= 1')
    }

    # check for questionnaire presence
    if (nrow(slot(object, "question_md")) < 1) {
      valid <- FALSE
      info <- c(info, 'questionnaire must be of length >= 1')
    }

    # insert more checks here



    # return validity or info
    if (valid) {
      return(TRUE)
    } else { return(info) }
  }
)
