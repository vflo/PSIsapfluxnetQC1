################################################################################
#' Verify provided species names (spelling and correctness)
#'
#' \code{qc_species_names} uses \code{Taxonstand} package.
#'
#' This function takes a vector of species names and check if they are right
#' spelled. Also, if a synonym is used, the function changes it automatically in
#' order to have the same name for the same species
#'
#' @param data Data frame as the obtained from \code{\link{qc_species_names_info}}
#'
#' @return A character vector with species fixed in spelling and correctness.
#'
#' @import Taxonstand
#'
#' @export

# START
# Function declaration

qc_species_names_fix <- function(data, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is data a data frame
    if (!is.data.frame(data)) {
      stop('data provided is not a data frame')
    }
    # data has the needed variables
    if (is.null(data$data_names) | is.null(data$tpl_names) | is.null(data$Concordance) | is.null(data$IsNA)) {
      stop('data do not have needed variables: ',
           'data_names, tpl_names, Concordance and isNA')
    }

    # STEP 1
    # If TPL generated NAs, return the original species with a warning
    if (any(data$IsNA)) {
      warning('NAs have been generated, please try again with a lower value of max_distance')
      return(data$data_names)

      # STEP 2
      # If not, return the tpl names
    } else {
      return(data$tpl_names)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_species_names_fix', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_species_names_fix', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_species_names_fix', sep = '.'))})

}

################################################################################
#' Info of species names spelling
#'
#' Summary of species names spelling info
#'
#' @family Quality Checks Functions
#'
#' @param species Character vector containing the species names to verify
#'
#' @param max_distance A number indicating the maximum distance allowed
#' for a match in agrep
#'
#' @return A data frame summarizing the species names declared, the species
#'   names obtained after tpl and the concordance and NAs info
#'
#' @import WorldFlora
#'
#' @export

# START
# Function declaration
qc_species_names_info <- function(species, max_distance = 1,
                                  parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 0
    # Argument checking
    # Is species a character vector?
    if (!is.vector(species, 'character')) {
      stop('species object is not a character vector, please verify data object')
    }
    # Warning if max_distance is under 0.75
    if (max_distance > 5) {
      message('Maximum distance value for spelling algorithm is over 5',
              ' and this can be cause of species name changes.',
              ' Maybe manual fix of some species should be done')
    }

    message("Checking species names")

    # STEP 1
    # Trimming blank spaces in both sides
    species <- stringr::str_trim(species, 'both')

    # STEP 2
    # Obtaining tpl info
    # tpl_df <- Taxonstand::TPL(species, max.distance = max_distance)
    # species_tpl <- tpl_df$Taxon
    # Obtaining WFO info
    save.dir = getwd()
    if(!file.exists("WFO_Backbone.zip")){
      message('Downloading World Flora Online database for the first time.',
              ' It will only be downloaded once.')
      save.file <- normalizePath(file.path(paste0(save.dir, "/WFO_Backbone.zip")))
      download.file(paste0("https://files.worldfloraonline.org/files/WFO_Backbone/",
                           "_WFOCompleteBackbone/WFO_Backbone.zip"),
                    destfile = save.file,
                    method = "wget", extra = "--no-check-certificate")
      utils::unzip(save.file, exdir = save.dir)
    }
    WFO.file1 <- paste0(save.dir, "/classification.txt",
                        sep = "")
    if (file.exists(WFO.file1) == FALSE) {
      WFO.file1 <- paste0(save.dir, "/classification.csv")
    }
    tpl_df <- WorldFlora::WFO.match(spec.data = species, WFO.file = WFO.file1)
    species_tpl <- tpl_df[which(tpl_df$New.accepted == TRUE),'spec.name']
    rm(WFO.data)
    gc()

    # 2.1 Checking for concordance taking into account that species_tpl maybe
    #     is NA
    concordance <- species == species_tpl
    concordance[is.na(concordance)] <- FALSE


    # STEP 3
    # Create the results data frame
    res <- data.frame(
      data_names = species,
      tpl_names = species_tpl,
      IsNA = is.na(species_tpl),
      Concordance = concordance,
      stringsAsFactors = FALSE
    )

    # STEP 4
    # Return the results
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_species_names_info', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_species_names_info', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_species_names_info', sep = '.'))})


}

################################################################################
#' Wrapper for species names check
#'
#' Wrapper for \code{\link{qc_species_names_info}} and \code{\link{qc_species_names_fix}}
#'
#' @family Quality Checks Functions
#'
#' @param species Character vector containing the species names to verify
#'
#' @param max_distance NA number indicating the maximum distance allowed for
#'   a match in agrep when performing corrections of spelling errors in
#'   specific epithets. Not used if corr = FALSE.
#'
#' @return a vector with the fixed names of the species if fix is possible or
#'   needed, or a vector with the original names of the species if the fix is not
#'   possible or they are correct
#'
#' @export

# START
# Function declaration
qc_species_names <- function(species, max_distance = 1,
                             parent_logger= 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 0
    # Argument checking
    # No needed as the checks are already made in the internal functions

    # STEP 1
    # Get names and tpl info
    info <- qc_species_names_info(species, max_distance, parent_logger)

    # STEP 2
    # Return the names
    res <- qc_species_names_fix(info, parent_logger)
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_species_names', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_species_names', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_species_names', sep = '.'))})


}

################################################################################
#' Check if sp_name and sp_ntrees coincides with plants specified in plant_md
#'
#' \code{qc_species_verification} checks for coincidence in the number and species
#' names between species_md and plant_md
#'
#' In order to check if provided species metadata coincides with provided plant
#' metadata, species names and number of plants in each species are checked
#' against plant metadata.
#'
#' @section Coincidence:
#'   Possible values of \code{coincidence} column in results data frame are:
#'   \code{TRUE}, indicating both metadata have the same species and the number
#'   of trees are the same; \code{FALSE} indicates that both metadata have the
#'   same species, but the number of trees are no the same; finally,
#'   \code{NA} indicates that the species are not the same in both metadata.
#'
#' @family Quality Checks Functions
#'
#' @param species_md Data frame containing species metadata.
#'
#' @param plant_md Data frame containing plant metadata.
#'
#' @return A data frame with the species provided in plant and species metadata,
#'   as well as the number of trees of each species in both metadata.
#'   \code{coincidence} column indicates result of checking both numbers and
#'   presence of the same species.
#'
#' @export

# START
# Function declaration

qc_species_verification <- function(species_md, plant_md,
                                    parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(species_md) | !is.data.frame(plant_md)) {
      message('One or both metadata objects is/are not data frame/s')
    }

    # STEP 1
    # Extract number and species names information from species_md
    sp_md <- species_md %>%
      dplyr::select(sp_name, sp_ntrees) %>%
      dplyr::rename(sp_names = sp_name, sp_n_trees = sp_ntrees) %>%
      dplyr::arrange(sp_names)

    # STEP 2
    # Extract number and species names information from plant_md
    pl_md <- plant_md %>%
      dplyr::select(pl_species) %>%
      dplyr::group_by(pl_species) %>%
      dplyr::summarize(pl_n_trees = n()) %>%
      dplyr::rename(sp_names = pl_species) %>%
      dplyr::arrange(sp_names)

    # STEP 3
    # Compare both metadata to look for errors and generate result object
    res <- dplyr::full_join(sp_md, pl_md, by = 'sp_names') %>%
      dplyr::mutate(Concordance = (sp_n_trees == pl_n_trees))

    # STEP 4
    # Return the results object
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_species_verification', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_species_verification', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_species_verification', sep = '.'))})

}
