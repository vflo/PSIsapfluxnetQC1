################################################################################
# VISUALIZATION FUNCTIONS                                                      #
#                                                                              #
# Functions to visualize the data                                              #
################################################################################


################################################################################
#' ggplot2 theme for SAPFLUXNET plots
#'
#' Custom ggplot2 theme for uniformization of plot visuals
#'
#' @export

theme_sfn <- function(base_size = 10, base_family = "Lato") {
  half_line <- base_size/2
  theme(line = element_line(colour = "black", size = 1,
                            linetype = 1, lineend = "butt"),
        rect = element_rect(fill = NA, colour = "black",
                            size = 1, linetype = 1),
        text = element_text(family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5,
                            vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE),
        axis.line = element_blank(),
        # axis.line.x = element_line(),
        # axis.line.y = element_line(),
        axis.text = element_text(size = rel(0.8)),
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line*2.5),
                                   vjust = 1),
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line*2),
                                   hjust = 1),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length = unit(-half_line, "pt"),
        axis.title.x = element_text(margin = margin(t = 0.8 * half_line,
                                                    b = 0.8 * half_line/2)),
        axis.title.y = element_text(angle = 90,
                                    margin = margin(r = 0.8 * half_line,
                                                    l = 0.8 * half_line/2)),
        legend.background = element_rect(colour  = NA, fill = ),
        legend.spacing = unit(1, "pt"),
        legend.key = element_rect(colour = NA),
        legend.key.size = unit(1, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.8)),
        legend.text.align = NULL,
        legend.title = element_text(hjust = 0.5),
        legend.title.align = 0,
        legend.position = "right",
        legend.direction = NULL,
        legend.justification = "top",
        legend.box = NULL,
        panel.background = element_blank(),
        panel.border = element_rect(),
        panel.grid = element_blank(),
        # panel.grid.major = element_line(colour = "black", size = rel(0.3),
        #                                 linetype = 2),
        # panel.grid.minor = element_blank(),
        # panel.grid.major.x = element_blank(),
        panel.spacing = unit(half_line, "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = TRUE,
        strip.background = element_rect(size = rel(0.3)),
        strip.text = element_text(colour = "grey10", size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line,
                                                    b = half_line)),
        strip.text.y = element_text(angle = -90,
                                    margin = margin(l = half_line, r = half_line)),
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm"),
        plot.background = element_blank(),
        plot.title = element_text(size = rel(1.2),
                                  margin = margin(b = half_line * 1.2)),
        plot.margin = margin(half_line, half_line, half_line, half_line),

        complete = TRUE)
}

################################################################################
#' Plotting a diagram of biomes
#'
#' This function produces a ggplot object showing the biomes as colored areas
#' according to mean annual temperature (MAT) and mean annual precipitation (MAP)
#' using a SpatialPolygonsDataFrame object obtained with
#' \code{\link{qc_get_biomes_spdf}}
#'
#' @family Visualization Functions
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#' in a single biome. By default, deserts are not merged.
#'
#' @return a ggplot object showing the biomes.
#'
#' @export

# START
# Function declaration
vis_biome <- function(merge_deserts = FALSE, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is merge_deserts logical?
    if (!(is.logical(merge_deserts))) {
      stop('merge_deserts must be logical')
    }
    # Is merge_deserts NA?
    if (is.na(merge_deserts)) {
      stop('merge_deserts must be either TRUE or FALSE')
    }

    # STEP 1
    # Get biomes SpatialPointsDataFrame object
    suppressMessages(
      biomes_df <- fortify(qc_get_biomes_spdf(merge_deserts = merge_deserts))
    )

    # STEP 2
    # Make and return the plot object
    # 2.1 Make color palette
    if (merge_deserts){

      pal <- viridis::viridis(9)[c(2,9,3,4,6,7,8,1)]

    } else {

      pal <- viridis::viridis(9)[c(2,3,5,4,9,6,7,8,1)]

    }

    # 2.2 Make the plot object
    plot <- ggplot() +
      ggiraph::geom_polygon_interactive(data = biomes_df,
                                        aes(tooltip = id, data_id = id,
                                            x = long, y = lat, group = id,
                                            fill = id)) +
      scale_fill_manual('Biomes', values = pal) +
      xlab('Mean annual precipitation (mm)') +
      ylab('Mean annual temperature (ºC)')

    # 2.3 Return the plot object
    return(plot)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'vis_biome',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'vis_biome',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'vis_biome',
                                                        sep = '.'))})

}

################################################################################
#' Plotting a diagram of biomes with sites as dots
#'
#' This function produces a ggplot object showing the biomes as colored areas
#' according to mean annual temperature (MAT) and mean annual precipitation (MAP),
#' using the function \code{\link{vis_biome}}, and adds the sites on it according
#' to their values of MAT and MAP.
#'
#' @family Visualization Functions
#'
#' @param data Data frame of site metadata, including mean annual temperature
#' (si_mat) and mean annual precipitation (si_map) columns, or at least
#' latitude (si_lat) and longitude (si_long) columns that will be used to obtain
#' climatic data with \code{\link{qc_get_biome}}.
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#' in a single biome. By default, deserts are not merged.
#'
#' @return a ggplot object showing the biomes.
#'
#' @export

# START
# Function declaration
vis_location_biome <- function(data, merge_deserts = FALSE,
                               parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is data a data.frame?
    if (!is.data.frame(data)) {
      stop('Provided data object is not a data.frame.',
           ' Please verify if it is the correct object')
    }
    # Does data contains a longitude variable?
    if (is.null(data$si_long)) {
      stop('There is no longitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    # Does data contains a latitude variable?
    if (is.null(data$si_lat)) {
      stop('There is no latitude variable in this dataset. ',
           'Please verify if it is the correct data')
    }
    # Is merge_deserts logical?
    if (!(is.logical(merge_deserts))) {
      stop('merge_deserts must be logical')
    }
    # Is merge_deserts NA?
    if (is.na(merge_deserts)) {
      stop('merge_deserts must be either TRUE or FALSE')
    }

    # STEP 1
    # Get MAT and MAP if not provided
    if (!all(c('si_mat', 'si_map') %in% names(data))){
      data <- qc_get_biome(data, merge_deserts = merge_deserts)
    }

    # STEP 2
    # Make the plot
    # 2.1 Get biome plot
    plot <- vis_biome(merge_deserts = merge_deserts)

    # 2.2 Make the plot object
    plot <- plot +
      ggiraph::geom_point_interactive(data = data, aes(
        x = si_map, y = si_mat,
        tooltip = si_code, data_id = si_code
      ),
      color = 'black', shape = 21, fill = 'white', size = 2, stroke = 0.5) +
      theme_bw() +
      coord_cartesian(xlim = c (0, 4500), ylim = c(-16, 30), expand = FALSE)

    # 2.3 Return the plot object
    return(plot)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'vis_location_biome',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'vis_location_biome',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'vis_location_biome',
                                                        sep = '.'))})

}

################################################################################
#' Environmental responses plot
#'
#' Plot the desired environmental funcion \emph{vs.} spaflow values
#'
#' @family Visualization Functions
#'
#' @param SfnData SfnData object
#'
#' @param env_var Character indicating the nameof the environmental variable to
#'   plot
#'
#' @param solar Use solarTIMESTAMP?
#'
#' @return a \code{ggplot} object with the desired plot
#'
#' @export

vis_environmental_responses <- function(
  SfnData,
  env_var = 'vpd',
  solar = FALSE,
  parent_logger = 'test'
) {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Checking arguments
    if (!is(SfnData, 'SfnData')) {
      stop('vis_environmental_responses needs an SfnData object as argument')
    }

    # STEP 1
    # Get the data
    env_data <- get_env(SfnData, solar)

    # 1.1 check for timestamp (if solar = TRUE and no solarTimestamp can be a
    #     memory problem)
    if (all(is.na(env_data[['TIMESTAMP']]))) {
      stop('TIMESTAMP is all NA, can not produce the plot')
    }

    # 1.2 plot data
    plot_data <- env_data %>%
      dplyr::select(TIMESTAMP, !!env_var) %>%
      dplyr::full_join(get_sapf(SfnData, solar), .) %>%
      tidyr::gather(Tree, Value, -TIMESTAMP, -(!!env_var))

    units_char <- get_plant_md(SfnData)[['pl_sap_units']][1]

    # STEP 2
    # Build the plot
    env_res_plot <- plot_data %>%
      ggplot(aes_(x = as.name(env_var), y = ~Value, colour = ~Tree)) +
      geom_point(alpha = 0.2) +
      labs(y = paste0('Sapflow [', units_char, ']')) +
      facet_wrap('Tree', ncol = 3)

    # STEP 3
    # Return the plot
    return(env_res_plot)

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'vis_environmental_responses',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'vis_environmental_responses',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'vis_environmental_responses',
                                                        sep = '.'))})
}
