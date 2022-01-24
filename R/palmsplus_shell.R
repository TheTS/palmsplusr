
#' Shell function for processing palmsplus data
#'
#' @param palms_data The palms dataset that is output from HABITUS. Path to csv file.
#' @param habitus Is this data from HABITUS (as opposed to PALMS, which has different column names). Default is \code{FALSE}.
#'
#' @param config_file The config file specifying the palmsplus processing parameters. Path to csv file.
#'
#' @param spatial_threshold Spatial threshold for multimodal trips in meters. Default is \code{200}.
#' @param temporal_threshold Temporal threshold for multimodal trips in minutes. Default is \code{10}.
#'
#' @param verbose Print progress after each step. Default is \code{TRUE}.
#'
#' @param save_output Logical. Save output files to disk. Default is \code{TRUE}.
#' @param output_path Path to directory where output files are saved. Default is current working directory
#' @param return_list Return all datasets as a list? Default is \code{FALSE}.
#'
#' @param days Build the \code{days} dataset? Default is \code{TRUE}.
#' @param trajectories Build the \code{trajectories} dataset? Default is \code{TRUE}.
#' @param multimodal Build the \code{multimodal} dataset? Default is \code{TRUE}.
#'
#'
#' @return
#' @export
#'
#' @examples
#'\dontrun{
#' palmsplus_shell(palms_data = "habitus_cleaned.csv", # Path to palms/habitus dataset
#'                 habitus = TRUE,                     # Are data from habitus (not palms)
#'
#'                 config_file = "config.csv",         # Path to config file
#'
#'                 days = TRUE,                        # Build the 'days' dataset?
#'                 trajectories = TRUE,                # Build the 'trajectories' dataset?
#'                 multimodal = TRUE,                  # Build the 'multimodal' dataset?
#'
#'                 spatial_threshold = 100,            # Spatial threshold for multimodal trips (meters)
#'                 temporal_threshold = 5,             # Temporal threshold for multimodal trips (minutes)
#'
#'                 save_output = TRUE,                 # Save output files?
#'                 output_path = "output",             # Directory where results are saved
#'                 return_list = TRUE)                 # Return output files as a list
#'}

palmsplus_shell <- function(palms_data,
                            habitus = FALSE,
                            config_file,
                            spatial_threshold = 200,
                            temporal_threshold = 10,
                            verbose = TRUE,

                            save_output = TRUE,
                            output_path = getwd(),
                            return_list = FALSE,

                            days = TRUE,
                            trajectories = TRUE,
                            multimodal = TRUE) {

  # Read palms data from csv (if the data is saved as csv from HABITUS)
  palms <- read_palms(file = palms_data, verbose = verbose, habitus = habitus)

  # Build palmsplus dataset. This dataset is used to create the days and trajectories datasets
  pp <- palms_build_palmsplus(data = palms, config_file = config_file, verbose = verbose)


  # Build days dataset if requested
  if (days) {

    d <- palms_build_days(data = pp, config_file = config_file, verbose = verbose)

  }


  # Build trajectories dataset if requested
  if (trajectories) {

    tr <- palms_build_trajectories(data = pp, config_file = config_file)


    # Build multimodal dataset if requested
    if (multimodal) {

      mm <- palms_build_multimodal(data = tr,
                                   spatial_threshold = spatial_threshold,
                                   temporal_threshold = temporal_threshold,
                                   verbose = verbose,
                                   config_file = config_file)

    }

  }



  # Save results to output folder: csv files (excluding geometry) and GIS shapefiles
  if (save_output) {

    if (!dir.exists(output_path)) {
      if (verbose)
        message("Creating output directory\n")

      dir.create(output_path)
    }

    write_csv(st_drop_geometry(pp), file.path(output_path, 'palmsplus.csv'))
    st_write(pp, delete_layer = TRUE, file.path(output_path, 'palmsplus.shp'))

    if (days) {
      write_csv(d, file.path(output_path, 'days.csv'))
    }

    if (trajectories) {
      write_csv(st_drop_geometry(tr), file.path(output_path, 'trajectories.csv'))
      st_write(tr, delete_layer = TRUE, file.path(output_path, 'trajectories.shp'))

      if (multimodal) {
        write_csv(st_drop_geometry(mm), file.path(output_path, 'multimodal.csv'))
        st_write(mm, delete_layer = TRUE, file.path(output_path, 'multimodal.shp'))
      }

    }

  }


  # Return relevant data as a list if requested
  if (return_list) {

    result <- list(palmsplus_output = pp)

    if (days) result <- append(result, list(days_output = d))

    if (trajectories) result <- append(result, list(trajectories_output = tr))

    if (multimodal & trajectories) result <- append(result, list(multimodal_output = mm))

    return(result)
  }


}













