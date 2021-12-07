
#' Shell function for processing palmsplus data
#'
#' @param palms_data The palms dataset that is output from HABITUS. Path to csv file.
#'
#' @param config_file The config file specifying the palmsplus processing parameters. Path to csv file.
#'
#' @param spatial_threshold Spatial threshold for multimodal trips in meters. Default is \code{200}.
#' @param temporal_threshold Temporal threshold for multimodal trips in minutes. Default is \code{10}.
#'
#' @param verbose Print progress after each step. Default is \code{TRUE}.
#'
#' @param days Build the \code{days} dataset? Default is \code{TRUE}.
#' @param trajectories Build the \code{trajectories} dataset? Default is \code{TRUE}.
#' @param multimodal Build the \code{multimodal} dataset? Default is \code{TRUE}.
#'
#' @param return_list Return all datasets as a list? Default is \code{FALSE}.
#'
#' @return
#' @export
#'
#' @examples
palmsplus_shell <- function(palms_data,
                            config_file,
                            spatial_threshold = 200,
                            temporal_threshold = 10,
                            verbose = TRUE,

                            days = TRUE,
                            trajectories = TRUE,
                            multimodal = TRUE,
                            return_list = FALSE) {

  # Read palms data from csv (if the data is saved as csv from HABITUS)
  palms <- read_palms(file = palms_data, verbose = verbose)

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



  if (return_list) {

    result <- list(palmsplus_output = pp)

    if (days) result <- append(result, list(days_output = d))

    if (trajectories) result <- append(result, list(trajectories_output = tr))

    if (multimodal & trajectories) result <- append(result, list(multimodal_output = mm))

    return(result)
  }




  # Save results to output folder: csv files (excluding geometry) and GIS shapefiles

  # write_csv(st_set_geometry(pp, NULL), "output/palmsplus.csv")
  # write_csv(days, "output/days.csv")
  # write_csv(st_set_geometry(tr, NULL), "output/trajectories.csv")
  # write_csv(st_set_geometry(mm, NULL), "output/multimodal.csv")
  #
  # st_write(pp, "output/palmsplus.shp")
  # st_write(tr, "output/trajecories.shp")
  # st_write(mm, "output/multimodal.shp")

}













