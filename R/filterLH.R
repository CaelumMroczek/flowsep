#' Title
#'
#' @param streamflow vector - streamflow values
#' @param alpha numeric - filter parameter (defaults to 0.925 following Nathan and McMahon (1990))
#' @param passes numeric - number of times the filter passes over the data (typically 1-3)
#'
#' @return a dataframe with 2 columns and nrow = length of streamflow dataset. Column one contains baseflow in the same units as input, column two contains quickflow in the same units as input.
#' @export
#'
#' @examples
#' streamflow <- GreenRiver[,2]
#' filterLH(streamflow)

# Function to compute baseflow and quickflow from streamflow data using a digital filtering method.
filterLH <- function(streamflow, alpha = 0.925, passes = 3) {

  # Suppressing warnings for cleaner output
  suppressWarnings({

    # Defining start and end values for the filter function
    Ends <- c(1, length(streamflow)) * rep(1, (passes + 1))

    # Defining adjustments for start values based on number of passes
    AddToStart <- c(1, -1) * rep(1, passes)

    # Initializing vectors to store baseflow and quickflow
    baseflow_previous <- streamflow  # Baseflow approximation from the previous pass
    quickflow <- vector(length = length(streamflow))  # Vector for quickflow

    # Initial baseflow value for the first time step
    baseflow <- ifelse(streamflow[1] < quantile(streamflow, 0.25), streamflow[1], mean(streamflow) / 1.5)

    # Looping over the specified number of passes
    for (pass in 1:passes) {
      # Looping over the range of values for the current pass
      for (i in (Ends[pass] + AddToStart[pass]):Ends[pass + 1]) {
        # Applying the digital filter to compute baseflow
        if ((alpha * baseflow[i - AddToStart[pass]] + ((1 - alpha) / 2) * (baseflow_previous[i] + baseflow_previous[i - AddToStart[pass]])) > baseflow_previous[i]) {
          baseflow[i] <- baseflow_previous[i]
        } else baseflow[i] <- alpha * baseflow[i - AddToStart[pass]] + ((1 - alpha) / 2) * (baseflow_previous[i] + baseflow_previous[i - AddToStart[pass]])

        # Computing quickflow values
        quickflow[i] <- streamflow[i] - baseflow[i]
      }

      # Updating the baseflow approximation for end values if it's not the final pass
      if (pass < passes) {
        baseflow_previous <- baseflow
        baseflow[Ends[pass + 1]] <- ifelse(streamflow[Ends[pass + 1]] < mean(baseflow_previous), streamflow[Ends[pass + 1]] / 1.2, mean(baseflow_previous))
      }
    }

    # Creating a data frame with baseflow and quickflow
    result <- data.frame(baseflow, quickflow)

    # Returning the data frame
    return(result)
  })
}