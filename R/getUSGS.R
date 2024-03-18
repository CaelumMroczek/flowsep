#' Get USGS Streamgage Discharge Data
#'
#' @param gage_no character - USGS streamgage number, usually an 8-digit number.
#' @param start_date character - starting date for data retrieval in the form YYYY-MM-DD. Defaults to earliest possible record.
#' @param end_date character - ending date for data retrieval in the form YYYY-MM-DD. Defaults to latest possible record.
#'
#' @return a dataframe with dates and mean daily discharge data (in cfs)
#' @export
#'
#' @examples
#' gage_no <- "09315000"
#' start_date <- "2021-09-30"
#' end_date <- "2022-10-01"
#' getUSGS(gage_no, start_date, end_date)

getUSGS <- function(gage_no, start_date = "", end_date = ""){
  gage_data <- dataRetrieval::readNWISdv(siteNumbers = gage_no,
                            startDate = start_date,
                            endDate = end_date,
                            statCd = "00003",
                            parameterCd = "00060")

  gage_data <- dataRetrieval::renameNWISColumns(gage_data)
  gage_data <- gage_data[,3:4]

}
