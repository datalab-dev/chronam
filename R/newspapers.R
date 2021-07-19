#' Get full list of newspapers metadata from Chronicling America Website
#'
#' 
#' @return tibble of all the newspapers
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
get_newspapers = function() {
    newspapers = jsonlite::fromJSON(
	    "https://chroniclingamerica.loc.gov/newspapers.json")$newspapers
    tibble::as_tibble(newspapers)

    # postprocess the newspapers
    # lccn is not unique in the newpspapers metadata
    # sometimes it has additional state values
    newspapers = aggregate(data=newspapers, state ~ lccn + url + title, FUN=c)
    tibble::as_tibble(newspapers)
}

