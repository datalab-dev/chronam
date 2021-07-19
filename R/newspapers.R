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
    # combine the newspapers's state assignments into a character vector
    newspapers = aggregate(data=newspapers, state ~ lccn + url + title, FUN=c)
    newspapers$states = newspapers$state


    # in keeping with the logic from issues
    # that the last state is the most important one
    # for example https://chroniclingamerica.loc.gov/lccn/sn83045160/
    # notice that tennessee is the fourth state for that lccn in the newspapers
    # dataframe and the last is the issues place column
    newspapers$state = sapply(newspapers$states, tail, 1)

    # also 
    tibble::as_tibble(newspapers)
}
