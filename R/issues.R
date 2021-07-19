#' Get the issues associated with a newspaper
#'
#' @param lccn character the loc control number for the newspaper
#' @return tibble of metadata for each issue of that newspaper
#' @importFrom jsonlite fromJSON
#' @export
get_issues = function(lccn) {
    construct_lccn_url = function(lccn="") {
	url = paste0("https://chroniclingamerica.loc.gov/lccn/", 
		     lccn, ".json", collapse="")
    }
    url = construct_lccn_url(lccn)

    # in case the url can't be parsed by fromJSON for some reason
    # e.g url doesn't exist
    tryCatch (
	      {
		  parsed_json = jsonlite::fromJSON(url)
	      },
	      error = function(cond) {
		  return(
			 data.frame(
				   "lccn" = lccn,
				   "url" = url,
				   "error-message" = cond$message)
			 )
	      }
    )

    # handle NULL before creating dataframe
    parsed_json = lapply(parsed_json, function(x) if (is.null(x)) NA else x)

    # create dataframe from list
    tibble::tibble(
		      place_of_publication = parsed_json$place_of_publication,
		      lccn = parsed_json$lccn,
		      start_year = parsed_json$start_year,
		      name = parsed_json$name,
		      publisher = parsed_json$publisher,
		      end_year = parsed_json$end_year,
		      url = parsed_json$issues$url,
		      date= parsed_json$issues$date_issued,
		      subject = list(parsed_json$subject), # can be multiple
		      place = list(parsed_json$place), # can be multiple
		      )
}

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
}

#' Determine if problem occured during downloading of an issue
#'
#' @param x dataframe return of chronam::get_issues
#' @return bool
check_fail = function(x) {
    "error.message" %in% names(x)
}


#' Get the state assignment for a each issue
#'
#' @param place character of locations associated with the newspapers 
#' @return character the state name for the last place in the list
get_state = function(place) {
    # takes whole column (called after issues have been compiled)
    # as postprocessing
    place = tail(place, 1)
    state = strsplit(place, "--")[[1]][1]
}


#' Get a dataframe of all the issues and their metadata
#'
#' this takes about 30 minutes to run on one core
#' so by default we just load the package data
#' if you suspect package data is out of date then
#' can be run with update=TRUE param
#' 
#' @param sample optional int refering to sample size, by default gets all
#' @param ncores optional int number of cores to use for pinging chronam site
#' @param update optional bool if FALSE just use the package data
#' @return dataframe of all the issues and their metadata
#' @export
get_issues_all = function(sample=-1, ncores=parallel::detectCores(), 
			  update=FALSE) {

    if (!update) {
	return(chronam::issues)
    }

    lccns = get_newspapers()$lccn

    if (sample >= 1) {
	lccns = sample(lccns, sample)
    }

    issues_dfs = parallel::mclapply(lccns, get_issues, mc.cores=ncores)
    failed_list = Filter(check_fail, issues_dfs) # 19 should be there
    success_list = Filter(Negate(check_fail), issues_dfs)

    issues_all = do.call("rbind", success_list)
    issues_all$state = sapply(issues_all$place, get_state)
    issues_all
}

#parse_place_of_publication_string(x) {
# convert place of pub into a state or territory name
# TODO:
#}
