
# given a specific newspaper (LCCN)
# return dataframe of all the issues for that newspaper
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

    # place and subject can have multiple values -> collapse to one
    parsed_json$place = paste0(parsed_json$place, collapse=",")
    parsed_json$subject = paste0(parsed_json$subject, collapse=",")

    # handle NULL before creating dataframe
    parsed_json = lapply(parsed_json, function(x) if (is.null(x)) NA else x)

    # create dataframe from list
    tibble::as_tibble(parsed_json)
}

get_newspapers = function() {
    newspapers = jsonlite::fromJSON(
	    "https://chroniclingamerica.loc.gov/newspapers.json")$newspapers
}

check_fail = function(x) {
    "error.message" %in% names(x)
}

# this takes about 30 minutes to run
# by default should just load the package data
get_issues_all = function(sample=-1, ncores=detectCores()) {

    lccns = get_newspapers()$lccn

    if (sample >= 1) {
	lccns = sample(lccns, sample)
    }

    issues_dfs = mclapply(lccns, get_issues, mc.cores=ncores)
    failed_list = Filter(check_fail, issues_dfs) # 19 should be there
    success_list = Filter(Negate(check_fail), issues_dfs)

    issues_all = do.call("rbind", success_list)
}
