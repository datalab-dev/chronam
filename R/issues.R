
# given a specific newspaper (LCCN)
# return dataframe of all the issues for that newspaper
get_issues = function(lccn) {
    construct_lccn_url = function(lccn="") {
	url = paste0("https://chroniclingamerica.loc.gov/lccn/", 
		     lccn, ".json", collapse="")
    }
    url = construct_lccn_url(lccn)

    tryCatch (
	      {
		  out = jsonlite::fromJSON(url)
		  out$place = paste0(out$place, collapse=",")
		  out$subject = paste0(out$subject, collapse=",")

		  out = as.data.frame(out)
	      },
	      error = function(cond) {
		  out = data.frame(
				   "lccn" = lccn,
				   "url" = url,
				   "error-message" = cond$message)
	      }
    )
}

get_newspapers = function() {
    newspapers = jsonlite::fromJSON(
	    "https://chroniclingamerica.loc.gov/newspapers.json")$newspapers
}

check_fail = function(x) {
    "error.message" %in% names(x)
}

get_issues_all = function(sample=-1) {

    lccns = get_newspapers()$lccn

    if (sample >= 1) {
	lccns = sample(lccns, sample)
    }

    issues_dfs = lapply(lccns, get_issues)
    failed_list = Filter(check_fail, issues_dfs) # 19 should be there
    success_list = Filter(Negate(check_fail), issues_dfs)

    issues_all = do.call("rbind", success_list)
}
