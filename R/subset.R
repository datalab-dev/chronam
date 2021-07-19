#' Return metadata about newpapers issues from within range
#'
#' @param start number for inclusive start date
#' @param end number for inclusive end date 
#' @param issues optional dataframe of metadata
#' @return tibble of metadata for each issue of that newspaper that has issues\
#' within the start and end date
#' @export
issues_in_range = function(start, end, 
	    issues=chronam::issues, newspapers=chronam::newspapers) {


    # 1. subset on range
    sub = issues[issues$year >= start & issues$year <= end, ] 

    if (nrow(sub) == 0) {
	return()
    }
    # 2. aggregate on lccn
    counts = data.frame(table(sub$lccn))
    colnames(counts) = c("lccn", "num_issues")

    # 3. join on newspapers.lccn
    results = tibble::as_tibble(merge(x = counts, y = newspapers, by="lccn"))

    # unnest the states
    results_unnested = tidyr::unnest(results, states)

    return ( list(
		  "newspapers" = results,
		  "newspaper_count" = length(results$lccn),
		  "issues_count" = sum(results$num_issues),
		  "newspapers_per_state" = table(results_unnested$state),
		  "newspapers_per_state_unnested" = table(results_unnested$state),
		  "issues_per_state" = 
		  aggregate(num_issues ~ state, data=results_unnested, sum),
		  "issues_per_state_unnested" = 
		  aggregate(num_issues ~ state, data=results_unnested, sum))
    )
}
