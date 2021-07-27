#' Return metadata about newpapers issues
#'
#' @param issues dataframe of issues to summarize information for
#' @param newspapers optional dataframe of newspaper metadata
#' @return tibble of metadata for each issue of that newspaper that has issues\
#' within the start and end date
#' @export
subset_info = function(issues, newspapers=chronam::newspapers) {

    # 2. aggregate on lccn
    counts = data.frame(table(issues$lccn))
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
		  aggregate(num_issues ~ state, data=results, sum),
		  "issues_per_state_unnested" = 
		  aggregate(num_issues ~ state, data=results_unnested, sum))
    )
}
