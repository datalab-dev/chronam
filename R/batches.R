#' Get full list of batch metadata from Chronicling America Website
#'
#' Takes a couple of minutes. 
#' @return tibble of all the batches
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
get_batches= function() {
    start_url = "https://chroniclingamerica.loc.gov/batches.json"
    result = parse_batches_part_json(start_url)
    nextcursor = result$nextcursor
    batches = result$batches

    while (!is.null(nextcursor)) {
	result = parse_batches_part_json(nextcursor)
	nextcursor = result$nextcursor
	batches = rbind(batches, result$batches)
    }
    return(batches)
}

#' Get metadata from a single json page of batch results
#'
#' @param url character url of the part to get data from
#' @return list with nextcursor url (can be null) and batches dataframe
parse_batches_part_json = function(url) {
    parsed_json = jsonlite::fromJSON(url)
    nextcursor = parsed_json["next"][[1]]
    batch_json = parsed_json["batches"][[1]]
    batches_df = tibble::tibble(
	    name = batch_json$name,
	    url = batch_json$url,
	    page_count = batch_json$page_count,
	    awardee_name = batch_json$awardee$name,
	    awardee_url = batch_json$awardee$url,
	    lccns = batch_json$lccns,
	    ingested = batch_json$ingested
	    )
    return(list("nextcursor" = nextcursor, "batches" = batches_df))
}
