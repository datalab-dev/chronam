# Functions for downloading the ocr text of newspapers 
#
# Start with a list of issues of interest
# For each of those issues get the batch that they are contained in
# Download the batches
# Parse the batches (raw ocr, ocr.xml -> text blocks)

#' Get Batch for each issue
#'
#' It would have been faster to lookup based on lccn and chronam::batches
#' Have to do it this way because each lccn can appear in multiple batches
#' Since this is very slow, only run this when downloading the data for
#' a set of the issues that is much smaller than the 2.6mill+ in the full 
#' collection. I.e only use this when you have a sample
#' 
#' @param issue_url character url for issue metadata
#' @return character of batch name
get_batch_from_issue = function(issue_url) {
    batch = jsonlite::fromJSON(issue_url)$batch$name
}

download_batch = function(){
    TODO:
}

parse_batch = function() {
    TODO:
}
