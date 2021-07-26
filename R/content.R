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

    # alternativley get list of lccns => search on batchesdf => list of batches
}

#' Download the batch ocr data from chronicling America
#' 
#' bulk data url: http://chroniclingamerica.loc.gov/data/batches/<batch_name>
#' bulk ocr data url: http://chroniclingamerica.loc.gov/data/ocr/<batch_name>.tar.bz2
#' 
#' batch info url: http://chroniclingamerica.loc.gov/batches/<batch_name>.json
#' bulk bib data url: http://chroniclingamerica.loc.gov/data/bib/<??>
#' bulk word_coordinates data url: http://chroniclingamerica.loc.gov/data/word_coordinates/<lccn>
#'
#' the ocr data url seems be missing batches ingested in the current month
#' @param batch_name character name of batch e.g wvu_els_ver01
#' @param odir optional character path of directory to download the files into
#' @importFrom httr GET write_disk
#' @importFrom jsonlite fromJSON
download_batch_ocr_tar = function(batch_name, odir=".") {
    fname = paste0(batch_name, ".tar.bz2")
    url = paste0("https://chroniclingamerica.loc.gov/data/ocr/", fname)

    httr::GET(url, httr::write_disk(paste0(odir, "/", fname), overwrite=TRUE))
}

#' Load the text for a locally downloaded batch
#'
#' @param path_to_tar character file path to tar file for a batch
#' @param exdir character file path extract contents to
#' @return dataframe of text_block, page, issue, date, year, month, day, lccn
#' @export
parse_batch_ocr_tar = function(path_to_tar, exdir=".") {
    #TODO:
    # eventually need to be able to handle already extracted tar files...
    # Going to need to merge with the issues_df metadata at some point

    # get file list
    files = untar(path_to_tar, list=TRUE)
    xml_files = files[grep("\\.xml$", files)]
    txt_files = files[grep("\\.txt$", files)]

    # extract xml
    untar(path_to_tar, xml_files, exdir=exdir)

    # parse xml
    issues_content_list = lapply(xml_files[1:5], parse_ocr_xml)

    # combine
    issues_content = do.call("rbind", issues_content_list)
}

#' Aggregate all the strings in a text block 
#' @param xmlnode for a TextBlock from ocr.xml file
#' @return string containing all the words in the text block
#' @importFrom xml2 xml_find_all xml_attr
get_block_content = function(block) {
    block_text = ""
    strings = xml2::xml_find_all(block, ".//String")
    contents = xml2::xml_attr(attr="CONTENT", strings)
    contents = trimws(contents)
    content = paste(contents, collapse=" ")
}

#' Parse the xml from an ocr.xml file 
#'
#' @param xml character filepath or xml content of an ocr.xml file
#' @return tibble where each row is a text block
#' @export
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all
#' @importFrom tibble tibble
parse_ocr_xml = function(xml) {

    doc = xml2::read_xml(xml)
    doc = xml2::xml_ns_strip(doc)
    blocks = xml2::xml_find_all(doc, "//TextBlock")

    block_content = as.character(lapply(blocks, get_block_content))
    fields = strsplit(file, "/")[[1]]
    issue = tibble::tibble (
			    lccn = fields[1],
			    year = fields[2],
			    month = fields[3],
			    day = fields[4],
			    id = paste(lccn, year, month, day, sep="_"),
			    ed = fields[5],
			    page = fields[6],
			    num_blocks = length(block_content),
			    content = block_content)
}


#' Get text for a single issue through the website (not bulk download)
#'
#' @param lccn
#' @param date
#' @return string (maybe text blocks if possible)
# TODO:
