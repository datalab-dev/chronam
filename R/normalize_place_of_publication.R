# returns state from place of publication
normalize_place_of_publication = function(places) {
}

library(stringr)

state = i$place_of_publication

# split on comma and get the last element
state = stringr::str_split(state, ",")
state = sapply(state, tail, 1)
state = stringr::str_split(state, "\\[")
state = sapply(state, tail, 1)

# remove punctuation
state = stringr::str_replace_all(state, "[[:punct:]]", " ")
state = trimws(state)

# TH is territory of Hawaii

