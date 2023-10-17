


#' Convert a given rhythm pattern, a BPM and a BPM type to duration values
#'
#' @param pattern
#' @param bpm
#' @param type
#'
#' @return
#' @export
#'
#' @examples
pattern_to_ms <- function(pattern, bpm = 120, type) {

  if(is.scalar.character(pattern)) {
    pattern <- itembankr::str_mel_to_vector(pattern)
  }

  ms_unit <- bpm_to_ms(bpm, type)

  pattern * ms_unit
}



bpm_to_ms <- function(bpm, type = 4) {

  stopifnot(type %in% c(4, 8, 16))

  beat_length_in_ms <- 60/bpm

  if(type == 8) {
    beat_length_in_ms <- beat_length_in_ms/2
  }

  if(type == 16) {
    beat_length_in_ms <- beat_length_in_ms/4
  }

  beat_length_in_ms

}



is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}

type_from_page <- function(page_type) {
  page_type %>%
    stringr::str_remove("record_") %>%
    stringr::str_remove("_page")
}


check_names_same <- function(names_1, names_2) {
  length(setdiff(names_1, names_2)) == 0
}


is.scalar.na <- function(x) {
  all(is.na(x)) & length(x) == 1
}

is.scalar.null <- function(x) {
  all(is.null(x)) & length(x) == 0
}

is.scalar.na.or.null <- function(x) {
  is.scalar.na(x) | is.scalar.null(x)
}

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}
