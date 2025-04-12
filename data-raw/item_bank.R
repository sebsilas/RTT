

load_all()

library(tidyverse)

# Advice from DM:

# Yes, you could use the corpus of rhythms we extracted from the Geerdes corpus to use with the RAT. These are rendered using two different drum sounds (low and high).
# But you could also reproduce these just with a single sound which might be easier for the young kids to do.
# In that case, one of the two sounds would be replaced by silences, I guess.


load('data-raw/RAT_item_bank.rda')

RTT_item_bank <- RAT_item_bank

rm(RAT_item_bank)



remove_trailing_0 <- function(v) {

  v <- strsplit(v, "")[[1]] %>%
    as.integer()

  first_1 <- min(which(v == 1))

  v <- v[first_1:length(v)]

  paste0(v, collapse = "")
}


get_no_zeroes <- function(no) {
  if(no == 1) {
    res <- 0
  } else {

  options(scipen = 999)
  res <- no %>%
    as.character() %>%
    strsplit(split = "") %>%
    pluck(1) %>%
    tibble(no = .) %>%
    count(no) %>%
    filter(no == 0) %>%
    pull(n)
  }
  res
}


split_durations <- function(pattern) {

  # FYI: 100000000000 is the biggest, but generalised solution below


  if(grepl("0", pattern)) {
    chunks <- paste0("1", strsplit(pattern, "1")[[1]])

    v <- purrr::map_int(chunks, get_no_zeroes) %>%
      magrittr::add(1)
    v <- v[2:length(v)]

    if(substr(pattern, nchar(pattern), nchar(pattern)) == "1") {
      v <- c(v, 1)
    }


  } else {
    # Should only contain ones
    v <- rep(1, nchar(pattern))
  }

  paste0(v, collapse = ", ")

}



RTT_item_bank_narrow <- RTT_item_bank %>%
  select(pattern, type, difficulty) %>%
  rowwise() %>%
  mutate(pattern = remove_trailing_0(pattern),
         pattern_split = split_durations(pattern) ) %>%
  ungroup()


# In the RAT paradigm, there can be "flips" because there are 2 different sounds. We don't have flips, so some of the items are redundant:
RTT_item_bank_narrow_count <- RTT_item_bank_narrow %>%
  count(pattern, type)

# However, it would be useful to have some aggregate idea of the difficulty, hence:

RTT_item_bank_narrow <- RTT_item_bank_narrow %>%
  group_by(pattern) %>%
  summarise(pattern = pattern,
            pattern_split = pattern_split,
            type = type,
            RAT_difficulty = mean(difficulty, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

rm(RTT_item_bank_narrow_count)


RTT_item_bank <- RTT_item_bank_narrow

rm(RTT_item_bank_narrow)



RTT_item_bank <- RTT_item_bank %>%
  rowwise() %>%
  mutate(
    durations_bpm_120 = paste0( pattern_to_ms( itembankr::str_mel_to_vector(pattern_split) , bpm = 120, type = type), collapse = ", "),
    stimulus_length = length(itembankr::str_mel_to_vector(durations_bpm_120))
    ) %>%
  ungroup() %>%
  mutate(item_id = paste0("RTT_", dplyr::row_number())) %>%
  filter(stimulus_length > 2)



# Make the last dur always 0.5 for scoring purposes (we don't know how long the last hit is)
RTT_item_bank <- RTT_item_bank %>%
  rowwise() %>%
  mutate( durations_bpm_120_2 = paste0(c(itembankr::str_mel_to_vector(durations_bpm_120)[1:(stimulus_length-1)], 0.5), collapse = ",") ) %>%
  ungroup()




use_data(RTT_item_bank, overwrite = TRUE)


library(itembankr)

RTT_item_bank_for_itembankr <- RTT_item_bank %>%
  rowwise() %>%
  mutate(abs_melody = paste0(rep(60, stimulus_length), collapse = ","),
         melody = paste0(rep(0, stimulus_length), collapse = ",") )  %>%
  ungroup() %>%
  rename(durations = durations_bpm_120) %>%
  select(abs_melody, melody, durations)



itembankr::create_item_bank(name = "RTT",
                            input = "phrase_df",
                            output = "phrase",
                            input_df = RTT_item_bank_for_itembankr,
                            distinct_based_on_melody_only = FALSE,
                            remove_redundancy = FALSE,
                            remove_melodies_with_only_repeated_notes = FALSE)


load("~/RTT/RTT_phrase.rda")


RTT_item_bank <- cbind(item_bank,
                       RTT_item_bank %>% select(-item_id) )


use_data(RTT_item_bank, overwrite = TRUE)


# Add to DB

library(DBI)

db_con <- musicassessrdb::musicassessr_con(db_name = "melody_dev")

dbWriteTable(db_con,
             name = 'item_bank_RTT_phrase',
             value = RTT_item_bank,
             row.names = FALSE,
             append = FALSE,
             overwrite = TRUE)

musicassessrdb::db_disconnect(db_con)


# Prod


db_con <- musicassessrdb::musicassessr_con(db_name = "melody_prod")

dbWriteTable(db_con,
             name = 'item_bank_RTT_phrase',
             value = RTT_item_bank,
             row.names = FALSE,
             append = FALSE,
             overwrite = TRUE)


musicassessrdb::db_disconnect(db_con)

