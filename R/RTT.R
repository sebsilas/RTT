



#' Launch the Rhythm Tapping Task
#'
#' @param app_name
#' @param musicassessr_aws
#' @param data_collection_method
#'
#' @return
#' @export
#'
#' @examples
RTT_standalone <- function(app_name = "RTT",
                           musicassessr_aws = FALSE,
                           data_collection_method = c("midi", "audio", "key_presses")) {

  data_collection_method <- match.arg(data_collection_method)
  page_type <- paste0("record_", data_collection_method, "_page")

  stopifnot(
            is.scalar.character(app_name),
            is.scalar.logical(musicassessr_aws),
            data_collection_method %in%  c("midi", "audio", "key_presses")
            )

  tl <- RTT_tl(page_type)

  musicassessr::make_musicassessr_test(
                         welcome_page = psychTestR::one_button_page("Welcome to the Rhythm Tapping Test!"),
                         final_page_ui = "Thank you for completing the test!",
                         elts = tl,
                         title = "Rhythm Tapping Test",
                         admin_password = "demo",
                         opt = musicassessr::musicassessr_opt(app_name = app_name,
                                                              midi_input = data_collection_method == "midi",
                                                              record_audio = data_collection_method == "audio",
                                                              musicassessr_aws = musicassessr_aws,
                                                              setup_options = musicassessr::setup_pages_options(input_type = if(data_collection_method == "midi") "midi_keyboard" else if(data_collection_method == "audio") "microphone" else "key_presses",
                                                                                                                headphones = TRUE,
                                                                                                                get_instrument_range = FALSE,
                                                                                                                SNR_test = FALSE,
                                                                                                                concise_wording = TRUE))
  )


}



RTT_tl <- function(page_type = "record_midi_page") {

  function() {
    psychTestR::join(
      #rhythm_free_recall_trials(num_items = 3L, page_type = page_type),
      steady_beat_trials(num_items = 5L, page_type = page_type),
      rhythm_call_and_response_trials(num_items = 5L, bpm = "user_determined", page_type = page_type)
    )
  }
}




rhythm_free_recall_trials <-  function(num_items = 3,
                                       give_average_bpm = TRUE,
                                       page_type = "record_midi_page",
                                       page_title = "Tap a steady beat",
                                       page_text = "Please tap a steady beat, then click Stop.") {


  block <- if(page_type == "record_midi_page") {
    musicassessr::record_midi_block(no_pages = num_items,
                                    page_title = page_title,
                                    page_text = page_text,
                                    get_answer = function(input, state, ...) {
                                      musicassessr::get_answer_rhythm_production(input, state, type = "midi", ...)
                                    })
  } else if(page_type == "record_audio_page") {
    musicassessr::record_audio_block(no_pages = num_items,
                                     page_title = page_title,
                                     page_text = page_text,
                                     get_answer = function(input, state, ...) {
                                       musicassessr::get_answer_rhythm_production(input, state, type = "audio", ...)
                                     }
                                     )
  } else if(page_type == "record_key_presses_page") {
    musicassessr::record_key_presses_block(no_pages = num_items,
                                            page_title = page_title,
                                            page_text = page_text,
                                            get_answer = function(input, state, ...) {
                                              musicassessr::get_answer_rhythm_production(input, state, type = "key_presses", ...)
                                            })
  } else {
    stop("Page type not known")
  }

  psychTestR::join(

    psychTestR::one_button_page(
      shiny::tags$div(
        shiny::tags$p("On the next set of pages, please tap a steady beat for 10 seconds."),
        shiny::tags$p("You can tap at whatever comfortable speed is good for you.")
      )),

    # Main trial block
    block,

    if(give_average_bpm) {
      psychTestR::reactive_page(function(state, ...) {
        results <- psychTestR::results(state)$results

        bpms <- purrr::map_int(results, function(i) {
          if(is.null(i$user_bpm)) NA else i$user_bpm
        }) %>% as.numeric()

        avg_bpm <- round(mean(bpms, na.rm = TRUE))

        psychTestR::set_global("user_bpm", avg_bpm, state)

        if(is.nan(avg_bpm)) {
          psychTestR::one_button_page("Your average BPM could not be computed.")
        } else {
          psychTestR::one_button_page(paste0("Your average BPM is ", avg_bpm, "."))
        }

      })
    }
  )


}





steady_beat_trials <- function(num_items, bpm_range = 60:200, length_in_seconds = 10, page_type = "record_midi_page") {

  smp <- sample(bpm_range, size = num_items) %>% sort()

  trials <- purrr::map(smp, function(bpm) steady_beat_trial_page(bpm = bpm, length_in_seconds = length_in_seconds, page_type = page_type))

  psychTestR::join(
    psychTestR::one_button_page("On the next set of pages, please try and tap along in time with the beat that you hear."),
    trials
  )

}

steady_beat_trial_page <- function(bpm = 120, length_in_seconds = 5, page_type = "record_midi_page") {

  print('steady_bea')
  print(page_type)

  psychTestR::reactive_page(function(state, ...) {

    midi_device <- psychTestR::get_global("midi_device", state)

    midi_device <- if(is.null(midi_device)) "" else midi_device


    if(bpm == "user_determined") {
      bpm <- psychTestR::get_global("user_bpm", state)
    }

    beats_per_second <- bpm/60
    length_of_note <- 1/beats_per_second
    no_notes <- length_in_seconds/length_of_note
    beat <- rep(length_of_note, no_notes)


    musicassessr::present_stimuli(stimuli = rep(60, length(beat)),
                                  durations = beat,
                                  display_modality = "auditory",
                                  stimuli_type = "midi_notes",
                                  page_title = "Tap along with the beat",
                                  page_text = "Tap along with the beat as best you can.",
                                  page_type = page_type,
                                  midi_device = midi_device,
                                  sound = 'rhythm',
                                  get_answer = function(input, state, ...) {
                                    musicassessr::get_answer_rhythm_production(input, state, type = type_from_page(page_type), ...)
                                  },
                                  trigger_start_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "simultaneous_recall", page_type = page_type, call_and_response_end = "auto")$trigger_start_of_stimulus_fun,
                                  trigger_end_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "simultaneous_recall", page_type = page_type, call_and_response_end = "auto")$trigger_end_of_stimulus_fun,
    )
  })
}





rhythm_call_and_response_trials <-  function(num_items = 10,
                                             bpm = "user_determined",
                                             page_type = "record_midi_page") {

  smp_rhythm <- RTT::RTT_item_bank %>%
    dplyr::slice_sample(n = num_items)


  trials <- purrr::pmap(smp_rhythm, function(pattern, pattern_split, type, RAT_difficulty, durations_bpm_120) {

    psychTestR::reactive_page(function(state, ...) {

      midi_device <- psychTestR::get_global("midi_device", state)

      if(is.null(bpm) || bpm == "user_determined") {
        bpm <- psychTestR::get_global("user_bpm", state)
      }

      print(paste0("Using user BPM: ", bpm))

      if(is.null(midi_device)) { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

      rhythm <- RTT::pattern_to_ms(pattern = itembankr::str_mel_to_vector(pattern_split), bpm = bpm, type = type)

      musicassessr::present_stimuli(stimuli = rep(60, length(rhythm)),
                                    durations = rhythm,
                                    display_modality = "auditory",
                                    stimuli_type = "midi_notes",
                                    page_title = "Tap back the rhythm.",
                                    page_text = "Please tap back a rhythm after you hear it, then click stop.",
                                    page_type = page_type,
                                    midi_device = midi_device,
                                    trigger_start_of_stimuli_fun = musicassessr:: paradigm("call_and_response", page_type = page_type)$beginning_of_stimulus_fun,
                                    trigger_end_of_stimuli_fun = musicassessr:: paradigm("call_and_response", page_type = page_type)$end_of_stimulus_fun,
                                    sound = 'rhythm',
                                    answer_meta_data = tibble::tibble(pattern = pattern_split, bpm = bpm, type = type),
                                    get_answer = function(input, state, ...) {
                                      musicassessr::get_answer_rhythm_production(input, state, type = type_from_page(page_type), ...)
                                    })

    })

  })

  psychTestR::join(
    psychTestR::one_button_page("On the next set of pages, please tap back the rhythm after you hear it, then click Stop."),
    trials
  )
}


#
#
# # Profile time alignment
#
#
# stimuli <- rep(0.5, 10) %>%
#   cumsum() %>%
#   c(0, .) %>%
#   tibble::tibble(stimuli = .)
#
# # Jitter = 1
# ran_seq <- stimuli %>%
#   pull(stimuli) %>%
#   jitter(1) %>%
#   tibble::tibble(user_rhythm = .) %>%
#   mutate(user_rhythm = case_when(user_rhythm < 0 ~ 0, TRUE ~ user_rhythm))
#
# # Jitter = 3
#
# ran_seq_3 <- stimuli %>%
#   pull(stimuli) %>%
#   jitter(3) %>%
#   tibble::tibble(user_rhythm = .) %>%
#   mutate(user_rhythm = case_when(user_rhythm < 0 ~ 0, TRUE ~ user_rhythm))
#
# # Jitter = 5
#
# ran_seq_5 <- stimuli %>%
#   pull(stimuli) %>%
#   jitter(5) %>%
#   tibble::tibble(user_rhythm = .) %>%
#   mutate(user_rhythm = case_when(user_rhythm < 0 ~ 0, TRUE ~ user_rhythm))
#
# # Jitter = 23
#
# ran_seq_23 <- stimuli %>%
#   pull(stimuli) %>%
#   jitter(23) %>%
#   tibble::tibble(user_rhythm = .) %>%
#   mutate(user_rhythm = case_when(user_rhythm < 0 ~ 0, TRUE ~ user_rhythm))
#
#
# # Plot
# ggplot() +
#   geom_vline(aes(xintercept = stimuli), color = "lightgrey", data = stimuli) +
#   geom_vline(aes(xintercept = user_rhythm), color = "purple", data = ran_seq) +
#   theme_void()
#
#
# ggplot() +
#   geom_vline(aes(xintercept = stimuli), color = "lightgrey", data = stimuli) +
#   geom_vline(aes(xintercept = user_rhythm), color = "purple", data = ran_seq_3) +
#   theme_void()
#
# ggplot() +
#   geom_vline(aes(xintercept = stimuli), color = "lightgrey", data = stimuli) +
#   geom_vline(aes(xintercept = user_rhythm), color = "purple", data = ran_seq_5) +
#   theme_void()
#
#
# ggplot() +
#   geom_vline(aes(xintercept = stimuli), color = "lightgrey", data = stimuli) +
#   geom_vline(aes(xintercept = user_rhythm), color = "purple", data = ran_seq_11) +
#   theme_void()
#
#
# # TAM
#
# TAMDistance(stimuli$stimuli, stimuli$stimuli)
#
# TAMDistance(stimuli$stimuli, ran_seq$user_rhythm) # TAM is not sensitive to small perturbations..?
#
# TAMDistance(stimuli$stimuli, ran_seq_3$user_rhythm)
#
# TAMDistance(stimuli$stimuli, ran_seq_5$user_rhythm)
#
# TAMDistance(stimuli$stimuli, ran_seq_23$user_rhythm)
#
#
# # dtw
#
#
# dtw_dist <- dtw::dtw(stimuli$stimuli, stimuli$stimuli)
# dtw_dist$distance
#
# dtw_dist_1 <- dtw::dtw(stimuli$stimuli, ran_seq$user_rhythm)
# dtw_dist_1$distance
#
# dtw_dist_3 <- dtw::dtw(stimuli$stimuli, ran_seq_3$user_rhythm)
# dtw_dist_3$distance
#
# dtw_dist_5 <- dtw::dtw(stimuli$stimuli, ran_seq_5$user_rhythm)
# dtw_dist_5$distance
#
# dtw_dist_23 <- dtw::dtw(stimuli$stimuli, ran_seq_23$user_rhythm)
# dtw_dist_23$distance
#
# # Maybe good old dtw is better?
