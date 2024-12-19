



#' Launch the Rhythm Tapping Task
#'
#' @param app_name
#' @param musicassessr_aws
#' @param data_collection_method
#' @param feedback
#' @param num_items
#' @param num_examples
#' @param call_and_response_end
#' @param call_and_response_bpm
#' @param get_p_id
#' @param sync_beat_bpm_range
#' @param sync_beat_bpms A vector of BPMs.
#' @param filter_call_and_response_stimuli_length NULL = not filtered. Or a vector of the range of stimulus lengths. e.g., 3:12 will only have stimulus lengths 3-12.
#' @param opening_and_final_image A string to the location of the image to use on the first and final pages.
#' @param mute_midi_playback Should user audio feedback be muted when using a MIDI pad?
#'
#' @return
#' @export
#'
#' @examples
RTT_standalone <- function(app_name = "RTT",
                           musicassessr_aws = FALSE,
                           data_collection_method = c("midi", "audio", "key_presses"),
                           feedback = rhythm_feedback(type = "none"),
                           num_items = list(free_recall = 3L,
                                            sync_beat = 3L,
                                            call_and_response = 10L),
                           num_examples = list(free_recall = 2L,
                                               sync_beat = 2L,
                                               call_and_response = 2L),
                           call_and_response_end = c("manual", "auto"),
                           call_and_response_bpm = "user_determined",
                           get_p_id = TRUE,
                           sync_beat_bpm_range = 60:200,
                           sync_beat_bpms = NULL,
                           filter_call_and_response_stimuli_length = NULL,
                           opening_and_final_image = "https://adaptiveeartraining.com/assets/drum.png",
                           mute_midi_playback = TRUE) {

  data_collection_method <- match.arg(data_collection_method)
  call_and_response_end <- match.arg(call_and_response_end)
  page_type <- paste0("record_", data_collection_method, "_page")

  stopifnot(
            is.scalar.character(app_name),
            is.scalar.logical(musicassessr_aws),
            data_collection_method %in%  c("midi", "audio", "key_presses"),
            is.list(feedback) && length(feedback) == 2L && check_names_same(names(feedback), c("type", "fun")),
            is.list(num_items) && length(num_items) == 3L && check_names_same(names(num_items), c("free_recall", "sync_beat", "call_and_response")),
            is.list(num_examples) && length(num_examples) == 3L && check_names_same(names(num_items), c("free_recall", "sync_beat", "call_and_response")),
            is.scalar.logical(get_p_id),
            is.null.or(sync_beat_bpm_range, is.numeric),
            is.null.or(sync_beat_bpms, is.numeric),
            is.null.or(filter_call_and_response_stimuli_length, is.numeric),
            is.scalar.character(opening_and_final_image),
            is.scalar.logical(mute_midi_playback)
            )

  tl <- RTT(page_type,
            feedback,
            num_items,
            num_examples,
            call_and_response_end,
            call_and_response_bpm,
            setup_pages = FALSE, # This get sorted by make_musicassessr_test
            data_collection_method,
            get_p_id = FALSE,
            sync_beat_bpm_range = sync_beat_bpm_range,
            sync_beat_bpms = sync_beat_bpms,
            mute_midi_playback = mute_midi_playback)

  welcome_pg <- psychTestR::one_button_page(shiny::tags$div(shiny::tags$h2("Welcome to the Rhythm Tapping Test!"),
                                                            shiny::tags$img(src = opening_and_final_image, height = 200, width = 200)))


  final_page_ui <-   shiny::tags$div(shiny::tags$img(src = opening_and_final_image, height = 200, width = 200),
                                     shiny::tags$h3("Thank you for completing the test!"))


  musicassessr::make_musicassessr_test(
                         welcome_page = welcome_pg,
                         final_page = psychTestR::final_page(final_page_ui),
                         elts = tl,
                         title = "Rhythm Tapping Test",
                         admin_password = "demo",
                         opt = musicassessr::musicassessr_opt(app_name = app_name,
                                                              get_p_id = get_p_id,
                                                              midi_input = data_collection_method == "midi",
                                                              record_audio = data_collection_method == "audio",
                                                              musicassessr_aws = musicassessr_aws,
                                                              setup_options = musicassessr::setup_pages_options(input_type = if(data_collection_method == "midi") "midi_keyboard" else if(data_collection_method == "audio") "microphone" else "key_presses",
                                                                                                                headphones = TRUE,
                                                                                                                get_instrument_range = FALSE,
                                                                                                                SNR_test = FALSE,
                                                                                                                concise_wording = TRUE,
                                                                                                                mute_midi_playback = mute_midi_playback)))


}



#' Rhythm tapping test
#'
#' @param page_type
#' @param feedback
#' @param num_items
#' @param num_examples
#' @param call_and_response_end
#' @param call_and_response_bpm
#' @param setup_pages
#' @param data_collection_method
#' @param get_p_id
#' @param sync_beat_bpm_range
#' @param sync_beat_bpms A vector of BPMs.
#' @param filter_call_and_response_stimuli_length NULL = not filtered. Or a vector of the range of stimulus lengths. e.g., 3:12 will only have stimulus lengths 3-12.
#' @param mute_midi_playback Should user audio feedback be muted when using a MIDI pad?
#'
#' @return
#' @export
#'
#' @examples
RTT <- function(page_type = "record_midi_page",
                feedback = rhythm_feedback(type = "none"),
                num_items = list(free_recall = 3L,
                                 sync_beat = 3L,
                                 call_and_response = 3L),
                num_examples = list(free_recall = 2L,
                                    sync_beat = 2L,
                                    call_and_response = 2L),
                call_and_response_end = c("manual", "auto"),
                call_and_response_bpm = "user_determined",
                setup_pages = TRUE,
                data_collection_method = c("midi", "audio", "key_presses"),
                get_p_id = TRUE,
                sync_beat_bpm_range = 60:200,
                sync_beat_bpms = NULL,
                filter_call_and_response_stimuli_length = NULL,
                mute_midi_playback = TRUE) {

  data_collection_method <- match.arg(data_collection_method)


  RTT_checks(call_and_response_bpm, num_items, num_examples, sync_beat_bpm_range, sync_beat_bpms)

  call_and_response_end <- match.arg(call_and_response_end)

  stopifnot(
    page_type %in% c("record_midi_page", "record_audio_page", "record_key_presses_page"),
    is.list(feedback) && length(feedback) == 2L && check_names_same(names(feedback), c("type", "fun")),
    is.list(num_items) && length(num_items) == 3L && length(setdiff(names(num_items), c("free_recall", "sync_beat", "call_and_response"))) == 0,
    is.list(num_examples) && length(num_examples) == 3L && length(setdiff(names(num_items), c("free_recall", "sync_beat", "call_and_response"))) == 0,
    call_and_response_bpm == "user_determined" || is.scalar.numeric(call_and_response_bpm),
    is.scalar.logical(setup_pages),
    is.scalar.character(data_collection_method),
    is.scalar.logical(get_p_id),
    is.null.or(sync_beat_bpm_range, is.numeric),
    is.null.or(sync_beat_bpms, is.numeric),
    is.null.or(filter_call_and_response_stimuli_length, is.numeric),
    is.scalar.logical(mute_midi_playback)
  )

  function() {

    print('adaas?')

    psychTestR::join(
      if(get_p_id) psychTestR::get_p_id(),
      if(setup_pages) musicassessr::setup_pages(input_type = if(data_collection_method == "midi") "midi_keyboard" else if(data_collection_method == "audio") "microphone" else "key_presses", headphones = TRUE, get_instrument_range = FALSE, SNR_test = FALSE, concise_wording = TRUE, mute_midi_playback = mute_midi_playback),

      # Free Recall Trials
      ## Examples
      if(num_examples$free_recall > 0L) rhythm_free_recall_trials(num_items = num_examples$free_recall, page_type = page_type, feedback = feedback, with_intro_page = num_examples$free_recall > 0L, with_example_introduction = TRUE, give_average_bpm = identical(feedback$type, "researcher"), mute_midi_playback = mute_midi_playback),
      ## Real Trials
      if(num_examples$free_recall > 0L) psychTestR::one_button_page("Now you're ready for the real thing!"),
      if(num_items$free_recall > 0L) rhythm_free_recall_trials(num_items = num_items$free_recall, page_type = page_type, feedback = feedback, with_intro_page = num_examples$free_recall < 1L, give_average_bpm = identical(feedback$type, "researcher"), mute_midi_playback = mute_midi_playback), # i.e., only give the average if using "researcher" feedback mode

      # Sync Beat Trials
      ## Examples
      if(num_examples$sync_beat > 0L) sync_beat_trials(num_items = num_examples$sync_beat, page_type = page_type, feedback = feedback, with_intro_page = num_examples$sync_beat > 0L, with_example_introduction = TRUE, bpm_range = sync_beat_bpm_range, preset_bpms = rep(sync_beat_bpms[1], num_examples$sync_beat), mute_midi_playback = mute_midi_playback),
      ## Real Trials
      if(num_examples$sync_beat > 0L) psychTestR::one_button_page("Now you're ready for the real thing!"),
      if(num_items$sync_beat > 0L) sync_beat_trials(num_items = num_items$sync_beat, page_type = page_type, feedback = feedback, with_intro_page = num_examples$sync_beat < 1L, bpm_range = sync_beat_bpm_range, preset_bpms = sync_beat_bpms, mute_midi_playback = mute_midi_playback),

      # Call and Response Trials
      ## Examples
      if(num_examples$call_and_response > 0L) rhythm_call_and_response_trials(num_items = num_examples$call_and_response, bpm = call_and_response_bpm, page_type = page_type, feedback = feedback, with_intro_page = num_examples$call_and_response > 0L, with_example_introduction = TRUE, filter_call_and_response_stimuli_length = filter_call_and_response_stimuli_length, mute_midi_playback = mute_midi_playback),
      ## Real Trials
      if(num_examples$call_and_response > 0L) psychTestR::one_button_page("Now you're ready for the real thing!"),
      if(num_items$call_and_response > 0L) rhythm_call_and_response_trials(num_items = num_items$call_and_response, bpm = call_and_response_bpm, page_type = page_type, feedback = feedback, call_and_response_end = call_and_response_end, with_intro_page = num_examples$call_and_response < 1L, filter_call_and_response_stimuli_length = filter_call_and_response_stimuli_length, mute_midi_playback = mute_midi_playback)
    )
  }
}




rhythm_free_recall_trials <- function(num_items = 3,
                                      give_average_bpm = TRUE,
                                      page_type = "record_midi_page",
                                      page_title = "Tap a steady beat",
                                      page_text = "Please tap a steady beat, then click Stop.",
                                      feedback = rhythm_feedback(type = "none"),
                                      with_intro_page = TRUE,
                                      with_example_introduction = FALSE,
                                      label = "rhythm_free_recall",
                                      mute_midi_playback = TRUE) {

  stopifnot(
    is.scalar.logical(with_intro_page),
    is.scalar.logical(with_example_introduction),
    is.scalar.logical(mute_midi_playback)
  )


  block <- if(page_type == "record_midi_page") {
    musicassessr::record_midi_block(no_pages = num_items,
                                    label = paste0(label, ".", page_type),
                                    page_title = page_title,
                                    page_text = page_text,
                                    get_answer = function(input, state, ...) {
                                      musicassessr::get_answer_rhythm_production(input, state, type = "midi", ...)
                                    },
                                    mute_midi_playback = mute_midi_playback)
  } else if(page_type == "record_audio_page") {
    musicassessr::record_audio_block(no_pages = num_items,
                                     label = paste0(label, ".", page_type),
                                     page_title = page_title,
                                     page_text = page_text,
                                     get_answer = function(input, state, ...) {
                                       musicassessr::get_answer_rhythm_production(input, state, type = "audio", ...)
                                     })
  } else if(page_type == "record_key_presses_page") {
    musicassessr::record_key_presses_block(no_pages = num_items,
                                           label = paste0(label, ".", page_type),
                                            page_title = page_title,
                                            page_text = page_text,
                                            get_answer = function(input, state, ...) {
                                              musicassessr::get_answer_rhythm_production(input, state, type = "key_presses", ...)
                                            })
  } else {
    stop("Page type not known")
  }

  # Note that, whilst the record_xxx_block functions have their own feedback arguments, we add
  # the feedback manually here so that we can use musicassessr::feedback_with_progress
  block <- add_rtt_feedback(block, feedback)

  intro_page <- psychTestR::one_button_page(
    shiny::tags$div(
      shiny::tags$p("On the next set of pages, please tap a steady beat for 10 seconds."),
      shiny::tags$p("You can tap at whatever comfortable speed is good for you.")
    ))

  psychTestR::join(

    if(with_intro_page) intro_page,
    if(with_example_introduction) psychTestR::one_button_page("First try some examples."),

    # Main trial block
    block,

    if(give_average_bpm) {
      psychTestR::reactive_page(function(state, ...) {
        results <- psychTestR::results(state)$results

        bpms <- purrr::map_int(results, function(i) {
          if(is.scalar.na.or.null(i$user_bpm)) NA else i$user_bpm
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





sync_beat_trials <- function(num_items,
                             bpm_range = 60:200,
                             length_in_seconds = 10,
                             page_type = "record_midi_page",
                             feedback = rhythm_feedback(type = "none"),
                             with_intro_page = TRUE,
                             with_example_introduction = FALSE,
                             preset_bpms = NULL,
                             label = "sync_beat_trial_page",
                             mute_midi_playback = TRUE) {

  if(!is.null(bpm_range)) {

    smp <- sample(bpm_range, size = num_items) %>% sort()
    trials <- purrr::map(smp, function(bpm) sync_beat_trial_page(bpm = bpm, length_in_seconds = length_in_seconds, page_type = page_type, label = label, mute_midi_playback = mute_midi_playback))

  } else if(!is.null(preset_bpms)) {

    trials <- purrr::map(preset_bpms, function(bpm) sync_beat_trial_page(bpm = bpm, length_in_seconds = length_in_seconds, page_type = page_type, label = label, mute_midi_playback = mute_midi_playback))

  } else {
    stop("Steady beat trial parameters not understood")
  }


  # Add feedback
  trials <- add_rtt_feedback(trials, feedback)

  psychTestR::join(
    if(with_intro_page) psychTestR::one_button_page("On the next set of pages, please try and tap along in time with the beat that you hear."),
    if(with_example_introduction) psychTestR::one_button_page("First try some examples."),
    trials
  )

}

sync_beat_trial_page <- function(bpm = 120, length_in_seconds = 5, page_type = "record_midi_page", label = "sync_beat_trial_page", mute_midi_playback = TRUE) {

  psychTestR::reactive_page(function(state, ...) {

    midi_device <- psychTestR::get_global("midi_device", state)

    midi_device <- if(is.null(midi_device)) "" else midi_device


    if(bpm == "user_determined") {
      bpm <- psychTestR::get_global("user_bpm", state)
    }

    logging::loginfo("BPM: %s", bpm)

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
                                  page_label = paste0(label, ".", page_type),
                                  midi_device = midi_device,
                                  sound = 'rhythm',
                                  get_answer = function(input, state, ...) {
                                    musicassessr::get_answer_rhythm_production(input, state, type = type_from_page(page_type), ...)
                                  },
                                  mute_midi_playback = mute_midi_playback,
                                  trigger_start_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "simultaneous_recall", page_type = page_type, call_and_response_end = "auto", midi_device = if(page_type == "record_midi_page") midi_device else NULL, instantiate_midi = page_type == "record_midi_page", mute_midi_playback = mute_midi_playback)$trigger_start_of_stimulus_fun,
                                  trigger_end_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "simultaneous_recall", page_type = page_type, call_and_response_end = "auto", mute_midi_playback = mute_midi_playback)$trigger_end_of_stimulus_fun,
    )
  })
}





rhythm_call_and_response_trials <-  function(num_items = 10,
                                             bpm = "user_determined",
                                             page_type = "record_midi_page",
                                             feedback = rhythm_feedback(type = "none"),
                                             call_and_response_end = c("manual", "auto"),
                                             with_intro_page = TRUE,
                                             with_example_introduction = FALSE,
                                             filter_call_and_response_stimuli_length = NULL,
                                             label = "rhythm_call_and_response",
                                             mute_midi_playback = TRUE) {

  call_and_response_end <- match.arg(call_and_response_end)

  if(!is.null(filter_call_and_response_stimuli_length)) {
    logging::loginfo("Filtering stimulus lengths to be between %s and %s", filter_call_and_response_stimuli_length[1], filter_call_and_response_stimuli_length[2])
    smp_rhythm <- RTT::RTT_item_bank %>%
      dplyr::filter(dplyr::between(stimulus_length, filter_call_and_response_stimuli_length[1], filter_call_and_response_stimuli_length[2])) %>%
      dplyr::slice_sample(n = num_items)
  } else {
    smp_rhythm <- RTT::RTT_item_bank %>%
      dplyr::slice_sample(n = num_items)
  }




  trials <- purrr::pmap(smp_rhythm, function(pattern, pattern_split, type, RAT_difficulty, durations_bpm_120, stimulus_length) {

    psychTestR::reactive_page(function(state, ...) {

      midi_device <- psychTestR::get_global("midi_device", state)

      if(is.null(bpm) || bpm == "user_determined") {
        bpm <- psychTestR::get_global("user_bpm", state)
      }

      logging::loginfo("Using user BPM: %s", bpm)

      if(is.null(midi_device) && page_type == "record_midi_page") { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

      rhythm <- RTT::pattern_to_ms(pattern = itembankr::str_mel_to_vector(pattern_split), bpm = bpm, type = type)

      musicassessr::present_stimuli(stimuli = rep(60, length(rhythm)),
                                    durations = rhythm,
                                    display_modality = "auditory",
                                    page_label = paste0(label, ".", page_type),
                                    stimuli_type = "midi_notes",
                                    page_title = "Tap back the rhythm.",
                                    page_text = "Please tap back a rhythm after you hear it, then click stop.",
                                    page_type = page_type,
                                    midi_device = if(is.null(midi_device)) "" else midi_device,
                                    trigger_start_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "call_and_response", page_type = page_type, call_and_response_end = call_and_response_end, mute_midi_playback = mute_midi_playback)$trigger_start_of_stimulus_fun,
                                    trigger_end_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "call_and_response", page_type = page_type, call_and_response_end = call_and_response_end, mute_midi_playback = mute_midi_playback)$trigger_end_of_stimulus_fun,
                                    sound = 'rhythm',
                                    answer_meta_data = tibble::tibble(pattern = pattern_split, bpm = bpm, type = type),
                                    get_answer = function(input, state, ...) {
                                      musicassessr::get_answer_rhythm_production(input, state, type = type_from_page(page_type), ...)
                                    },
                                    mute_midi_playback = mute_midi_playback
                                    )

    })

  })

  # Add feedback
  trials <- add_rtt_feedback(trials, feedback)

  psychTestR::join(
    if(with_intro_page) psychTestR::one_button_page("On the next set of pages, please tap back the rhythm after you hear it, then click Stop."),
    if(with_example_introduction) psychTestR::one_button_page("First try some examples."),
    trials
  )
}


RTT_checks <- function(call_and_response_bpm, num_items, num_examples, sync_beat_bpm_range, sync_beat_bpms) {

  if(call_and_response_bpm == "user_determined" && num_items$free_recall < 1L && num_items$call_and_response > 0L) {
    stop("If num_items$free_recall is < 1 and num_items$call_and_response > 0L, then call_and_response_bpm must be an integer representing the BPM")
  }

  if(num_items$free_recall < 1L && num_examples$free_recall > 0L) {
    stop("num_examples$free_recall cannot be greater than 0L if num_items$free_recall is < 1")
  }

  if(num_items$sync_beat < 1L && num_examples$sync_beat > 0L) {
    stop("num_examples$sync_beat cannot be greater than 0L if num_items$sync_beat is < 1")
  }

  if(num_items$call_and_response < 1L && num_examples$call_and_response > 0L) {
    stop("num_examples$call_and_response cannot be greater than 0L if num_items$call_and_response is < 1")
  }

  if(!is.null(sync_beat_bpm_range) && !is.null(sync_beat_bpms)) {
    stop("One of sync_beat_bpm_range or sync_beat_bpms must be NULL")
  }

  if(!is.null(sync_beat_bpms) && length(sync_beat_bpms) != num_items$sync_beat) {
    stop("The length of sync_beat_bpms must be the same as the length of num_items$sync_beat or NULL")
  }
}
