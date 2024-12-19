
#' Constructor for 2023 calibration study
#'
#' @param app_name
#' @param data_collection_method
#'
#' @return
#' @export
#'
#' @examples
RTT_calibration_constructor <- function(app_name = "RTT_calibration_study",
                                        data_collection_method = "audio") {

  RTT::RTT_standalone(app_name = app_name,
                      data_collection_method = data_collection_method,
                      feedback = RTT::rhythm_feedback(type = "image", image = "https://musicassessr.com/assets/robot.png", height = 200, width = 200),
                      num_examples = list(free_recall = 1L, sync_beat = 1L, call_and_response = 1L),
                      num_items = list(free_recall = 3L, sync_beat = 6L, call_and_response = 5L),
                      call_and_response_bpm = 69,
                      sync_beat_bpm_range = NULL,
                      sync_beat_bpms = c(140, 140, 70, 70, 210, 210),
                      filter_call_and_response_stimuli_length = 3:12,
                      opening_and_final_image = "https://musicassessr.com/assets/robot.png")
}
