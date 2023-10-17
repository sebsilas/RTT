

add_rtt_feedback <- function(block, feedback) {
  if(feedback$type == "none") {
    block <- block
  } else if(feedback$type == "image") {
    block <- musicassessr::add_feedback_with_progress(block, feedback$fun)
  } else if(feedback$type == "researcher") {
    block <- musicassessr::add_feedback(block, feedback$fun)
  } else {
    stop("Feedback type not recognised")
  }
  block
}

#' Setup feedback
#'
#' @param type
#' @param text
#' @param image
#' @param height
#' @param width
#' @param progress
#'
#' @return
#' @export
#'
#' @examples
rhythm_feedback <- function(type,
                            text = "Well done!",
                            image = 'https://adaptiveeartraining.com/assets/drum.png',
                            height = NULL,
                            width = NULL,
                            progress = NULL) {

  if(type == "none") {
    fb <- FALSE
  } else if(type == "image") {

    image_fun <- function(image, height = NULL, width = NULL, text, progress = NULL) {
      function(progress) {
        musicassessr::feedback_image(image, height, width, text, progress)
      }
    }

    # Enclose
    fb <- image_fun(image, height, width, text)

  } else if(type == "researcher") {

    fb <- musicassessr::feedback_rhythm_production

  } else {
    stop("Feedback type not recognised, must be 'none', 'image', or 'researcher'.")
  }

  # Return a list with the type and the function

  fb <- list(
    type = type,
    fun = fb
  )

  return(fb)

}
