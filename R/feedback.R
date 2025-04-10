

add_rtt_feedback <- function(block, feedback, after = 2, scale_pr = 1) {
  if(feedback$type == "none") {
    block <- block
  } else if(feedback$type == "image") {
    block <- musicassessr::add_feedback_with_progress(block, feedback$fun, after = after, scale_pr = scale_pr)
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
#' @param get_async_data
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
                            progress = NULL,
                            get_async_data = FALSE) {

  if(type == "none") {
    fb <- FALSE
  } else if(type == "image") {

    image_fun <- function(image, height = NULL, width = NULL, text, progress = NULL, get_async_data = FALSE) {
      function(progress) {
        musicassessr::feedback_image(image, height, width, text, progress, get_async_data)
      }
    }

    # Enclose
    fb <- image_fun(image, height, width, text, get_async_data = get_async_data)

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
