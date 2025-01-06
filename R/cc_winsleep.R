#' Set Windows sleep Option
#'
#' Set number of minutes after which Windows should sleep when plugged in
#'
#' @param minutes The number of minutes after which the device should go to sleep
#'
#' @details
#' This is a convenience function to temporarily modify / disable the number of minutes after which
#' when Windows goes to sleep when idle when plugged in.
#'
#' This function can be run before starting a lengthy process, to prevent
#' windows from going to sleep when there are no mouse or keyboard interactions (i.e., 'idle').
#' Setting the number of minutes to '0' disables going to sleep completely.
#'
#' Note that this function is i) for windows only, and ii) only sets the sleep setting that's in effect when the computer is plugged in.
#'
#' @export

cc_winsleep <- function(minutes = 30) {

  if (.Platform$OS.type == "windows") {
    ret_code <- system(paste0("powercfg /change standby-timeout-ac ", minutes))
    if (ret_code == 0) {
      if (minutes == 0) {
        message(crayon::green(paste0(" - Going to sleep disabled")))
      } else {
        message(crayon::green(paste0(" - Windows will go to sleep after being 'idle' for ", minutes, " minutes")))
      }

    } else {
      warning("The return code was not zero (may not have worked)")
    }
    invisible(ret_code)
  } else {
    stop("This function only works on Windows")
  }
}






