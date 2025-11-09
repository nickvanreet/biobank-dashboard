# R/ui/ui_utils.R
# UI utility functions
# ============================================================================

#' Safe icon wrapper with fallback
#' @param name Icon name (Bootstrap Icons or Font Awesome)
#' @param ... Additional arguments
#' @return Icon object or NULL
#' @export
safe_icon <- function(name, ...) {
  if (is.null(name) || is.na(name) || name == "") {
    return(NULL)
  }
  
  tryCatch({
    # Try Bootstrap Icons first
    bsicons::bs_icon(name, ...)
  }, error = function(e1) {
    tryCatch({
      # Fallback to Font Awesome
      shiny::icon(name, ...)
    }, error = function(e2) {
      # Return NULL instead of another icon to avoid recursion
      NULL
    })
  })
}

#' Create a notification with standard styling
#' @param message Message text
#' @param type Type of notification ("info", "success", "warning", "error")
#' @param duration Duration in seconds
#' @export
show_notification <- function(message, type = "info", duration = 5) {
  showNotification(
    message,
    type = type,
    duration = duration
  )
}

#' Create a value box with error handling
#' @param title Box title
#' @param value Box value (can be reactive output)
#' @param showcase Icon to show
#' @param theme Color theme
#' @export
safe_value_box <- function(title, value, showcase = NULL, theme = "primary") {
  tryCatch({
    bslib::value_box(
      title = title,
      value = value,
      showcase = showcase,
      theme = theme
    )
  }, error = function(e) {
    bslib::value_box(
      title = title,
      value = "Error",
      showcase = safe_icon("exclamation-triangle"),
      theme = "danger"
    )
  })
}
