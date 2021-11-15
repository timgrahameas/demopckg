#' report_p
#'
#' `report_p` does something good.
#'
#' @param p A value to evaluate
#' @param digits is significant figures
#' @return Returns the value of \code{p_string}
#' @examples
#'
#' report_p(1) # returns 1
#' 
#' @export
report_p <- function(p, digits = 3) {
  if (p < 0) stop("p cannot be less than 0")
  if (p > 1) stop("p cannot be greater than 1")
  if (p == 0) stop("p cannot be 0")
  if (!(digits %in% 1:5)) {
    warning("digits should probably be an integer between 1 and 5")
    digits = 3
  }
  
  p_round <- round(p, digits) %>%
    as.character() %>%
    # omit leading zero for APA-style
    stringr::str_replace(pattern="0.", replacement=".") %>%
    # pad right with zeros
    stringr::str_pad(width=digits+1, side="right", pad="#")
  
  p_string <- paste("p =", p_round)
  
  return(p_string)
}