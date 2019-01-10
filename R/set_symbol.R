#' Turn mark counts into a symbol
#'
#' Set symbols for each mark value.
#'
#' "" for 0 marks
#' "/" for 1 mark
#' "X" for 2 marks
#' "0" for 3 marks
#'
#' @param number_of_marks vector of the number of marks to turn into a symbol
#'
#' @return vector of symbols
#'
#' @export
set_symbol <- function(number_of_marks) {

  # Set symbols for each mark value.
  # "" for 0 marks
  # "/" for 1 mark
  # "X" for 2 marks
  # "0" for 3 marks
  sym <- rep("", length(number_of_marks))
  sym <- ifelse(number_of_marks == 0, "",
         ifelse(number_of_marks == 1, "/",
         ifelse(number_of_marks == 2, "X",
         ifelse(number_of_marks == 3, "0", "ERROR"))))

  # Return symbols
  sym

}
