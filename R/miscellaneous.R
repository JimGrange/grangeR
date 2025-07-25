#' Opposite of %in%: Not-In Operator
#'
#' Returns a logical vector indicating if elements of the left-hand vector (`x`)
#' are not present in the right-hand vector (`table`). This is the logical
#' negation of `%in%`.
#'
#' @param x A vector of values to be tested.
#' @param table A vector, matrix, or data frame to be matched against.
#' Each element of `x` is tested for absence from this object.
#'
#' @return A logical vector of the same length as `x`.
#' @export
`%notin%` <- function(x, table) {
  !(x %in% table)
}
