#' Determine the mode of any given vector \code{x}
#'
#' It's kinda dumb that R doesn't have a built-in mode function. So here.
#'
#' @param x vector of values; can be integers/strings/booleans/etc.
#'
#' @return value the value that occurs most often
#'
#' @examples
#' # Construct vector where 10 appears three times.
#' x <- c(1:10, 10:20, 10:30)
#' mode.value(x)
#'
#' @export
mode.value <- function(x) {
    unique.x <- base::unique(x)
    unique.x[base::which.max(base::tabulate(base::match(x, unique.x)))]
}

#' Compute the symmetric difference between two vectors.
#'
#' R's base setdiff function does a one-way set difference, e.g. setdiff(x, y) = x - y.
#' More often than not, I'm wondering what values are different between x and y -- both ways.
#' Hence, symmetric difference. Full disclosure: I copied the core of this function from the
#' following StackOverflow post, with some basic tidying for interactive analysis.
#'
#' Source: https://stackoverflow.com/questions/19797954/function-to-find-symmetric-difference-opposite-of-intersection-in-r
#'
#' @param x one of the two vectors (soon-to-be sets) of interest
#' @param y one of the two vectors (soon-to-be sets) of interest
#' @param pretty boolean denoting whether we should also output the one- and two-sided set differences.
#'               More appropriate for interactive analyses.
#'
#' @examples
#' # Find out what differences there are between 1:10 and 5:15
#' sym.diff(1:10, 5:15) # `1, 2, 3, 4, 11, 12, 13, 14, 15`
#'
#' # Verbose output version:
#' sym.diff(1:10, 5:15, pretty = TRUE) # Output one- and two-sided set difference
#' @export
sym.diff <- function(x, y, pretty = FALSE) {
  x.to.y <- setdiff(x, y)
  y.to.x <- setdiff(y, x)
  sym.diff <- unique(c(x.to.y, y.to.x))

  if (pretty)
    glue::glue(
      'In `x` but not `y`: `{LHS.items}`\n',
      'In `y` but not `x`: `{RHS.items}`\n',
      'Symmetric difference: `{all.items}`',
      LHS.items = paste0(x.to.y, collapse = ', '),
      RHS.items = paste0(y.to.x, collapse = ', '),
      all.items = paste0(sym.diff, collapse = ', ')
    ) %>% base::message()

  return(sym.diff)
}
