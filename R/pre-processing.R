#' Convert categorical variables into dummy/indicator variables
#'
#' This function is the R equivalent of Python's pandas' :get_dummmies(). It's a fairly
#' simple wrapper around \code{\link[caret]{dummyVars}}. This function binarizes all explicitly-factor
#' columns within the \code{data} argument. This function can be used in one of two ways
#' -- see 'Details'. TODO: support y as a quosure. TODO: change :y to :ignore vector.
#'
#' \enumerate{
#'   \item \code{dummify(x)}: no y parameter passed, so all factor columns are binarized
#'   \item \code{dummify(x, y = 'col_name')}: y parameter passed, so all factor columns except y are binarized.
#'   \itemize{
#'     \item{Note: }{For this second approach, the y column MUST be a factor. You've been warned.}
#'   }
#' }
#'
#' @param data any dataframe-esque object containing a mix of factor and numerical columns.
#'
#' @param y A response column. Pass this value to the function if the response (y) column
#' is a factor within :data that you would like to NOT be dummified. If not passed, we assume
#' all factors are free to be dummified. See 'Details'.
#'
#' @examples
#' x <- data.frame(
#'   id     = seq(1, 10),
#'   age    = rpois(n=10, lambda=20),
#'   gender = rep(c('M', 'F'), 5),
#'   blood.type = factor(rep(c('A', 'B'), 5))
#' )
#'
#' # Only expand `blood.type` factor column (`gender` is ignored because it's a character)
#' dummify(x)
#'
#' x$gender <- factor(x$gender)
#'
#' # Expand all columns into binary representations.
#' dummify(x)
#'
#' # Ignoring `gender`, binarize all other factors.
#' dummify(x, y = 'blood.type')
#'
#' @export
dummify <- function(data, y = '') {
  y.is.column <- y %in% base::names(data)
  y.is.factor <- dplyr::if_else(
    y.is.column && base::is.factor(dplyr::pull(data, y)),
    true = T, false = F
  )

  factor.columns <- data %>% dplyr::select_if(.predicate = base::is.factor)
  non.factor.columns <- data %>% dplyr::select_if(.predicate = ~ !base::is.factor(.))

  if (y.is.factor) {
    # Extract all factor column names and exclude :y.
    factor.col.names <- base::names(factor.columns)
    removed.y.factor.col.names <- base::setdiff(factor.col.names, y)

    # Build caret::dummyVars formula, listing all non-:y factors.
    joined.column.names <- base::paste0(removed.y.factor.col.names, collapse = ' + ')
    formula.column.names <- stats::as.formula(base::paste0('~', joined.column.names))
  } else {
    formula.column.names <- stats::as.formula('~ .')
  }

  # Convert all factor columns in our dataset to a dummy class, ensuring full rank.
  # e.g. ([1 2 3] => [[1 0 0 ], [0 1 0], [0 0 1]])
  dummy.output <- caret::dummyVars(
    formula = formula.column.names,
    data = factor.columns,
    fullRank = TRUE
  )

  # Generate tibble of our now-binary factors.
  # Warning: might take a while!
  dummy.columns <- stats::predict(dummy.output, newdata = data) %>% tibble::as.tibble()
  base::names(dummy.columns) <- dummy.columns %>% base::names() %>% base::make.names()

  # Generate the full dataset by omitting our original data's factor columns (including :y),
  # isolating the response column, then binding: (1) the response column :y,
  # (2) numeric-only columns, and (3) our new binary-only columns to :result.
  pre.result <- dplyr::bind_cols(non.factor.columns, dummy.columns)

  if (y.is.column) {
    y.only <- data[, y]
    result <- dplyr::bind_cols(y.only, pre.result)
  }

  return(result)
}

#' Identify, alert, and remove columns with near zero variance.
#'
#' This function is yet another fairly thin wrapper around \code{\link[caret]{nearZeroVar}}.
#' caret's function returns a vector of indices that are near zero variance, and this function
#' goes further to (a) inform the analyst of what columns are being removed and (b) removing
#' those columns.
#'
#' @param data any dataframe-esque object containing a mix of factor and numerical columns.
#'
#' @examples
#'
#' x <- data.frame(
#'   id     = seq(1, 10),
#'   age    = rpois(n=10, lambda=20),
#'   gender = rep(c('M', 'F'), 5),
#'   blood.type = factor(rep(c('A', 'B'), 5)),
#'   debt = factor(1e6)
#' )
#'
#' # Removes only `debt` because (1) it's a factor and (2) has zero variance.
#' remove.zero.var.factors(x)
#'
#' @export
remove.zero.var.factors <- function(data) {
  # Isolate our non-categorical and categorical columns.
  factor.cols <- data %>% dplyr::select_if(.predicate = base::is.factor)
  non.factor.cols <- data %>% dplyr::select_if(.predicate = ~ !base::is.factor(.))

  # For our categorical columns, un-select those that caret::nearZeroVar reports
  # as having, well, near zero variance. nearZeroVar() returns indices -- e.g. '3 9 20' --
  # so select(-c(3, 9, 20)) removes these columns. If no categorical variables
  # are near zero variance, none are removed.
  col.index.to.remove <- caret::nearZeroVar(factor.cols)

  # Print what columns we're removing.
  factor.cols %>%
    base::names() %>%
    .[col.index.to.remove] %>%
    purrr::map(~base::paste0('Removing factor column `', ., '` due to low variance.')) %>%
    purrr::flatten_chr() %>%
    base::message()

  removed.constant.factor.cols <- factor.cols %>% dplyr::select(-col.index.to.remove)

  # Generate and return our new dataset (non-categorical + cleaned categorical columns)
  new.data <- bind_cols(non.factor.cols, removed.constant.factor.cols)
  return(new.data)
}
