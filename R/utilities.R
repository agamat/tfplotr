#' Convert a dataframe to a matrix for chord diagrams
#'
#' @param df A data frame with three columns: a row variable, a column variable,
#'   and a value variable.
#' @param column_var The name of the variable to use for columns.
#' @param cell The name of the variable to use for filling the matrix.
#'
#' @return A square matrix.
#'
#' @importFrom tidyr spread
#'
#' @export
convert_df_to_matrix <- function(df, column_var, cell) {
  a <- tidyr::spread_(df, column_var, cell, fill = 0)

  m <- as.matrix(a[, -1])
  rownames(m) <- colnames(m)

  return(m)
}