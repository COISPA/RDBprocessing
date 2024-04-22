#' Function to create a key from the fields of a table
#' @param tab table
#' @param colIndex indices of columns to consider in the ID
#' @param sep separator
#' @return key
#' @export
#' @examples fpKey(data_ex, colnames(data_ex)[1:5])

fpKey <- function(tab, colIndex, sep = ":-:") {
    key <- tab[, colIndex]
    key <- apply(key, 1, paste, collapse = sep)
    key <- gsub("[[:space:]]", "", key)
    return(key)
}
