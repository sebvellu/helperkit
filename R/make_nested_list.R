#' Create a Nested List
#'
#' This function creates a nested list with dimensions specified by `lgth`.
#' Each element at the deepest level is initialized as `NULL`.
#'
#' @details
#' The last element of `lgth` determines the length of the deepest list.
#' The remaining elements determine, from inside to outside, how many copies
#' of the current list are created at each level.
#'
#' For example, `make_nested_list(c(2, 3))` creates a list of length 2,
#' where each element is a list of length 3.
#'
#' @param lgth Integer vector giving the length of the list at each level,
#' from the outermost to the innermost level.
#'
#' @return
#' A nested list with one level for each element of `lgth` and with the
#' corresponding lengths specified by `lgth`.
#'
#' @examples
#' make_nested_list(c(2, 3))
#'
#' x <- make_nested_list(c(2, 3, 4))
#' length(x)
#' length(x[[1]])
#' length(x[[1]][[1]])
#'
#' @export
make_nested_list <- function(lgth) {
    rslt <- vector("list", lgth[length(lgth)])

    if (length(lgth) > 1) {
        for (subl in rev(lgth[-length(lgth)])) {
            rslt <- replicate(subl, rslt, simplify = FALSE)
        }
    }

    return(rslt)
}
