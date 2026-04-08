#' @export

safesyminv <- function(matr) {
	tsol <- try(chol2inv(chol(matr)), TRUE)
	if (inherits(tsol, "try-error")) {
		#return(safesolve(matr))
		return(safesolid(matr))
	} else {
		return(tsol)
	}
}
