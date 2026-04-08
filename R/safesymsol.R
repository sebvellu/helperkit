#' @export

safesymsol <- function(mata, matb = NULL) {
	if (is.null(matb)) {
		return(safesyminv(mata))
	} else {
		tsol <- try(chol(mata), TRUE)
		if (inherits(tsol, "try-error")) {
			#return(safesolve(mata, matb))
			return(safesoleq(mata, matb))
		} else {
			return(backsolve(tsol, forwardsolve(t(tsol), matb)))
		}
	}
}
