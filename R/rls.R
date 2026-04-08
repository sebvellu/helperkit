#' @export

rls <- function(yvls, xvls, rmat, rvec, wght) {
	return(rfmls(yvls, yvls, yvls, xvls, rmat, rvec, wght))
}
