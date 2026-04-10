#' @export

bdiag <- function(mata, matb) {
	rslt <- cbind(mata, matrix(0, nrow(mata), ncol(matb)))
	rslt <- rbind(rslt, cbind(matrix(0, nrow(matb), ncol(mata)), matb))
	return(rslt)
}
