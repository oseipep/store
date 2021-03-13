#' @importFrom stats aggregate
#' @export
addmed <-
function(data, vble,id) {
	submed <- aggregate(data[,vble], list(id=data[,id]), meadian)
	colnames(submed) <- c(id,"pmed")
	merge(data, submed)
}
