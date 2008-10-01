df2mlds.df <- function(d, st) {
	if (missing(st)) st <- sort(unique(unlist(d[, -1])))
	names(d) <- c("resp", paste("S", 1:4, sep = ""))
	attr(d, "stimulus") <- st
	invord <- d$S1 > d$S4
	attr(d, "invord") <- invord
	class(d) <- c("mlds.df", "data.frame")
	d
}