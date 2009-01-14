df2mlds.df <- function(d, st) {
	.Deprecated("as.mlds.df", "MLDS")
	if (missing(st)) st <- sort(unique(unlist(d[, -1])))
	names(d) <- c("resp", paste("S", 1:4, sep = ""))
	attr(d, "stimulus") <- st
	invord <- d$S1 > d$S4
	attr(d, "invord") <- invord
	class(d) <- c("mlds.df", "data.frame")
	d
}

`as.mlds.df` <- function(d, ...)
	UseMethod("as.mlds.df")
	
`as.mlds.df.default` <- function(d, ...)
    NextMethod("as.mlds.df", d, ...)

`as.mlds.df.data.frame` <- function(d, st, ...) {
	if (ncol(d) > 5) {
		stim1 <- ifelse(rowSums(d[, -1]) == 0, 0, 1)
		ix.mat <- cbind(stim.1 = stim1, d[, -1])
		dd <- t(apply(ix.mat, 1, function(x) which(x != 0)))
		dd <- as.data.frame(cbind(d[, 1], dd))
		names(dd) <- c("resp", paste("S", 1:4, sep = ""))
		d <- dd
		if (missing(st)) st <- sort(unique(unlist(d[, -1])))
		attr(d, "stimulus") <- st
		d
		} else {
	if (missing(st)) st <- sort(unique(unlist(d[, -1])))
	names(d) <- c("resp", paste("S", 1:4, sep = ""))
	attr(d, "stimulus") <- st
	invord <- d$S1 > d$S4
	attr(d, "invord") <- invord
	class(d) <- c("mlds.df", "data.frame")
	d
	}
}

	