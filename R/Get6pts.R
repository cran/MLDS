`Get6pts` <-
function(x, ...) {
# x object of class mlds
	n <- length(x$pscale)
	ii <- expand.grid(seq(dim(fs <-  t(combn(seq(n), 3)))[1]),
				seq(dim(ss <-  t(combn(seq(2, n), 3)))[1]))
	fl <- cbind(fs[ii$Var1,], ss[ii$Var2,])
	fc <- (fl[, 1] < fl[, 4])
	fl <- fl[fc, ] # all 6-tuples w/ a1 < a2

	if (x$method == "glm") {
		dd <- as.mlds.df(x$obj$data)
		} else
	{ dd <- x$data }
	
	base <- 10^seq(3, 0)
	b1 <- as.matrix((dd[, -1]) - 1) %*% base #codage of 4-tuples
	A <- match((fl[, c(1, 2, 4, 5)] - 1) %*% base, b1)
	B <- match((fl[, c(2, 3, 5, 6)] - 1) %*% base, b1)
	E <- match((fl[, c(1, 3, 4, 6)] - 1) %*% base, b1)
	cc <- data.frame(A = A, B = B, E = E)
	cc <- cc[complete.cases(cc), ] #keep only if all 3 present
	Six.Pts <- lapply(cc, function(x, y) y[unlist(x), ] , y = dd)
	attr(Six.Pts, "indices") <- cc
    Six.Pts
}

