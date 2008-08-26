`boot.mlds` <- function(x, nsim,  ...) {
	d <- if (x$method == "glm") 
				ix.mat2df(x$obj$data) else
				x$data
	p <- fitted(x)
	rsim <- matrix(rbinom(length(p) * nsim, 1, p), 
					nrow = length(p), ncol = nsim)
	bts.samp <- apply(rsim, 2, function(x, dd) {
		dd$resp <- x
		psct <- mlds(dd, ...)$pscale
		psct/psct[length(psct)]
		},
		 dd = d)
	list(boot.samp = bts.samp,
		 bt.mean = apply(bts.samp, 1, mean),
		 bt.sd = apply(bts.samp, 1, sd),
		 N = nsim
		 )
	}