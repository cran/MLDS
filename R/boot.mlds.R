`boot.mlds` <- function(x, nsim,  ...) {
	d <- if (x$method == "glm") 
				as.matrix(x$obj$data[, -1]) else
				as.matrix(make.ix.mat(x$data)[, -1])
#				as.mlds.df(x$obj$data) else
#				x$data
	p <- fitted(x)
	rsim <- matrix(rbinom(length(p) * nsim, 1, p), 
					nrow = length(p), ncol = nsim)
	bts.samp <- apply(rsim, 2, function(y, dd) {
#		dd$resp <- x
#		psct <- mlds(dd, ...)$pscale
		
		psct <- glm.fit(dd, y, family = binomial(x$link), ...)$coefficients
		names(psct) <- x$stimulus[-1]
		c(psct, sigma = 1)/psct[length(psct)]
		},
		 dd = d)
	list(boot.samp = bts.samp,
		 bt.mean = apply(bts.samp, 1, mean),
		 bt.sd = apply(bts.samp, 1, sd),
		 N = nsim
		 )
	}
	
`boot.mlbs` <- function (x, nsim, ...) 
{
    d <- as.matrix(x$obj$data[, -1])
    p <- fitted(x)
    rsim <- matrix(rbinom(length(p) * nsim, 1, p), nrow = length(p), 
        ncol = nsim)
    bts.samp <- apply(rsim, 2, function(y, dd) {
        psct <- glm.fit(dd, y, family = binomial(x$link), ...)$coefficients
        names(psct) <- x$stimulus[-1]
        c(psct, sigma = 1)/psct[length(psct)]
    }, dd = d)
    list(boot.samp = bts.samp, bt.mean = apply(bts.samp, 1, mean), 
        bt.sd = apply(bts.samp, 1, sd), N = nsim)
}
