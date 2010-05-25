`mlds` <- function(x, ...) 
	UseMethod("mlds")


`mlds.mlds.df` <- 
function(x, stimulus = NULL, method = "glm",
			lnk = "probit", opt.meth = "BFGS", 
			opt.init = NULL, control = glm.control(maxit = 50000,
				epsilon = 1e-14), ...
			) {
	data <- x
#data, data.frame from diff scale exp.
#stimulus, physical levels
	if (missing(stimulus)) {
		if (inherits(data, "mlds.df")) {
			stimulus <- attr(data, "stimulus")
			} else
		{stimulus <- seq(max(data))}
		} 
		
	if (method == "glm") {
		# set-up matrix
		dsInc.df <- make.ix.mat(data)
		psc.glm <- glm(resp ~ . - 1, 
			family = binomial(link = lnk), 
			data = dsInc.df, control = control, ...)
		psc.glm$call$family[[2]] <- lnk
		psc.glm$call$control <- control #glm.control(maxit = 50000,
#				epsilon = 1e-14)
		psc.lst <- list(pscale = c(0, coef(psc.glm)), 
			stimulus = stimulus,
			sigma = 1, 
			method = "glm", link = lnk,
			obj = psc.glm
			)
	} else { # if method "optim"
	diff.err <- function(x, d) {
 ### x <- perceptual scale and sd estimates
 ### d <- data.frame
 
		nlen <- length(x) + 1
		n <- vector("numeric", nlen)
		n[1] <- 0 ; n[nlen] <- 1
		n[seq(2, nlen - 1)] <- plogis(x[seq(1, nlen - 2)])
		s <- exp(x[nlen - 1])
	
		del <-  matrix(n[unlist(d[, -d$resp])],
#		matrix(n[unlist(subset(d, select = -resp))], 
				ncol = 4) %*% c(1, -1, -1, 1)
		z <- del/s
		fam <- binomial(link = lnk)
		p <- fam$linkinv(z)
		p[p < .Machine$double.eps] <- .Machine$double.eps
		p[p > (1 - .Machine$double.eps)] <- 1 -.Machine$double.eps

		-sum(log(p[d[, 1] == 1]), na.rm = TRUE) -
			sum(log(1 - (p[d[, 1] == 0])), na.rm = TRUE)
}
	
diff.sc <- function(n, s, d, opt.m = opt.meth) {
 ### n <- initial est. for perceptual scale
 ### s <- initial est. of sd
 ### d <- data.frame

 	x <- qlogis(n) #values 2 to len - 1
 	x[length(x) + 1] <- log(s)
	 r.opt <- optim(x, diff.err, d = d, hessian = TRUE,
 				method = opt.m,
				control = list(maxit = 50000, abstol = 1e-14), ...)
 	list(pscale = c(0, plogis(r.opt$par[-length(r.opt$par)]), 1),
 		stimulus = stimulus,
 		sigma = exp(r.opt$par[length(r.opt$par)]),
		logLik = -r.opt$value, hess = r.opt$hessian,
		method = "optim", link = lnk,
		data = d,
		conv = r.opt$convergence
		)
}
	xi <- max(data)
	psc.opt <- diff.sc(n = opt.init[2:(xi-1)], 
					   s = opt.init[length(opt.init)], d = data)
	psc.lst <- psc.opt
	}
class(psc.lst) <- "mlds"
psc.lst			
} 			

`mlds.formula` <- 
function(x, p, data, stimulus = NULL, 
			lnk = "probit", opt.meth = "BFGS", 
			control = list(maxit = 50000,
				reltol = 1e-14), ...
			) {
	form <- x
	Form2fun <-   function(f, p = quote(p)) {
		xx <- all.vars(f)
		fp  <- match(p, xx)
		xx <- c(xx[fp], xx[-fp])
		ff <- vector("list", length(xx))
		names(ff) <- xx
		ff[[length(ff) + 1]] <- f[[2]]
		as.function(ff, parent.frame())
	}
	d <- data
	if (length(d) == 4)
	 	d[, 1] <- ifelse(d[, 2] > d[, 3], 1 - d[, 1], d[, 1])
	wts <- if (length(d) == 4) c(1, -2, 1) else c(1, -1, -1, 1)
	nc <- if (length(d) == 4) 3 else 4
	sx <- if (missing(stimulus)) {
		if (inherits(d, c("mlds.df", "mlbs.df"))) {
			attr(d, "stimulus")
			} else
		{seq(max(d))}
		} else stimulus
      	
	diff.err <- function(parm, ff, sx, d) {
	#compute likelihood w/ f(p)	, p includes s as last param
		s <- parm[length(parm)]
		px <- parm[-length(parm)]
		del <- matrix(ff(px, sx[unlist(d[, -d$resp])]), 
			ncol = nc) %*% wts
		z <- del/s			
		fam <- binomial(link = lnk)
		p <- fam$linkinv(z)
		p[p < .Machine$double.eps] <- .Machine$double.eps
		p[p > (1 - .Machine$double.eps)] <- 1 -.Machine$double.eps

		-sum(log(p[d$resp == 1]), na.rm = TRUE) -
			sum(log(1 - (p[d$resp == 0])), na.rm = TRUE)	}
	f <- Form2fun(form, expression(p))
	res <- optim(p, diff.err, ff = f, sx = sx, d = d,
			hessian = TRUE, method = opt.meth,
	 		control = control)
	pscale <- f(res$par[-length(res$par)], sx)	
	pscale <- (pscale - pscale[1])/(pscale[length(pscale)] - pscale[1])
	psc <- list(pscale = pscale, stimulus = sx, 
		sigma = res$par[length(res$par)],
		par = res$par[-length(res$par)],
		logLik = -res$value, hess = res$hessian,
		method = "formula", link = lnk, data = d,
		conv = res$convergence,	formula = form,
		func = f)
	class(psc) <- if (length(data) == 5 ) "mlds" else "mlbs"
	psc
}

`mlds.mlbs.df` <- function(x, stimulus = NULL, method = "glm",
			lnk = "probit",
			control = glm.control(maxit = 50000, 
        	epsilon = 1e-14), ...) {
    if (method != "glm") 
    	stop("Only glm method currently defined for this class!\n")   
    if (missing(stimulus)) {
        if (inherits(x, "mlbs.df")) {
            stimulus <- attr(x, "stimulus")
        }
        else {
            stimulus <- seq(max(x))
        }
    }
   d <- x
   N <- max(d[, -1], na.rm = TRUE)
	bix.mat <- matrix(0, nrow = nrow(d), ncol = N)
	for (ix in seq_len(nrow(d))) {
		iy <- unlist(d[ix, -1])
		bix.mat[ix, iy] <- c(1, -2, 1)
	}
	d[, 1] <- ifelse(d[, 2] > d[, 3], 1 - d[, 1], d[, 1])
	d.bis <- data.frame(resp = d[, 1], S = bix.mat)
	d.bis$S.1 <- NULL
	out.bis <- glm(factor(resp) ~ . - 1, 
		family = binomial(link = lnk), data = d.bis,
		control = control, ...)
	out.lst <- list(pscale = c(0, coef(out.bis)), 
		stimulus = stimulus, sigma = 1, method = "glm",
		link = lnk, obj = out.bis)
    class(out.lst) <- "mlbs"
    out.lst
}


`mlds.data.frame` <- function(x, ...) {
	x <- if (length(x) == 5) 
		as.mlds.df(x) else
		as.mlbs.df(x)	
	mlds(x, ...)
}