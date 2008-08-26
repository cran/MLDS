`mlds` <- function(x, ...) 
	UseMethod("mlds")


`mlds.default` <- 
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
		{stimulus = seq(max(data))}
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
	
diff.sc <- function(n, s, d) {
 ### n <- initial est. for perceptual scale
 ### s <- initial est. of sd
 ### d <- data.frame

 	x <- qlogis(n) #values 2 to len - 1
 	x[length(x) + 1] <- log(s)
	 r.opt <- optim(x, diff.err, d = d, hessian = TRUE,
 				method = "BFGS",
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
	sx <- if (missing(stimulus)) {
		if (inherits(d, "mlds.df")) {
			attr(d, "stimulus")
			} else
		{seq(max(d))}
		} else stimulus	
	
	diff.err <- function(parm, ff, sx) {
	#compute likelihood w/ f(p)	, p includes s as last param
		s <- parm[length(parm)]
		px <- parm[-length(parm)]
		del <- matrix(ff(px, sx[unlist(d[, -d$resp])]), 
			ncol = 4) %*% c(1, -1, -1, 1)
		z <- del/s			
		fam <- binomial(link = lnk)
		p <- fam$linkinv(z)
		p[p < .Machine$double.eps] <- .Machine$double.eps
		p[p > (1 - .Machine$double.eps)] <- 1 -.Machine$double.eps

		-sum(log(p[d$resp == 1]), na.rm = TRUE) -
			sum(log(1 - (p[d$resp == 0])), na.rm = TRUE)	}
	f <- Form2fun(form, expression(p))
	res <- optim(p, diff.err, hessian = TRUE, method = "BFGS",
	 		control = control, ff = f, sx = sx)
	pscale <- f(res$par[-length(res$par)], sx)	
	pscale <- (pscale - pscale[1])/(pscale[length(pscale)] - pscale[1])
	psc <- list(pscale = pscale, stimulus = sx, 
		sigma = res$par[length(res$par)],
		par = res$par[-length(res$par)],
		logLik = -res$value, hess = res$hessian,
		method = "formula", link = lnk, data = d,
		conv = res$convergence,	formula = form,
		func = f)
	class(psc) <- "mlds"
	psc
}
