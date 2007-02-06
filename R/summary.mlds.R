`summary.mlds` <-
function(object, 
			digits = max(3, getOption("digits") - 4), ...) {
#object, obj of class mlds
	z <- object
	cat
	ans <- list()
	ans$pscale <- z$pscale
	names(ans$pscale) <- z$stimulus
	ans$sigma <- z$sigma
	if (z$method == "optim") ans$logLik <- z$logLik else
		ans$logLik <- logLik(z$obj)[1]
	ans$method <- z$method
	ans$link <- z$link
	class(ans) <- "summary.mlds"
	ans
	}

