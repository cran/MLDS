`plot.mlds` <-
function(x, standard.scale = FALSE,... ) {
#x, object of class mlds
	par(ask = FALSE)
	if (standard.scale) {
		ll <- length(x$pscale)
		plot(x$stimulus, x$pscale/x$pscale[ll], ...)
		} else
	plot(x$stimulus, x$pscale, ... )
	}

