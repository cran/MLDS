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

`lines.mlds`<-
function(x, standard.scale = FALSE, ... ) {
#x, object of class mlds
	if (standard.scale) {
		ll <- length(x$pscale)
		lines(x$stimulus, x$pscale/x$pscale[ll], ...)
	} else
	lines(x$stimulus, x$pscale, ...)
	}

`points.mlds`<-
function(x, standard.scale = FALSE, ... ) {
#x, object of class mlds
	if (standard.scale) {
		ll <- length(x$pscale)
		points(x$stimulus, x$pscale/x$pscale[ll], ...)
	} else
	points(x$stimulus, x$pscale, ...)
	}

`plot.mlbs` <- function(x, standard.scale = FALSE, ...){
	 par(ask = FALSE)
    if (standard.scale) {
        ll <- length(x$pscale)
        plot(x$stimulus, x$pscale/x$pscale[ll], ...)
    }
    else plot(x$stimulus, x$pscale, ...)
}

`lines.mlbs` <- function (x, standard.scale = FALSE, ...) 
{
    if (standard.scale) {
        ll <- length(x$pscale)
        lines(x$stimulus, x$pscale/x$pscale[ll], ...)
    }
    else lines(x$stimulus, x$pscale, ...)
}

`points.mlbs` <- function (x, standard.scale = FALSE, ...) 
{
    if (standard.scale) {
        ll <- length(x$pscale)
        points(x$stimulus, x$pscale/x$pscale[ll], ...)
    }
    else points(x$stimulus, x$pscale, ...)
}
