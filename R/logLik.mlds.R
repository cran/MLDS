`logLik.mlds` <-
function(object, ...) {
#object, obj of class mlds
	if (object$method == "glm")
		val <- logLik(object$obj) else
		{ val <- object$logLik	
		attr(val, "df") <- length(object$pscale) - 1
		class(val) <- "logLik"
		}
    val
    }

