`predict.mlds` <-
function(object, newdata = NULL,
		 type = "link", ...) {
#object, obj of class mlds
	# don't need type "terms"
	if (object$method == "glm") {
		if (is.null(newdata)) {
			ans <- predict(object$obj, type = type, ...) } else
			{
			 ans <- predict(object$obj, newdata = newdata,
			 	 type = type, ...)
			}
	} else
	{
	 fam <- binomial(link = object$link)
	 psc <- object$pscale
	 s <- object$sigma
	 if (is.null(newdata)) d <- object$data else
	 					  d <- newdata
	 del <- matrix(psc[unlist(subset(d, select = -resp))], 
		ncol = 4) %*% c(1, -1, -1, 1)
	 z <- del/s
	 if (type == "link") ans <- z else
	 	ans <- fam$linkinv(z)
			}
	 as.vector(ans)
	}

