`AIC.mlds` <-
function(object, ..., k = 2) {
#object, obj of class mlds
	AIC(logLik(object), ..., k = k)
	}

