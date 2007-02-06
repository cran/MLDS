`make.ix.mat` <-
function(data, xi = NULL, ...) {
# data, basic data.frame from diff scale experiment
# xi, in case some values not included as from 6pt ana.
	if ( is.null(xi) ) xi <- max(data)
	nr <- nrow(data)
	wts <- rep(c(1, -1, -1, 1), each = nr)	
	ix.mat <- matrix(0, ncol = xi, nrow = nr)
	ix.mat[matrix(c(rep(1:nr, 4),
		as.vector(unlist(subset(data, select = -resp)))), ncol = 2)] <- wts
	dsInc.df <- data.frame(resp = data$resp, stim = ix.mat)
	dsInc.df <- dsInc.df[, -2]
	dsInc.df
	}

