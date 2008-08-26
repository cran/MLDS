DefineMyScale <- function(rr = c(seq(0, 0.9, len = 10), 0.98)) {
			#sqrt(r)  # for equal-spacing in r^2
			rr
			}
			
DisplayOneTrial <- function(rr, N = 100, ptSize = 1) {
	for (ix in 4:1) {
		covm <- matrix(c(1, rep(rr[ix], 2), 1), 2, 2)
		xy <- MASS:::mvrnorm(n = N, rep(0, 2), covm, 
				empirical = TRUE)
		plot(xy, axes = FALSE, xlab = "", ylab = "", pty = "s",
			cex = ptSize, pch = 16, col = "black",
			xlim = c(-4, 4), ylim = c(-4, 4))
	}
			}

runSampleExperiment <- function(DisplayTrial, DefineStimuli, 
		NumTrials = NULL, DisplaySize = 10, ...) {
	cat("Four stimuli are presented on each trial \n")
	cat("If you perceive a greater difference between  \n")
	cat("  the lower two than the upper two, enter a 1. \n")
	cat("If you perceive a greater difference between \n")
	cat("  the upper pair, enter a 2. \n")
	cat("If you want to quit, enter a 0 \n\n\n")
	stim <- do.call(DefineStimuli, list())
	NumStimuli <- length(stim)
	allTrials <- t(combn(seq(NumStimuli), 4))
	trialOrder <- sample(seq(nrow(allTrials)), replace = FALSE)
	trial <- allTrials[trialOrder, ]
	topbot <- as.logical(rbinom(nrow(trial), 1, 0.5))
	trial[topbot, ] <- trial[topbot, c(3, 4, 1, 2)]
	resp <- rep(NA, nrow(trial))	
	dispdev <- switch(.Platform$OS,
					unix = if (.Platform$GUI == "AQUA")
						"quartz" else
				 		"x11", windows = "windows")
	do.call(dispdev, list(width = DisplaySize, 
						  height = DisplaySize))				 
	NT <- if (missing(NumTrials)) seq(nrow(trial)) else
			seq(NumTrials)
	for (tr in NT) {
		par(mfrow = c(2, 2))
			do.call(DisplayTrial, 
				list(stim[trial[tr, ]], ...))
		cat("\n", tr, "\nEnter 1 (Lower) or 2 (Upper) Pair: ", "\n")
        ii <- 0
       while (!(resp[tr] %in% 0:2)) {
       			if (ii > 0) print ("Enter only 1, 2 or 0 to quit")
        	 	resp[tr] <- scan(n = 1)
        	 	ii <- ii + 1
        	 	resp[tr] }
        if (resp[tr] == 0) break
#		cat("\n", tr,  "\nLower or Upper Pair (1, 2): ", "\n")
#		resp[tr] <- scan(n = 1)
	}
	graphics.off()

#Save output in results as mlds.df
	resp <- resp - 1 
	results <- data.frame(resp = resp, stim = trial)
	results <- as.data.frame(lapply(results, as.integer))
	names(results) <- c("resp", paste("S", 1:4, sep = ""))
	attr(results,  "stimulus") <- stim
	attr(results, "invord") <- topbot
	class(results) <- c("mlds.df", "data.frame")
	results
}	