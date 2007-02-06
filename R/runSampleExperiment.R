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
	stim <- do.call(DefineStimuli, list())
	NumStimuli <- length(stim)
	allTrials <- t(combn(seq(NumStimuli), 4))
	trialOrder <- sample(seq(nrow(allTrials)), replace = FALSE)
	trial <- allTrials[trialOrder, ]
	topbot <- as.logical(rbinom(nrow(trial), 1, 0.5))
	trial[topbot, ] <- trial[topbot, c(3, 4, 1, 2)]
	resp <- rep(NA, nrow(trial))	
	switch(.Platform$OS,
				unix = if (.Platform$GUI == "AQUA")
						quartz(width = DisplaySize, 
							height = DisplaySize) else 
						x11(width = DisplaySize, 
							height = DisplaySize),
				windows = windows(width = DisplaySize, 
					height = DisplaySize))
	NT <- if (is.null(NumTrials)) seq(nrow(trial)) else
			seq(NumTrials)
	for (tr in NT) {
		par(mfrow = c(2, 2))
			do.call(DisplayTrial, 
				list(stim[trial[tr, ]], ...))
		cat("\n", tr,  "\nLower or Upper Pair (1, 2): ", "\n")
		resp[tr] <- scan(n = 1)
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