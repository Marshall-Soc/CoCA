cca2 <- function (dtf, filter.significance = TRUE, 
		filter.value = 0.01,
		zero.action = c("drop", "ownclass"), 
		verbose = TRUE) {
	# CORRELATIONAL CLASS ANALYSIS (CRCA) 
	# PURPOSE:  Divide observations into schematic classes based on row correlations.
	# PARAMETERS:
	# 	* dtf : the data frame containing the variables of interest
	#   * filter.significance : Significance filtering sets "insignificant" ties to 0 to decrease noise 
	#     and increase stability.  Simulation results show that this greatly increases accuracy in many settings.
	#     Set filter.significance = FALSE to disable this.
	#   * zero.action : what to do with 0-variance rows before partitioning the graph. 
	#       zero.action = "drop"     : drop rows with 0 variance from the analyses (default).
	#       zero.action = "ownclass" : the correlations between 0-var rows and all other rows
	#     	is set to 0, and the correlations between all pairs of 0-var rows are set to 1. 
	#       This effectively creates a "zero class".
	#   * verbose : whether to print details of what CCA is doing to the screen
	#  RETURN VALUES:
	#   * Class memberships are returned in $membership (see help(leading.eigenvector.community) for details).
	#   * The whole correlation matrix is in $cormat (minus the 0-variance rows, if they were dropped).  
	#       It contains a "dtf" attribute which holds the whole dataframe (minus dropped rows)
	#       It also contains a "zeros" attribute which holds the indexes of the dropped rows.
	#   * For convenience, the dataframe is separated into the modules found by the algorithm.  
	#         These individual modules (classes) are in $modules.  
	#         The rows corresponding for each module i are in $modules[[i]]$dtf.
	#         The column correlations for each module i are in $modules[[i]]$cormat.
	#         These modules can be plotted via the S3 method below by simply calling plot(module).

	
	if (verbose) echo <- cat 
	else echo <- c
		
	if (packageVersion("igraph") < 0.7) {
		warning(paste("Your igraph version is ", as.character(packageVersion("igraph")), 
						". CCA produces more accurate results with igraph >= 0.7.\n", sep = ""))
	}
	
	cormat <- .make.cormat(dtf, zero.action)
	if (filter.significance == TRUE) {
			echo("Filtering out correlations for which Pr(|r| != 0) > ", filter.value, "\n")
			cormat <- .filter.insignif (cormat, ncol(dtf), pcutoff = filter.value)
	} else {
			echo("Not filtering significances.\n")
	}
	
    # igraph.arpack.default$maxiter = 10
    # igraph.arpack.default$sym = TRUE
    # igraph.arpack.default$ncv = 1000000

	graph <- .cormat.to.igraph(cormat, absolute.value = TRUE)
	# comm <- igraph::leading.eigenvector.community(graph)
    comm <- igraph::leading.eigenvector.community(graph, options = list(maxiter = 1e+06))
			
	modules <- .separate(attr(cormat, "dtf"), comm$membership) 
	
	val <- list (membership = comm$membership, modules = modules, cormat = cormat)
	class (val) <- "cca"

	echo (paste(capture.output(print(val)), collapse = "\n"), "\n")
	
	return (invisible(val))
}


##### HELPER FUNCTIONS BELOW THIS LINE #########
.make.cormat <- function (dtf, zero.action) {
	# Helper function.  Make a correlation matrix from data frame.  
	if (!all(sapply(dtf, is.numeric))) dtf2 <- data.frame(sapply(dtf, as.numeric))
	else dtf2 <- dtf
	
	# Floating point imprecision may make 0-variance rows appear to have variance slightly higher than 0.
	zeros <- which(apply(dtf2, 1, var) <= 0.000000001)
	
	if (zero.action[1] == "drop" & (length(zeros) > 0)) {
		dtf2 <- dtf2[-zeros,]	
	}
	
	rv <- abs(cor(t(dtf2)))
	
	attributes(rv)$zeros <- zeros
	attributes(rv)$zero.action <- zero.action[1]
	attributes(rv)$dtf <- dtf2
	
	if ((zero.action[1] == "ownclass") & length(zeros) > 0) {
		rv[zeros,] <- 0
		rv[,zeros] <- 0
		rv[zeros,zeros] <- 1
	}
	
  	diag(rv) <- 0
	
	return (rv)
}


.filter.insignif <- function (corr, N.vars, pcutoff = 0.05) {
	# Helper function.
	# Filter signififances at p <= pcutoff (two-tailed).
	corr <- abs(corr)
	
	if (any(diag(corr) != 0))
		stop("Non-zero elements on the diagonal. diag(corr) <- 0 before running this function.")
	
	suppressWarnings(tvalues <- corr * sqrt ((N.vars-2) / (1 - corr^2)))
	if (any(is.infinite(tvalues))) {
		tvalues[is.infinite(tvalues)] <- 9999 # a very big number
	}
	cutoff <- abs(qt(pcutoff / 2, N.vars))
	
	isolates.pre <- sum(apply(corr, 1, sum) == 0)
	corr[tvalues < cutoff] <- 0
	isolates.post <- sum(apply(corr, 1, sum) == 0)
	
	if (isolates.post > isolates.pre) {
		warn1 <- paste ("Significance filtering left", isolates.post - isolates.pre, "rows with no non-zero ties. The CCA result will contain at least one small degenerate class.")
		warning(warn1)
	}
	
	return (corr)
}

.cormat.to.igraph <- function (corr, absolute.value = TRUE) { 
	# Helper function.
	# Make igraph object from correlation matrix.
	if (absolute.value)
		corr <- abs(corr)
	diag(corr) <- 0
	graph <- igraph::graph.adjacency(corr, mode="undirected", weighted = TRUE, diag = FALSE)
	return(graph)
}

.separate <- function (dtf, membership) {
	# separate a data frame and cormat according to the membership vector.
	ids <- sort(unique(membership))
	modules <- list()
	if (is(dtf, "matrix")) {
		rownames(dtf) <- NULL
		dtf <- data.frame(dtf)
	}
	
	for (i in 1:length(ids)) {
		curmod <- list()
		class(curmod) <- "cca.module"
		curmod$dtf <- dtf[membership == ids[i],]
		curmod$cormat <- cor(curmod$dtf)
		curmod$degenerate <- any(is.na(curmod$cormat))
		modules[[i]] <- curmod
	}
	
	return (modules)
}