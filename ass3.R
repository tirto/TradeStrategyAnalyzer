
toBand <- function(X, KD) {
	# test that X is symmetric
	N <- nrow(X)
	stopifnot(all.equal(nrow(X), ncol(X)))
	# sanity check on KD
	if(KD >= N) {
		print("ERROR: KD greater than N")
	}
	resultN <- KD + 1
	result <- matrix(nrow=resultN, ncol=N)
	# do some index calculations
	for (i in 1:resultN) {
		endCol <- N-i+1
		if(i == N) {
			v <- c(X[i:N, 1:endCol], rep(NA, i-1))
		} else {
			v <- c(diag(X[i:N, 1:endCol]), rep(NA, i-1))
		}
		result[i,] <- v
	}

	# return
	result
}

replaceBand <- function(X, band) {
	# test that X is symmetric
	N <- nrow(X)
	stopifnot(all.equal(nrow(X), ncol(X)))
	# test that band matrix has same number of columns
	stopifnot(all.equal(ncol(X), ncol(band)))
	KD <- nrow(band) - 1
	result <- X
	D <- N
	for(b in 1:(KD+1)) {
		for(d in 1:D) {
			result[d+(b-1),d] <- band[b,d]
			result[d,d+(b-1)] <- band[b,d]
		}
		D <- D -1
	}
	# return
	result
}

#========= TEST CODE ============
load("A3Data.rda")
test3_mycode <- toBand(test1,3)
test4_mycode <- replaceBand(test1, toBand(test2,3))
stopifnot(all.equal(test3_mycode, test3))
stopifnot(all.equal(test4_mycode, test4))

#========== PART B ==============
setClass("Band", contains="structure", representation(b="matrix"))
setMethod("initialize", "Band", function(.Object, X, KD) {
			print("***INITIALIZING CLASS BAND***");
			.Object@b <- toBand(X, KD);
			.Object})
testBandInstance <- new("Band", X=test1, KD=3)
stopifnot(all.equal(testBandInstance@b, test3))

#========== PART C ==============
