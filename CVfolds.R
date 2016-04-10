CVfolds <- function(data,k) {
	if (k > nrow(data)) {
		cat('No. of folds can not be greater than the rows in data set...\n')
		return(NULL)
	}
	data <- as.data.frame(data)
	cutLength <- round(nrow(data) / k)
	CVfolds <- NULL
	for (i in 1:k) {
		sampleRows <- sample(nrow(data),min(cutLength,nrow(data)))
		CVfolds[[i]] <- data[sampleRows,]
		data <- data[-sampleRows,]
	}
	if (nrow(data) != 0  ) {
		for (i in 1:nrow(data)) {
			sampleFold <- sample(1:k,1)
			CVfolds[[sampleFold]] <- rbind(CVfolds[[sampleFold]],data[1,])
			data <- data[-1,]
		}		
	}
	return(CVfolds)
}
