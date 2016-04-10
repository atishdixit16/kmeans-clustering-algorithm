source('CVfolds.R')
kmeansAtish <- function(data,k) {
	epsilon <- 0.01
	clusters <- CVfolds(data,k)
	min <- 1000000
	for (i in 1:3000) 
	{	
	cat('.')	
	#finding CGs
	centers <- NULL
	for (i in 1:k) {
		centers <- rbind(centers,colMeans(clusters[[i]]))
	}
	#finding replace indices and performance measure
	newCluster <- NULL
	centerDistance <- 0
	for (i in 1:k) {
		newClust <- NULL
		centDist <- 0
		for (j in 1:nrow(clusters[[i]])) {	
			distance <- as.matrix( dist(rbind(clusters[[i]][j,],centers)) )
			#print(distance)
			distance <- distance[-1,1]
			centDist <- distance[i] + centDist
			newClust <- c ( newClust  , which(distance==min(distance)))
		}
		newCluster[[i]] <- newClust
		newCluster[[i]] <- as.vector( newCluster[[i]] )
		centerDistance <- centerDistance + centDist
	}
	# exiting cindition
	if ( centerDistance < min) {
		min <- centerDistance
		minClusters <- clusters
	}
	# actually replacing the instances
	dataStack <- NULL
	for (i in 1:k) 
		dataStack <- rbind(dataStack,clusters[[i]])
	newClust <- NULL
	for (i in 1:k) 
		newClust <- c(newClust,newCluster[[i]])
	dataStack <- cbind(dataStack,newClust)
	clusters <- list(NULL,NULL,NULL)
	for (i in 1:nrow(dataStack))
		clusters[[ dataStack[i,ncol(dataStack)] ]] <- rbind(clusters[[ dataStack[i,ncol(dataStack)] ]],dataStack[i,-ncol(dataStack)])
	}
	return(minClusters)
}
