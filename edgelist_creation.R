head(results)
co_occur_pairs<-function(dataset){
	#dataset<-results
	
	#head(dataset)
	final.results<-data.frame()
	rhos<-c(-.75,-.5,-.25,.25,.5,.75)
	trts<-as.vector(unique(dataset$trt))
	#trts
for(t in 1:length(trts)){
	#t<-1
	dataset_trt<-subset(dataset, trt==trts[t])

	head(dataset_trt)
	summary(dataset_trt)
for(r in 1:6){
	#r<-5
	if(rhos[r] < 0){temp<-subset(dataset_trt, rho <= rhos[r])}
	if(rhos[r] > 0){temp<-subset(dataset_trt, rho >= rhos[r])}
	if(dim(temp)[1]>1){
	#head(temp)
	temp.graph<-simplify(graph.edgelist(as.matrix(temp[,c(2,3)]),directed=FALSE))
	edge_list<-data.frame(get.edgelist(temp.graph,names=TRUE))
	#head(edge_list)
	edge_list$pairs<-paste(edge_list$X1,edge_list$X2)
	#head(edge_list)
	edge_list$rho_cut<-rhos[r]
	edge_list$trt<-trts[t]
	final.results<-rbind(final.results,edge_list)	}
}
	print(t/length(trts))
}
	return(final.results)
}

thing<-co_occur_pairs(results)
head(thing)