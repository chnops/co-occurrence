comm_stats<-function(dataset){
	#dataset<-results
	
	#head(dataset)
	final.results<-data.frame()
	rhos<-c(-.75,-.5,-.25,.25,.5,.75)
	trts<-as.vector(unique(dataset$trt))
	#trts
for(t in 1:length(trts)){
	#t<-3
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
	temp_comm<-edge.betweenness.community(temp.graph, directed=FALSE)
	
	member_data<-cbind(row.names(as.matrix(membership(temp_comm))),as.matrix(membership(temp_comm)))
	row.names(member_data)<-NULL

	mod<-rep(modularity(temp_comm),dim(member_data)[1])
	rho_cut<-rep(rhos[r],dim(member_data)[1])
	trt<-rep(trts[t],dim(member_data)[1])
	stats<-cbind(trt,rho_cut,mod,member_data)
	names(stats)<-c("trt","rho_cut","modularity","taxon","module")
	
	
	final.results<-rbind(final.results,stats)	}
}
	print(t/length(trts))
}
	return(final.results)
}
head(results)

thing<-comm_stats(results)