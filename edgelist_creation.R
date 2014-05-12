library(igraph)
library(fdrtool)
library(ggplot2)


co_occur_pairs<-function(dataset){
	#dataset<-data
	
	#head(dataset)
	final.results<-data.frame()
	rhos<-c(-.75,-.5,.5,.75)
	trts<-as.vector(unique(dataset$trt))
	#trts
for(t in 1:length(trts)){
	#t<-1
	dataset_trt<-subset(dataset, trt==trts[t])
	dataset_trt_no0<-subset(dataset_trt, ab1 > 0 & ab2 > 0)
	
	dataset_trt_no0$pairs<-paste(dataset_trt_no0$taxa1,dataset_trt_no0$taxa2)
	
for(r in 1:4){
	#r<-5
	if(rhos[r] < 0){temp<-subset(dataset_trt_no0, rho <= rhos[r])}
	if(rhos[r] > 0){temp<-subset(dataset_trt_no0, rho >= rhos[r])}
	if(dim(temp)[1]>1){
	
	temp.graph<-simplify(graph.edgelist(as.matrix(temp[,c(2,3)]),directed=FALSE))
	edge_list<-data.frame(get.edgelist(temp.graph,names=TRUE))
	
	edge_list$pairs<-paste(edge_list$X1,edge_list$X2)
	edge_list_pvals<-merge(edge_list,dataset_trt_no0,by="pairs",sort=FALSE  )
	
	edge_list_pvals$rho_cut<-rhos[r]
	edge_list_pvals$trt<-trts[t]
	
	edge_list_pvals$qval<-fdrtool(edge_list_pvals$p.value, statistic="pvalue",plot=FALSE,verbose=FALSE)$qval
	as.matrix(names(edge_list_pvals))
	
	final.results<-rbind(final.results,edge_list_pvals[,-c(2:3)])	}
}
	print(t/length(trts))
}
	return(final.results)
}

results<-read.csv(file.choose())
edge_lists<-co_occur_pairs(results)


