
library(ggplot2)
library(igraph)
library(reshape)

results<-read.csv("total_order_results_4-29_thinned.csv")
results<-subset(results, trt != "tropical soil")

network_stats<-function(dataset){
	
	final.results<-data.frame()
	rhos<-c(-.75,-.5,.5,.75)
	trts<-as.vector(unique(dataset$trt))
	
for(t in 1:length(trts)){
	
	dataset_trt<-subset(dataset, trt==trts[t])

	head(dataset_trt)
	summary(dataset_trt)
for(r in 1:length(rhos)){
	
	if(rhos[r] < 0){temp<-subset(dataset_trt, rho <= rhos[r])}
	if(rhos[r] > 0){temp<-subset(dataset_trt, rho >= rhos[r])}
	if(dim(temp)[1]>1){
	
	temp.graph<-simplify(graph.edgelist(as.matrix(temp[,c(2,3)]),directed=FALSE))

	stats<-data.frame(row.names((as.matrix(degree(temp.graph,normalized=TRUE)))),(as.matrix(degree(temp.graph,normalized=TRUE))),(as.matrix(betweenness(temp.graph))))
	names(stats)<-c("order","norm_degree","betweenness")
	
	stats$rho_cut<-rhos[r]
	stats$trt<-trts[t]
	stats$clustering_coeff<-transitivity(temp.graph,type="global")
	stats$clustering_coeff_rand<-transitivity(erdos.renyi.game(length(V(temp.graph)),length(E(temp.graph)),type="gnm"))
	stats$cluster_ratio<-stats$clustering_coeff/stats$clustering_coeff_rand

#remove these if using data not from this study
	stats$cluster_ratio<-stats$clustering_coeff/stats$clustering_coeff_rand
if(trts[t]=="M1" | trts[t]=="M2" | trts[t]=="M3" | trts[t]=="M4" | trts[t]=="M5" | trts[t]=="M6" ){stats$ecosystem<-"Male"}
	if(trts[t]=="F1" | trts[t]=="F2" | trts[t]=="F3"  ){stats$ecosystem<-"Female"}
	if(trts[t]=="soil" | trts[t]=="forest soil"  ){stats$ecosystem<-"Soil"}
	if(trts[t]=="Unsprayed" | trts[t]=="Sprayed_w_streptomycin"  ){stats$ecosystem<-"Apple"}

	final.results<-rbind(final.results,stats)	}
}
	print(t/length(trts))
}
	return(final.results)
}

network_results<-data.frame(network_stats(results))
