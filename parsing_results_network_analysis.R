setwd("/Users/metagenomics/Desktop/Ryan_co-occur_code/revision_work/")
library(ggplot2)
library(igraph)


results<-read.csv("order_results.csv")
results<-results[,-1]
list(unique(results$trt))
soils_results<-subset(results, trt=="soil")# |trt=="tropical soil" | trt=="forest soil" | trt=="shrubland" | trt=="grassland soil")
apple_results<-subset(results, trt=="Unsprayed" |trt=="Sprayed_w_streptomycin" )
body_results<-subset(results, trt=="M1"| trt=="M2"| trt=="M3" | trt=="M4" | trt=="M5" | trt=="M6" | trt=="F1" | trt=="F2" | trt=="F3")


network_stats<-function(dataset){
	final.results<-data.frame()
	rhos<-c(-.75,-.5,-.25,.25,.5,.75)
	trts<-as.vector(unique(dataset$trt))
for(t in 1:length(trts)){
	dataset_trt<-subset(dataset, trt==trts[t])
for(r in 1:6){
	#r<-4
	if(rhos[r] < 0){temp<-subset(dataset_trt, rho <= rhos[r])}
	if(rhos[r] > 0){temp<-subset(dataset_trt, rho >= rhos[r])}
	#temp
	#head(temp)
	temp.graph<-simplify(graph.edgelist(as.matrix(temp[,c(2,3)]),directed=FALSE))
	#plot(temp.graph)
	#?degree
	stats<-data.frame(row.names((as.matrix(degree(temp.graph,normalized=TRUE)))),(as.matrix(degree(temp.graph,normalized=TRUE))),(as.matrix(betweenness(temp.graph))))
	names(stats)<-c("order","norm_degree","betweenness")
	row.names(stats)<-NULL
	stats$rho_cut<-rhos[r]
	stats$trt<-trts[t]
	final.results<-rbind(final.results,stats)	
}
	
}
	return(final.results)
}
network_stats(soils_results)

