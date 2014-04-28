setwd("/Users/metagenomics/Desktop/Ryan_co-occur_code/revision_work/")
library(ggplot2)
library(igraph)


results<-read.csv("total_order_results_4-28.csv")

list(unique(results$trt))
summary(results)
soils_results<-subset(results, trt=="soil")# |trt=="tropical soil" | trt=="forest soil" | trt=="shrubland" | trt=="grassland soil")
apple_results<-subset(results, trt=="Unsprayed" |trt=="Sprayed_w_streptomycin" )
body_results<-subset(results, trt=="M1"| trt=="M2"| trt=="M3" | trt=="M4" | trt=="M5" | trt=="M6" | trt=="F1" | trt=="F2" | trt=="F3")

trts
network_stats<-function(dataset){
	#dataset<-results
	
	#head(dataset)
	final.results<-data.frame()
	rhos<-c(-.75,-.5,-.25,.25,.5,.75)
	trts<-as.vector(unique(dataset$trt))
	#trts
for(t in 1:length(trts)){
	#t<-2
	dataset_trt<-subset(dataset, trt==trts[t])

	head(dataset_trt)
	summary(dataset_trt)
for(r in 1:6){
	#r<-1
	if(rhos[r] < 0){temp<-subset(dataset_trt, rho <= rhos[r])}
	if(rhos[r] > 0){temp<-subset(dataset_trt, rho >= rhos[r])}
	if(dim(temp)[1]>1){
	#head(temp)
	temp.graph<-simplify(graph.edgelist(as.matrix(temp[,c(2,3)]),directed=FALSE))
	#plot(temp.graph)
	#?degree
	stats<-data.frame(row.names((as.matrix(degree(temp.graph,normalized=TRUE)))),(as.matrix(degree(temp.graph,normalized=TRUE))),(as.matrix(betweenness(temp.graph))))
	names(stats)<-c("order","norm_degree","betweenness")
	#row.names(stats)<-NULL
	#stats
	stats$rho_cut<-rhos[r]
	stats$trt<-trts[t]
	final.results<-rbind(final.results,stats)	}
}
	print(t/length(trts))
}
	return(final.results)
}

network_results<-network_stats(results)

unique(network_results$trt)
network_results$ln_norm_degree<-log(network_results$norm_degree)
network_results$ln_betweenness<-log1p(network_results$betweenness)
ggplot(network_results)+geom_point(aes(x=ln_norm_degree,y=ln_betweenness,colour=as.factor(rho_cut)))+facet_wrap(~trt,scales="free")

head(network_results)