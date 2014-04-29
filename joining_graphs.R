data<-read.csv(file.choose())
head(data)
list(unique(data$trt))
list(unique(data$rho_cut))
library(igraph)
#starting with soils#
grays<-gray.colors(3,start=0,end=.5)
#starting with high rho graphs
graph1<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="soil")[,1:2]),directed=FALSE))
graph2<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="forest soil")[,1:2]),directed=FALSE))

graph_int_high<-simplify(graph.intersection.by.name(graph1,graph2,keep.x.vertices=FALSE))
graph_int_high<-delete.vertices(graph_int_high,which(degree(graph_int_high) < 1))

E(graph_int_high)$color<-grays[1]
E(graph_int_high)$width<-2.5
plot(graph_int_high)
#second level rho

graph1<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="soil")[,1:2]),directed=FALSE))
graph2<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="forest soil")[,1:2]),directed=FALSE))

graph_int_med<-simplify(graph.intersection.by.name(graph1,graph2,keep.x.vertices=FALSE))
graph_int_med<-delete.vertices(graph_int_med,which(degree(graph_int_med) < 1))

E(graph_int_med)$color<-grays[3]
E(graph_int_med)$width<-2.5
plot(graph_int_med)

#union the two graphs
soils_graph<-graph.union.by.name(graph_int_high,graph_int_med)
highs<-(match(paste(get.edgelist(graph_int_high)[,1],get.edgelist(graph_int_high)[,2]),paste(get.edgelist(soils_graph)[,1],get.edgelist(soils_graph)[,2])))

E(soils_graph)$width<-2.5
E(soils_graph)$color<-"gray"
E(soils_graph)$color
for(i in 1:length(highs)){ E(soils_graph)$color[highs[i]]<-"black" }
plot(soils_graph)
E(soils_graph)$color[highs[2]]


#next with apple#
grays<-gray.colors(3,start=0,end=.5)
#starting with high rho graphs
list(unique(data$trt))
graph1<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="Unsprayed")[,1:2]),directed=FALSE))
graph2<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="Sprayed_w_streptomycin")[,1:2]),directed=FALSE))

graph_int_high<-simplify(graph.intersection.by.name(graph1,graph2,keep.x.vertices=FALSE))
graph_int_high<-delete.vertices(graph_int_high,which(degree(graph_int_high) < 1))

E(graph_int_high)$color<-grays[1]
E(graph_int_high)$width<-2.5
plot(graph_int_high)
#second level rho

graph1<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="Unsprayed")[,1:2]),directed=FALSE))
graph2<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="Sprayed_w_streptomycin")[,1:2]),directed=FALSE))

graph_int_med<-simplify(graph.intersection.by.name(graph1,graph2,keep.x.vertices=FALSE))
graph_int_med<-delete.vertices(graph_int_med,which(degree(graph_int_med) < 1))

E(graph_int_med)$color<-grays[3]
E(graph_int_med)$width<-2.5
plot(graph_int_med)

#union the two graphs
apple_graph<-graph.union.by.name(graph_int_high,graph_int_med)
highs<-(match(paste(get.edgelist(graph_int_high)[,1],get.edgelist(graph_int_high)[,2]),paste(get.edgelist(apple_graph)[,1],get.edgelist(apple_graph)[,2])))

E(apple_graph)$width<-2.5
E(apple_graph)$color<-"gray"
E(apple_graph)$color
for(i in 1:length(highs)){ E(apple_graph)$color[highs[i]]<-"black" }
plot(apple_graph)
E(apple_graph)$color


soils_apple_graph<-simplify(graph.intersection.by.name(soils_graph,apple_graph,keep.x.vertices=FALSE))
soils_apple_graph
plot(soils_apple_graph)


#now doing the body graphs#
#starting with high rho graphs


graph1<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="M1")[,1:2]),directed=FALSE))
graph2<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="M2")[,1:2]),directed=FALSE))
graph3<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="M3")[,1:2]),directed=FALSE))
graph4<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="M4")[,1:2]),directed=FALSE))
graph5<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="M5")[,1:2]),directed=FALSE))
graph6<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="M6")[,1:2]),directed=FALSE))
graph7<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="F1")[,1:2]),directed=FALSE))
graph8<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="F2")[,1:2]),directed=FALSE))
graph9<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.75 & trt=="F3")[,1:2]),directed=FALSE))

graph_int_high<-simplify(graph.intersection.by.name(graph1,graph2,keep.x.vertices=FALSE))
summary(graph_int_high)
graph_int_high<-simplify(graph.intersection.by.name(graph_int_high,graph3,keep.x.vertices=FALSE))
summary(graph_int_high)
graph_int_high<-simplify(graph.intersection.by.name(graph_int_high,graph4,keep.x.vertices=FALSE))
summary(graph_int_high)
graph_int_high<-simplify(graph.intersection.by.name(graph_int_high,graph5,keep.x.vertices=FALSE))
summary(graph_int_high)
graph_int_high<-simplify(graph.intersection.by.name(graph_int_high,graph6,keep.x.vertices=FALSE))
summary(graph_int_high)
graph_int_high<-simplify(graph.intersection.by.name(graph_int_high,graph7,keep.x.vertices=FALSE))
summary(graph_int_high)
graph_int_high<-simplify(graph.intersection.by.name(graph_int_high,graph8,keep.x.vertices=FALSE))
summary(graph_int_high)
graph_int_high<-simplify(graph.intersection.by.name(graph_int_high,graph9,keep.x.vertices=FALSE))
summary(graph_int_high)

#doing medium#
graph1<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="M1")[,1:2]),directed=FALSE))
graph2<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="M2")[,1:2]),directed=FALSE))
graph3<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="M3")[,1:2]),directed=FALSE))
graph4<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="M4")[,1:2]),directed=FALSE))
graph5<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="M5")[,1:2]),directed=FALSE))
graph6<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="M6")[,1:2]),directed=FALSE))
graph7<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="F1")[,1:2]),directed=FALSE))
graph8<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="F2")[,1:2]),directed=FALSE))
graph9<-simplify(graph.edgelist(as.matrix(subset(data, rho_cut==0.5 & trt=="F3")[,1:2]),directed=FALSE))

graph_int_med<-simplify(graph.intersection.by.name(graph1,graph2,keep.x.vertices=FALSE))
summary(graph_int_med)
graph_int_med<-simplify(graph.intersection.by.name(graph_int_med,graph3,keep.x.vertices=FALSE))
summary(graph_int_med)
graph_int_med<-simplify(graph.intersection.by.name(graph_int_med,graph4,keep.x.vertices=FALSE))
summary(graph_int_med)
graph_int_med<-simplify(graph.intersection.by.name(graph_int_med,graph5,keep.x.vertices=FALSE))
summary(graph_int_med)
graph_int_med<-simplify(graph.intersection.by.name(graph_int_med,graph6,keep.x.vertices=FALSE))
summary(graph_int_med)
graph_int_med<-simplify(graph.intersection.by.name(graph_int_med,graph7,keep.x.vertices=FALSE))
summary(graph_int_med)
graph_int_med<-simplify(graph.intersection.by.name(graph_int_med,graph8,keep.x.vertices=FALSE))
summary(graph_int_med)
graph_int_med<-simplify(graph.intersection.by.name(graph_int_med,graph9,keep.x.vertices=FALSE))
summary(graph_int_med)
plot(graph_int_med)
