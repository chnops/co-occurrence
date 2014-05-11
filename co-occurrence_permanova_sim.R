#Here is the code for figure 1#

#simulating 6 species. Species 1-3 are correlated from ecosystem A and species 4-6 are correlated from ecosystem B (different co-occurrence).  Composition will be different in each


library(bioDist)
library(vegan)
library(mvtnorm)
library(reshape)
library(ggplot2)
library(igraph)
#Here I am making a variance covariance matrix for simulating correlated data
sig<-matrix(cbind(1,.9,.9,
				  .9,1,.9,
				  .9,.9,1),ncol=3)
sig

community<-cbind(
rbind(abs(rmvnorm(100, mean=rep(10,3),sigma=sig)), cbind(abs(rnorm(100,10,10)),abs(rnorm(100,10,10)),abs(rnorm(100,10,10)))),
rbind(cbind(abs(rnorm(100,10,10)),abs(rnorm(100,10,10)),abs(rnorm(100,10,10))),abs(rmvnorm(100, mean=rep(10,3),sigma=sig)))
)

Ecosystem<-c(rep("A",100),rep("B",100))
Samples<-as.factor(seq(1,200,1))
community<-data.frame(Samples,Ecosystem, community)
#The object community is a data frame with metadata (sample name, ecosystem type) followed by columns of abundance data for each species

#The matrix needs to be melted so that all the metadata is maintained.  All species data is collapsed into two columns where the first column, "variable", are species id's and the second column are abundances
community.melt<-melt(community, id=c("Samples","Ecosystem"))

#matrices are made for each ecosystem type and recombined for the analysis so that there is a column of species id's and samples are the y variables
X1<-cast(subset(community.melt, Ecosystem=="A"), Ecosystem+variable~Samples, value="value")
X2<-cast(subset(community.melt, Ecosystem=="B"), Ecosystem+variable~Samples, value="value")
colnames(X2)<-colnames(X1)


cocur<-data.frame(rbind(X1,X2))

trts<-as.vector((unique((community$Ecosystem))))



results<-matrix(nrow=0,ncol=7)
options(warnings=-1)

for(a in 1:length(trts)){
	#pull the first element from the vector of treatments
	trt.temp<-trts[a]
	#subset the dataset for those treatments
	temp<-subset(community, Ecosystem==trt.temp)
	
	#in this case the community data started at column 4, so the loop for co-occurrence has to start at that point
	for(b in 3:(dim(temp)[2]-1)){
		#every species will be compared to every other species, so there has to be another loop that iterates down the rest of the columns
		for(c in (b+1):(dim(temp)[2])){
			
			#summing the abundances of species of the columns that will be compared
			species1.ab<-sum(temp[,b])
			species2.ab<-sum(temp[,c])
			#if the column is all 0's no co-occurrence will be performed
			if(species1.ab >1 & species2.ab >1){
				test<-cor.test(temp[,b],temp[,c],method="spearman",na.action=na.rm)
				rho<-test$estimate
				p.value<-test$p.value
			}
			
			if(species1.ab <=1 | species2.ab <= 1){
				rho<-0
				p.value<-1
			}	
			
			new.row<-c(trts[a],names(temp)[b],names(temp)[c],rho,p.value,species1.ab,species2.ab)
			results<-rbind(results,new.row)			
			
		}
		
	}
	
	
	print(a/length(trts))
	
}


head(results)
results<-data.frame(data.matrix(results))
names(results)<-c("trt","taxa1","taxa2","rho","p.value","ab1","ab2")




head(results)

results_sub<-subset(results, as.numeric(as.character(rho)) > 0.25)
head(results_sub)
g1<-graph.edgelist(as.matrix(subset(results_sub, trt=="A")[,2:3]),directed=FALSE)
g2<-graph.edgelist(as.matrix(subset(results_sub, trt=="B")[,2:3]),directed=FALSE)
V(g1)$color<-"red"
V(g2)$color<-"blue"
E(g1)$color<-"black"
E(g2)$color<-"black"
V(g1)$label<-NA
V(g2)$label<-NA
V(g1)$size<-30
V(g2)$size<-30
par(mfrow=c(1,2))
plot(g1,layout=layout.circle)
plot(g2,layout=layout.circle)


#making the spearman's distance matrix
cocur.dist<-spearman.dist(data.matrix(cocur[,-c(1:2)]))

#the test below is the PERMANOVA outlined in the text
adonis(cocur.dist~cocur$Ecosystem, permutations=9999)

#making figures for figure 1#
head(community)
?geom_smooth()
ggplot(community)+theme_bw()+facet_wrap(~Ecosystem,scales="free")+geom_smooth(aes(x=X4,y=X6,colour=Ecosystem,size=.5),se=FALSE)+theme(aspect.ratio=1)+geom_point(aes(x=X4,y=X6,colour=Ecosystem),size=3,alpha=0.5)+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))+theme(legend.title=element_text(size=15),legend.text=element_text(size=15))+scale_colour_manual(values=c("red","blue"))+labs(x="Microbe 4",y="Microbe 6")


mds<-monoMDS(cocur.dist)
head(cocur)
plot(mds)
dim(cocur)
ggplot.NMDS(mds,cocur$Ecosystem, c("red","blue"))


###simulations for Supplementary Material#
covariance<-seq(1,0,-.01)
results<-matrix(nrow=0,ncol=3)
for(i in 1:length(covariance)){
	xx<-covariance[i]
	for(j in 1:100){
		sig<-matrix(cbind(1,xx,xx,
				     	 xx,1,xx,
					     xx,xx,1),ncol=3)	
					     
		community<-cbind(
		rbind(abs(rmvnorm(100, mean=rep(10,3),sigma=sig)), 			   													cbind(abs(rnorm(100,10,10)),abs(rnorm(100,10,10)),abs(rnorm(100,10,10)))),
		rbind(cbind(abs(rnorm(100,10,10)),abs(rnorm(100,10,10)),abs(rnorm(100,10,10))),abs(rmvnorm(100, 				mean=rep(10,3),sigma=sig)))
		)

		Ecosystem<-c(rep("A",100),rep("B",100))
		Samples<-as.factor(seq(1,200,1))
		community<-data.frame(Samples,Ecosystem, community)

		community.melt<-melt(community, id=c("Samples","Ecosystem"))

		X1<-cast(subset(community.melt, Ecosystem=="A"), Ecosystem+variable~Samples, value="value")
		X2<-cast(subset(community.melt, Ecosystem=="B"), Ecosystem+variable~Samples, value="value")
		colnames(X2)<-colnames(X1)

		cocur<-data.frame(rbind(X1,X2))

		cocur.dist<-spearman.dist(data.matrix(cocur[,-c(1:2)]))


		object<-adonis(cocur.dist~cocur$Ecosystem, permutations=9999)
		Rsq<-object$aov.tab$R2[1]
		p.value<-as.matrix(object$aov.tab[6])[1]
		new.row<-c(covariance[i],Rsq,p.value)
		results<-rbind(results,new.row)
	}
	print(i/length(covariance))
}

###looking at results

data<-read.csv(file.choose(), header=TRUE)
head(data)
data<-data[,-1]

names(data)<-c("correlation","Rsq","p.value")
head(data)
library(ggplot2)
data<-data.frame(data)



ggplot(data, aes(correlation, p.value))+geom_point(alpha=0.5)+theme_bw()+geom_hline(yintercept=0.05, colour="red",size=2)+scale_y_log10(breaks=c(0.0001,0.001,0.01,0.05,.1,1))+theme(aspect.ratio=1)

ggplot(data, aes(correlation, Rsq))+geom_point(alpha=0.5)+theme_bw()+theme(aspect.ratio=1)

ggplot(data, aes(p.value, Rsq))+geom_point(alpha=0.5)+theme_bw()+theme(aspect.ratio=1)+geom_vline(xintercept=0.05, colour="red",size=2)+scale_x_log10(breaks=c(0.0001,0.001,0.01,0.05,.1,1))