#simulating 10 species. Species 1-5 are correlated from ecosystem A and species 6-10 are correlated from ecosystem B (different co-occurrence).  Composition will be different in each


library(bioDist)
library(vegan)
library(mvtnorm)
library(reshape)
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

#making the spearman's distance matrix
cocur.dist<-spearman.dist(data.matrix(cocur[,-c(1:2)]))

#the test below is the PERMANOVA outlined in the text
adonis(cocur.dist~cocur$Ecosystem, permutations=9999)




###simulations for Supplementary Material
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