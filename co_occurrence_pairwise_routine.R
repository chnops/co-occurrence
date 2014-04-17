
###trying it with the total dataset
setwd("/Users/metagenomics/Desktop/Ryan_co-occur_code/revision_work/")
library(vegan)
library(reshape)

comm.data<-read.csv("total_genus_info.csv")
comm.data<-comm.data[,-1]
comm.data.read<-subset(comm.data, reads >= 1517)

#rarified to 1407
comm.data<-cbind(comm.data.read[,c(1:4)],rrarefy(comm.data.read[,-c(1:4)],1517))



trts<-as.vector((unique((comm.data$rep))))

results<-matrix(nrow=0,ncol=7)
options(warnings=-1)

for(a in 1:length(trts)){
	#pull the first element from the vector of treatments
	trt.temp<-trts[a]
	#subset the dataset for those treatments
	temp<-subset(comm.data, trt==trt.temp)
	
	#in this case the community data started at column 4, so the loop for co-occurrence has to start at that point
	for(b in 5:(dim(temp)[2]-1)){
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
	print(b/dim(temp)[2])	
	}
	
	
	
	
}


head(results)
results<-data.frame(data.matrix(results))
names(results)<-c("trt","taxa1","taxa2","rho","p.value","ab1","ab2")

