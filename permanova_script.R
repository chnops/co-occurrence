

library(vegan)
library(reshape)
library(bioDist)

#read in data
comm.data<-read.csv("total_order_info.csv")

# removing columns that aren't useable and samples that do not have enough reads
comm.data<-comm.data[,-1]
comm.data.read<-subset(comm.data, reads >= 1407)

#rarified to 1407
comm.data<-cbind(comm.data.read[,c(1:4)],rrarefy(comm.data.read[,-c(1:4)],1407))

#making sure datasets without enough samples are removed
comm.data_sub<-subset(comm.data, rep != "tropical soil" & rep != "shrubland" & rep != "grassland soil")

comm_data_sub<-comm.data_sub[,-1]

#making sure certain variables are factors
comm_data_sub$MGRASTid<-factor(comm_data_sub$MGRASTid)
comm_data_sub$trt<-factor(comm_data_sub$trt)
comm_data_sub$rep<-factor(comm_data_sub$rep)

#melt data set and recast so that samples are columns and microbes are metadata are rows
comm_data_sub_melt<-melt(comm_data_sub, id=c("MGRASTid","trt","rep"))
comm_data_sub_cast<-cast(comm_data_sub_melt, trt+rep+variable~MGRASTid, value="value",add.missing=TRUE,fun.aggregate=sum)

#make the distance matrix, this will take a while.  Make sure columns of metadata are not included
microbes.dist<-spearman.dist(data.matrix(comm_data_sub_cast[,-c(1:3)]))

#NA's will be removed and replaced with 1's.  They are equal to correlations of 0 (1-0=0 for spearman correlation)
for(i in 1:length(microbes.dist)){
	
	if(is.na(microbes.dist[i])==TRUE){microbes.dist[i]<-1}
}

#run a permanova with trt as the independent variable
adonis(microbes.dist~comm_data_sub_cast$trt,data=NULL)
