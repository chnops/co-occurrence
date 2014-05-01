
setwd("/Users/metagenomics/Desktop/Ryan_co-occur_code/revision_work/")
library(vegan)
library(reshape)

comm.data<-read.csv("total_family_info.csv")
comm.data<-comm.data[,-1]
comm.data.read<-subset(comm.data, reads >= 1353)

#rarified to 1407
comm.data<-cbind(comm.data.read[,c(1:4)],rrarefy(comm.data.read[,-c(1:4)],1353))
trts<-as.vector((unique((comm.data$rep))))
trts

comm.data_sub<-subset(comm.data, rep != "tropical soil" & rep != "shrubland" & rep != "grassland soil")
head(comm.data_sub[,1:10])
comm_data_sub<-comm.data_sub[,-1]

comm_data_sub$MGRASTid<-factor(comm_data_sub$MGRASTid)
comm_data_sub$trt<-factor(comm_data_sub$trt)
comm_data_sub$rep<-factor(comm_data_sub$rep)
str(comm_data_sub)

comm_data_soil<-subset(comm_data_sub, trt=="soil")
comm_data_apple<-subset(comm_data_sub, trt=="apple")
comm_data_female<-subset(comm_data_sub, rep=="F1" | rep=="F2" | rep=="F3")
comm_data_male<-subset(comm_data_sub, rep=="M1" | rep=="M2" | rep=="M3" | rep=="M4" | rep=="M5" | rep=="M6")
head(comm_data_soil[,1:10])
soil_melt<-melt(comm_data_soil,id=c("MGRASTid","trt","rep"))
apple_melt<-melt(comm_data_apple,id=c("MGRASTid","trt","rep"))
female_melt<-melt(comm_data_female,id=c("MGRASTid","trt","rep"))
male_melt<-melt(comm_data_male,id=c("MGRASTid","trt","rep"))

soil_cast<-cast(soil_melt, trt+rep+variable~MGRASTid, value="value",add.missing=TRUE,fun.aggregate=sum)
names(comm_data_sub)
comm_data_sub_melt<-melt(comm_data_sub, id=c("MGRASTid","trt","rep"))
comm_data_sub_cast<-cast(comm_data_sub_melt, trt+rep+variable~MGRASTid, value="value",add.missing=TRUE,fun.aggregate=sum)
library(bioDist)
library(vegan)
microbes.dist<-spearman.dist(data.matrix(comm_data_sub_cast[,-c(1:3)]))
for(i in 1:length(microbes.dist)){
	
	if(is.na(microbes.dist[i])==TRUE){microbes.dist[i]<-0}
}

adonis(microbes.dist~comm_data_sub_cast$trt,data=NULL)
