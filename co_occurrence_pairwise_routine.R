#Here is the first step for anaylsis of actual data
library(vegan)
library(reshape)


#Import and normalize data
# FOR ORDERS
#comm.data<-read.csv("data/total_order_info.csv", row.names=1)
comm.data<-read.csv("data/order_counts.csv", row.names=1)
rarefactionCutoff <- 1407
# FOR FAMILIES
#comm.data<-read.csv("data/total_family_info.csv")
#rarefactionCutoff <- 1353

comm.data.read<-subset(comm.data, reads >= rarefactionCutoff)
comm.data<-cbind(comm.data.read[,c(1:4)],rrarefy(comm.data.read[,-c(1:4)],rarefactionCutoff))


# Calculate pairwise taxa correlations per treatment
trts<-as.vector((unique((comm.data$rep))))
trts<-trts[-c(4,5)]

results<-matrix(nrow=0,ncol=7)
for(a in 1:length(trts)){
	trt.comm.data<-subset(comm.data, rep==trts[a])
	
	# first taxa count column = 4
	for(b in 4:(dim(trt.comm.data)[2]-1)){
		for(c in (b+1):(dim(trt.comm.data)[2])){
			
			# ab = abundance
			species1.ab<-sum(trt.comm.data[,b])
			species2.ab<-sum(trt.comm.data[,c])
		
			if(species1.ab >1 & species2.ab >1){
				test<-cor.test(trt.comm.data[,b],trt.comm.data[,c],method="spearman",na.action=na.rm, exact=F)
				rho<-test$estimate
				p.value<-test$p.value
			}
			else {
				rho<-0
				p.value<-1
			}	
			new.row<-c(trts[a],names(trt.comm.data)[b],names(trt.comm.data)[c],rho,p.value,species1.ab,species2.ab)
			results<-rbind(results,new.row)			
		}
	}
	#this step can be very long.  This tracks your progress
	print(a/length(trts))
}
results<-data.frame(results)
names(results)<-c("trt","taxa1","taxa2","rho","p.value","ab1","ab2")

write.csv(results, file="data/order_pairwise_spearman.csv")
#write.csv(results, file="data/family_pairwise_spearman.csv")
q(save="yes")
