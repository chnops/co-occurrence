
###read in community data file
comm.data<-read.csv(file.choose())
# this is a vector describing the different treatments from your data (ecosystems, for example)
trts<-as.vector((unique((comm.data$trt))))

results<-matrix(nrow=0,ncol=7)
options(warnings=-1)

#this loop will iterate through the different treatments in your data frame first
for(a in 1:length(trts)){
	#pull the first element from the vector of treatments
	trt.temp<-trts[a]
	#subset the dataset for those treatments
	temp<-subset(body.data, trt==trt.temp)
	
	#in this case the community data started at column 4, so the loop for co-occurrence has to start at that point
	for(b in 4:(dim(temp)[2]-4)){
		#every species will be compared to every other species, so there has to be another loop that iterates down the rest of the columns
		for(c in (4+b):(dim(temp)[2]-3)){
			
			#summing the abundances of species of the columns that will be compared
			species1.ab<-sum(temp[,b])
			species2.ab<-sum(temp[,c])
			#if the column is all 0's no co-occurrence will be performed
			if(species1.ab >0 & species2.ab >0){
				test<-cor.test(temp[,b],temp[,c],method="spearman",na.action=na.rm)
				rho<-test$estimate
				p.value<-test$p.value
			}
			
			if(species1.ab ==0 | species2.ab ==0){
				rho<-0
				p.value<-1
			}	
			
			new.row<-c(trts[a],names(temp)[b],names(temp)[c],rho,p.value,species1.ab,species2.ab)
			body.results<-rbind(results,new.row)			
			
		}
		#The statment below can be used to print out how far along the loop is.  X would be the total number of species and Z is the number of species*trts
		#print((a*X+b)/Z)
	}
	
	
	
	
}


head(results)