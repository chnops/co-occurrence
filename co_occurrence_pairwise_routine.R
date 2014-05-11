
###trying it with the total dataset

library(vegan)
library(reshape)

comm.data<-read.csv("total_order_info.csv")
comm.data<-comm.data[,-1]
comm.data.read<-subset(comm.data, reads >= 1407)

#rarified to 1407 here for order
comm.data<-cbind(comm.data.read[,c(1:4)],rrarefy(comm.data.read[,-c(1:4)],1407))



trts<-as.vector((unique((comm.data$rep))))
trts<-trts[-c(4,5)]


results<-matrix(nrow=0,ncol=7)
options(warnings=-1)

for(a in 1:length(trts)){
	#pull the first element from the vector of treatments
	trt.temp<-trts[a]
	#subset the dataset for those treatments
	temp<-subset(comm.data, rep==trt.temp)
	
	#in this case the community data started at column 6, so the loop for co-occurrence has to start at that point
	for(b in 6:(dim(temp)[2]-1)){
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

#Because output from MG-RAST has some eukaryotic orders and families they must be removed before futher analysis.  These groups are therefore remmoved with the following subetting statements

# for orders
results<-subset(results, taxa1 != "Carnivora" & taxa1 != "Coleochaetales" & taxa1 != "Ericales" & taxa1 != "Euglyphida" & taxa1 != "Eurotiales" & taxa1 != "Funariales" & taxa1 != "Gelidiales" & taxa1 != "Glomerellales" & taxa1 != "Grimmiales" & taxa1 != "Hypnales" & taxa1 != "Isoptera" & taxa1 != "Lamiales" & taxa1 != "Naviculales" & taxa1 != "Pavlovales" & taxa1 != "Poales" & taxa1 != "Primates" & taxa1 != "Polytrichales" & taxa1 != "Siphonostomatoida" & taxa1 != "Solanales" & taxa1 != "Vaucheriales" & taxa1 != "Vitales" & taxa1 != "Coniferales" & taxa1 != "Gloeochaetales" & taxa1 != "Gnetales" & taxa1 != "Jungermanniales" & taxa1 != "Pseudoscourfieldiales" & taxa1 != "Glaucocystales" & taxa1 != "Asterales" & taxa1 != "Brassicales" & taxa1 != "Malpighiales" & taxa1 != "Oxalidales" & taxa1 != "Zingiberales" & taxa1 != "Fabales" & taxa1 != "Arecales" & taxa1 != "Magnoliales" & taxa1 != "Fragilariales" & taxa1 != "Buxales" & taxa1 != "Austrobaileyales" & taxa1 != "Isochrysidales" & taxa1 != "Polypodiales" & taxa1 != "Zygnematales" & taxa1 != "Apiales" & taxa1 != "Metzgeriales" & taxa1 != "Ustilaginales" & taxa1 != "Eurotiales" & taxa1 != "Psilotales" & taxa1 != "Sordariales" & taxa2 != "Carnivora" & taxa2 != "Coleochaetales" & taxa2 != "Ericales" & taxa2 != "Euglyphida" & taxa2 != "Eurotiales" & taxa2 != "Funariales" & taxa2 != "Gelidiales" & taxa2 != "Glomerellales" & taxa2 != "Grimmiales" & taxa2 != "Hypnales" & taxa2 != "Isoptera" & taxa2 != "Lamiales" & taxa2 != "Naviculales" & taxa2 != "Pavlovales" & taxa2 != "Poales" & taxa2 != "Primates" & taxa2 != "Polytrichales" & taxa2 != "Siphonostomatoida" & taxa2 != "Solanales" & taxa2 != "Vaucheriales" & taxa2 != "Vitales" & taxa2 != "Coniferales" & taxa2 != "Gloeochaetales" & taxa2 != "Gnetales" & taxa2 != "Jungermanniales" & taxa2 != "Pseudoscourfieldiales" & taxa2 != "Glaucocystales" & taxa2 != "Asterales" & taxa2 != "Brassicales" & taxa2 != "Malpighiales" & taxa2 != "Oxalidales" & taxa2 != "Zingiberales" & taxa2 != "Fabales" & taxa2 != "Arecales" & taxa2 != "Magnoliales" & taxa2 != "Fragilariales" & taxa2 != "Buxales" & taxa2 != "Austrobaileyales" & taxa2 != "Isochrysidales" & taxa2 != "Polypodiales" & taxa2 != "Zygnematales" & taxa2 != "Apiales" & taxa2 != "Metzgeriales" & taxa2 != "Ustilaginales" & taxa2 != "Eurotiales" & taxa2 != "Psilotales" & taxa2 != "Sordariales")

# for families
results<-subset(results, taxa1 != "Caligidae" & taxa1 != "Ericaceae" & taxa1 != "Grimmiaceae" & taxa1 != "Hominidae" & taxa1 != "Oleaceae" & taxa1 != "Poaceae" & taxa1 != "Rhinotermitidae" & taxa1 != "Solanaceae" & taxa1 != "Trichomonadidae" & taxa1 != "Vitaceae" & taxa1 != "Paulinellidae" & taxa1 != "Glaucocystaceae" & taxa1 != "Pinaceae" & taxa1 != "Asteraceae" & taxa1 != "Brassicaceae" & taxa1 != "Musaceae" & taxa1 != "Oxalidaceae" & taxa1 != "Euphorbiaceae" & taxa1 != "Fabaceae" & taxa1 != "Arecaceae" & taxa1 != "Lamiaceae" & taxa1 != "Magnoliaceae" & taxa1 != "Naviculaceae" & taxa1 != "Crossosomataceae" & taxa1 != "Convolvulaceae" & taxa1 != "Cupressaceae" & taxa1 != "Noelaerhabdaceae" & taxa1 != "Pteridaceae" & taxa1 != "Zygnemataceae" & taxa1 != "Apiaceae" & taxa1 != "Berberidaceae" & taxa1 != "Nymphaeaceae" & taxa1 != "Aneuraceae" & taxa1 != "Aristolochiaceae" & taxa1 != "Phakopsoraceae" & taxa2 != "Caligidae" & taxa2 != "Ericaceae" & taxa2 != "Grimmiaceae" & taxa2 != "Hominidae" & taxa2 != "Oleaceae" & taxa2 != "Poaceae" & taxa2 != "Rhinotermitidae" & taxa2 != "Solanaceae" & taxa2 != "Trichomonadidae" & taxa2 != "Vitaceae" & taxa2 != "Paulinellidae" & taxa2 != "Glaucocystaceae" & taxa2 != "Pinaceae" & taxa2 != "Asteraceae" & taxa2 != "Brassicaceae" & taxa2 != "Musaceae" & taxa2 != "Oxalidaceae" & taxa2 != "Euphorbiaceae" & taxa2 != "Fabaceae" & taxa2 != "Arecaceae" & taxa2 != "Lamiaceae" & taxa2 != "Magnoliaceae" & taxa2 != "Naviculaceae" & taxa2 != "Crossosomataceae" & taxa2 != "Convolvulaceae" & taxa2 != "Cupressaceae" & taxa2 != "Noelaerhabdaceae" & taxa2 != "Pteridaceae" & taxa2 != "Zygnemataceae" & taxa2 != "Apiaceae" & taxa2 != "Berberidaceae" & taxa2 != "Nymphaeaceae" & taxa2 != "Aneuraceae" & taxa2 != "Aristolochiaceae" & taxa2 != "Phakopsoraceae" & taxa2 != "Udoteaceae" & taxa2 != "Rutaceae" & taxa2 != "Dictyoglomaceae" )