treatments <- read.csv('data/treatments.csv', row.names=1)
treatments$reads <- NA
samples <- rownames(treatments)


#Because output from MG-RAST has some eukaryotic orders and families they must be removed before futher analysis.
badOrders <- c("Carnivora", "Coleochaetales", "Ericales", "Euglyphida", "Eurotiales", "Funariales", "Gelidiales", "Glomerellales", "Grimmiales", "Hypnales", "Isoptera", "Lamiales", "Naviculales", "Pavlovales", "Poales", "Primates", "Polytrichales", "Siphonostomatoida", "Solanales", "Vaucheriales", "Vitales", "Coniferales", "Gloeochaetales", "Gnetales", "Jungermanniales", "Pseudoscourfieldiales", "Glaucocystales", "Asterales", "Brassicales", "Malpighiales", "Oxalidales", "Zingiberales", "Fabales", "Arecales", "Magnoliales", "Fragilariales", "Buxales", "Austrobaileyales", "Isochrysidales", "Polypodiales", "Zygnematales", "Apiales", "Metzgeriales", "Ustilaginales", "Eurotiales", "Psilotales", "Sordariales")
badFamilies <- c("Caligidae", "Ericaceae", "Grimmiaceae", "Hominidae", "Oleaceae", "Poaceae", "Rhinotermitidae", "Solanaceae", "Trichomonadidae", "Vitaceae", "Paulinellidae", "Glaucocystaceae", "Pinaceae", "Asteraceae", "Brassicaceae", "Musaceae", "Oxalidaceae", "Euphorbiaceae", "Fabaceae", "Arecaceae", "Lamiaceae", "Magnoliaceae", "Naviculaceae", "Crossosomataceae", "Convolvulaceae", "Cupressaceae", "Noelaerhabdaceae", "Pteridaceae", "Zygnemataceae", "Apiaceae", "Berberidaceae", "Nymphaeaceae", "Aneuraceae", "Aristolochiaceae", "Phakopsoraceae")


library(plyr)
counts <- data.frame()
for (sample in 1:length(samples)) {
	fileName <- paste("data/raw_order_", samples[sample], ".tsv", sep="")
	sampleCounts <- data.frame(t(read.table(fileName, sep="\t", row.names=1)))

	sampleCounts <- data.frame(sampleCounts[,setdiff(colnames(sampleCounts), badOrders)])
	#sampleCounts <- data.frame(sampleCounts[,setdiff(colnames(sampleCounts), badFamilies)])
	counts <- rbind.fill(counts, sampleCounts)

	treatments[samples[sample],"reads"] <- sum(sampleCounts[1,])
}


counts[is.na(counts)] <- 0
counts <- counts[, order(colnames(counts))]
result <- cbind(treatments, counts)

write.csv(result, file="data/order_counts.csv")
q(save="no")
