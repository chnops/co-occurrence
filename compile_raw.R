treatments <- read.csv('data/treatments.csv', row.names=1)
treatments$reads <- NA

samples <- rownames(treatments)

library(plyr)
counts <- data.frame()
for (sample in 1:length(samples)) {
	fileName <- paste("data/raw_order_", samples[sample], ".tsv", sep="")
	sampleCounts <- data.frame(t(read.table(fileName, sep="\t", row.names=1)))
	counts <- rbind.fill(counts, sampleCounts)

	treatments[samples[sample],"reads"] <- sum(sampleCounts[1,])
}
counts

counts[is.na(counts)] <- 0
counts <- counts[, order(colnames(counts))]
result <- cbind(treatments, counts)

write.csv(result, file="data/order_counts.csv")
q(save="no")
