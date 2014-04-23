setwd("/Users/metagenomics/Desktop/Ryan_co-occur_code/revision_work/")

results<-read.csv("order_results.csv")
results<-results[,-1]
list(unique(results$trt))
soils_results<-subset(results, trt=="soil" |trt=="tropical soil" | trt=="forest soil" | trt=="shrubland" | trt=="grassland soil")
apple_results<-subset(results, trt=="Unsprayed" |trt=="Sprayed_w_streptomycin" )
body_results<-subset(results, trt=="M1"| trt=="M2"| trt=="M3" | trt=="M4" | trt=="M5" | trt=="M6" | trt=="F1" | trt=="F2" | trt=="F3")

