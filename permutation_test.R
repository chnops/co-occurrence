
#Code adapted from Dr. Dean Adams
perm.ANOVA<-function(Y,X){
F.obs<-anova(lm(Y~X,data=NULL))$F[[1]]  #Find Test value and save
permute<-9999
P.Ftest<-1
F.rand.vec<-NULL
F.rand.vec<-rbind(F.rand.vec,F.obs)
for(i in 1:permute){
 
	y.rand<-sample(Y)  
	F.rand<-anova(lm(y.rand~X))$F[[1]]  
	F.rand.vec<-rbind(F.rand.vec,F.rand)
	P.Ftest<-if(F.rand>=F.obs) (P.Ftest+1) else P.Ftest
	}  
return(c(F.obs,P.Ftest/(permute+1)))

}



