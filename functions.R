ggplot.NMDS<-function(XX,ZZ,COLORS){
	library(ggplot2)
	MDS1<-data.frame(scores(XX))$MDS1
	MDS2<-data.frame(scores(XX))$MDS2
	Aggregate.Fraction<-ZZ
	
	NMDS<-data.frame(MDS1,MDS2,Aggregate.Fraction)
	
	NMDS.mean=aggregate(NMDS[,1:2],list(group=Aggregate.Fraction),mean)
	
	veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) {
		theta <- (0:npoints) * 2 * pi/npoints
		Circle <- cbind(cos(theta), sin(theta))
		(center + scale * t(Circle %*% chol(cov)))
	}

	ellipses <- list()
	for( g in levels(NMDS$Aggregate.Fraction)) {
		newEllipse <- cbind(as.data.frame(t(with(NMDS[NMDS$Aggregate.Fraction==g,],
		veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2))))))
		,group=g)

		ellipses[[length(ellipses) + 1]] <- newEllipse
	}

	X1<-ggplot(data = NMDS, aes(MDS1, MDS2))
	X1 <- X1 + geom_point(aes(color = Aggregate.Fraction),size=5) +
		scale_color_manual(values=COLORS) + 
		theme(axis.text.x=element_text(size=20),axis.text.y=element_text(size=20),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20)) + 
		theme(legend.title=element_text(size=15),legend.text=element_text(size=15))+theme_bw()+theme(aspect.ratio=1)
	for (ellipse in 1:length(ellipses)) {	
		X1 <- X1 + geom_path(data=ellipses[[ellipse]], aes(x=MDS1, y=MDS2, colour=group), size=0, linetype=5)
	}
	X1
}
