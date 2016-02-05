power.model=function(mu1, sigma1, mu2, sigma2){
     return(1-pnorm(qnorm(1-0.01, mu1, sigma1), mu2, sigma2))}
     
error.model=function(mu1, sigma1, mu2, sigma2, p0=0.5){
	 temp.fcn=function(x){
	 	return(min(p0*dnorm(x, mu1, sigma1), (1-p0)*dnorm(x, mu2, sigma2)))}
	 error.rt=integrate(Vectorize(temp.fcn), lower=-Inf, 
	 							  upper=Inf)
     return(error.rt)}
     
par(mfrow=c(1,2), font.main=1, cex.main=1, cex.axis=0.8, mar=c(4,2,1,1))

curve(dnorm(x, 0, 1), -2, 10, xlab="", ylab="")
curve(dnorm(x, 3, 3), -2, 10, add=T, col=2)
legend(1.5, 0.4, c("N(0, 1)", expression("N(3, "*3^2*")")), lty=rep(1,2), col=1:2, bty="n", cex=0.8)
text(-2, 0.2, expression("Med. Sig. Level: "*s[X]*"= 0.0014"), adj=0, cex=0.7)
text(-2, 0.15, expression("Pred. Rate: "*1-e[X]*"= 0.83"), adj=0,cex=0.7)

curve(dnorm(x, 0, 0.05), -3, 3, col=2, xlab="", ylab="")
curve(dnorm(x, 0, 1), -3, 3, add=T, col=1)
legend(-3, 8, c("N(0, 1)", expression("N(0, "*0.05^2*")")), lty=rep(1,2), col=1:2, bty="n", cex=0.8)
#text(0.2, 4, "Power: 0 \nPred. Rate: 0.94", adj=0, cex=0.8)
text(-2.5, 4, expression("Med. Sig. Level:"*s[Y]*"= 0.5"), adj=0, cex=0.7)
text(-2.5, 3, expression("Pred. Rate:"*1-e[Y]*"= 0.94"), adj=0, cex=0.7)

