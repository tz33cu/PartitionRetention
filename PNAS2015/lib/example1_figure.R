library(lattice)

#mypanel <- function(x,y,z,...) {
#  panel.wireframe(x,y,z,...)
#  panel.cloud(x,y,z,...)
#}
#wireframe(results.flat.1$Bayes.rate ~ results.flat.1$or.scale * results.flat.1$MAF, xlab="X", ylab="Y", zlab="Z",
#          panel=mypanel)
######
library(latticeExtra)

png("bayesrate.png", width=400, height=400)
mypanel <- function(x,y,z,...){

    panel.3dbars(x,y,z, ...,
    col.facet=panel.col[trellis.panelArgs()$subscripts])
                           
}
view.ang=list(z=100, x=-70)

n.cut=20
panel.col=level.colors(results.flat.1$Bayes.rate[results.flat.1$sample.size==levels(results.flat.1$sample.size)[1]], 
        at = do.breaks(range(results.flat.1$Bayes.rate), n.cut),
                               col.regions = topo.colors,
                               colors = TRUE)
cloud(Bayes.rate ~ or.scale * MAF, data=results.flat.1[results.flat.1$sample.size==levels(results.flat.1$sample.size)[1],],
     xlab=list(label="OR", cex=1.2), 
     ylab=list(label="Allele Freq", cex=1.2), 
     zlab=list(label="True Prediction Rate", cex=1.2, rot=90), cex.axis=0.7,
     panel.3d.cloud=mypanel, xbase=or.step*0.8, ybase=p.step*0.8, 
      main=list(label="Theoretical Bayes rate", cex=1.5),
     screen=view.ang, #layout=c(3,1),
     colorkey = list(col = topo.colors, 
                     at = do.breaks(range(results.flat.1$Bayes.rate), n.cut)), lwd=0.4)
dev.off()

png("PRI.png", width=1200, height=400)
n.cut=20
panel.col=level.colors(results.flat.3$PR.I, 
        at = do.breaks(range(results.flat.3$PR.I), n.cut),
                               col.regions = topo.colors,
                               colors = TRUE)

cloud(PR.I ~ or.scale * MAF|sample.size, data=results.flat.3,
     xlab=list(label="OR", cex=1), 
     ylab=list(label="Allele Freq", cex=1), 
     zlab=list(label="PR\'s I", cex=1, rot=90), cex.axis=0.7,
     panel.3d.cloud=mypanel, xbase=or.step*0.8, ybase=p.step*0.8, 
      main=list(label="I score", cex=1.5),
     screen=view.ang, layout=c(3,1),
     colorkey = list(col = topo.colors, 
                     at = do.breaks(range(results.flat.3$PR.I), n.cut)),
     lwd=0.4)
dev.off()

png("trainrate.png", width=1200, height=400)

n.cut=20
panel.col=level.colors(results.flat.2$Training.rate, 
        at = do.breaks(range(results.flat.2$Training.rate), n.cut),
                               col.regions = topo.colors,
                               colors = TRUE)
cloud(Training.rate ~ or.scale * MAF|sample.size, data=results.flat.2,
     xlab=list(label="OR", cex=1), 
     ylab=list(label="Allele Freq", cex=1), 
     zlab=list(label="Prediction rate in training set", cex=0.8, rot=90), 
     panel.3d.cloud=mypanel, xbase=or.step*0.8, ybase=p.step*0.8, 
     main=list(label="In-sample training prediction rate", cex=1.5),
     screen=view.ang, layout=c(3,1),
      colorkey = list(col = topo.colors, 
                      at = do.breaks(range(results.flat.2$Training.rate), n.cut)))

dev.off()
