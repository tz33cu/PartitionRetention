f.i.2way=function(y,x){
    dat.mat=table(x,y)
    n.d=dat.mat[,1]
    n.u=dat.mat[,2]
    nn.d=sum(n.d)
    nn.u=sum(n.u)
    i.score=nn.d*nn.u*sum((n.d/nn.d-n.u/nn.u)^2)/(nn.d+nn.u)
    return(i.score)
}

f.i.2way.perm=function(y,x, B=1000){
    score.o=f.i.2way(y, x)
    ct=0
    for(i in 1:B){
        ct=ct+1*(f.i.2way(sample(y), x)>score.o)
    }
    return(list(perm.p=ct/B))
}
