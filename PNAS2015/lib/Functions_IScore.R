f.list.I=function(var.list, data.x, data.y){
    
    kk=length(var.list)
    
    if(kk>1){
        xx=data.x[,as.vector(var.list)]%*%as.vector((3^(0:(kk-1))))
    }
    else{
        xx=as.matrix(data.x)[,var.list]
    }
    yy=unlist(data.y)
    
    #print(length(xx))
    #print(length(yy))
    
    #dat.mat=table(xx,yy)
    xx=as.factor(xx)
    
    # Globle mean and variation of Y
    y.mean=mean(yy)
    y.var=var(yy)
    
    # Parition means of Y and counts
    mean.vec=unlist(by(yy, xx, mean))
    n.ct.vec=unlist(table(xx))
    
    i.score=sum((mean.vec-y.mean)^2*n.ct.vec^2)/length(yy)/y.var
    
    #n.d=dat.mat[,1]
    #n.u=dat.mat[,2]
    #nn.d=sum(n.d)
    #nn.u=sum(n.u)
    #i.score=nn.d*nn.u*sum((n.d/nn.d-n.u/nn.u)^2)/(nn.d+nn.u)
        
    return(c(var.list, i.score))
}

f.list.screen.I=function(var.list, data.x, data.y){
    
    kk=length(var.list)
    mk.ind=rep(1, kk)
    I.score=NA
    
    #print(paste("Start screening variables:", format(var.list))
    
    score.pre=f.list.I(var.list, data.x, data.y)[length(var.list)+1]

    if(kk>1){
        var.list.use=1:kk
        data.x.use=data.x[,var.list]
        result.v=t(combn(var.list.use, kk-1, f.list.I, simplify=T, 
              data.x=data.x.use, data.y=data.y))
        print("before/after scores")
        print(c(score.pre, max(result.v[,kk])))
        if(max(result.v[,kk])> score.pre){
            mk.ind[-result.v[which.max(result.v[,kk]),1:(kk-1)]]=0
            var.list.use=1:(kk-1)
            data.x.use=data.x[,var.list[mk.ind>0]]
            out.recur=f.list.screen.I(var.list.use, data.x.use, data.y)
            mk.ind[mk.ind>0]=mk.ind[mk.ind>0]*out.recur[(kk-1)+(1:(kk-1))]
            I.score=out.recur[length(out.recur)]
        }
        else{
            I.score=score.pre
        }
    
    }
    else{
        I.score=score.pre
    }
    
    return(c(var.list, mk.ind, I.score))
}
