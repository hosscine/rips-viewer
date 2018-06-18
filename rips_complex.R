calcVRFilt <- function(X,maxscale,maxdimension,dst=NULL){
  if(is.null(dst)) dst <- as.matrix(dist(X))
  
  bound <- list(which(dst > 0 & dst < maxscale & upper.tri(dst), arr.ind = T))
  colnames(bound[[1]]) <- NULL
  filt <- list(cbind(1,dst[bound[[1]]]))
  
  if(maxdimension >= 2){
    
    for(n in 2:maxdimension){
      
      pares <- t(combn(1:nrow(X),n+1))
      paredist <- matrix(0,nrow(pares),choose(n+1,2))
      parelist <- combn(1:(n+1),2)
      for(i in 1:ncol(paredist)){
        paredist[,i] <- dst[cbind(pares[,parelist[1,i]],
                                  pares[,parelist[2,i]])]
      }
      maxdist <- paredist[cbind(1:nrow(paredist),max.col(paredist))]
      
      bound <- append(bound,list(pares[maxdist <= maxscale,]))
      filt <- append(filt,list(cbind(n,maxdist[maxdist <= maxscale])))
    }
  }
  
  boundary <- lapply(1:nrow(X),function(n)NULL)
  for(n in 1:maxdimension)
    boundary <- c(boundary,lapply(1:nrow(bound[[n]]),function(i)bound[[n]][i,]))
  
  filtration <- do.call(rbind,filt)
  filtration <- rbind(matrix(0,nrow = nrow(X),2),filtration)
  dimnames(filtration) <- list(1:nrow(filtration),c("dim","deg"))
  attr(filtration,"boundary") <- boundary
  
  return(filtration)
  
}

plot.filtration <- function(X=X,K=filtration,t=max(K[,"deg"]),maxdim=max(K[,"dim"])+1,...){
  plot(X,ann=F,type = "n", asp = 1,...)
  v <- lapply(which(K[,"deg"]<=t),calcVertex,attr(K,"boundary"))
  overdim <- F
  for(s in v){
    # 0-simplex
    if(length(s)==1 && maxdim>=0){
      points(X[s,,drop=F])
      text(X[s,,drop=F],labels = s,pos=1)
    }
    # 1-simplex
    else if(length(s)==2 && maxdim>=1){
      segments(x0 = X[s[1],1],y0 = X[s[1],2],
               x1 = X[s[2],1],y1 = X[s[2],2])
    }
    # 2-simplex
    else if(length(s)==3 && maxdim>=2){
      polygon(X[s,],angle = max(s)*pi*180,density = 3)
    }
    else overdim <- T
  }
  if(overdim) 
    warning(paste("drawing dimension of simplex is restricted",min(maxdim,2)),call. = F)
}

showRadius <- function(X, K, t = max(K[,"deg"]), lty = 3){
  require(plotrix)
  . <- apply(X, 1, function(x, t, lty) draw.circle(x[1], x[2], t, lty = lty, border = 2), t/2, lty)
}

calcVertex <- function(i,b){
  s <- i
  while(!is.null(b[[s[1]]])) s <- unique(unlist(b[s]))
  return(s)
}
