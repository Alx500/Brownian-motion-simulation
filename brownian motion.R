acc<-function(P,i,t=12,y,plotit=TRUE,...){
  if(missing(P)||missing(i)||missing(y)){
    cat("Falta de argumentos (P,i,y).\n")
    return(NA)
  }
  
  F<-vector("double",as.integer(y))
  for(j in 1:as.integer(y)){
    F[j]=P*(1+i/(100*t))^(t*j)
  }
  
  if(plotit==TRUE){
    plot(1:as.integer(y),F,type = "s",...)
  }
  
  else{
    return(F)
  }
}


Haar<-function(n,k,t=0,plotit=FALSE){
  if(n==0 && k==1){
    if(plotit== TRUE){
      plot(c(0,1),c(1,1),type = "s",col="BLUE",xlab = "x",ylab = "y")
      abline(h=0)
      abline(v=0)
    }
    return (1)
  }
  
  if(k%%2==0 || k>=2^n){
    stop("El valor 'k' no es impar o es mayor igual que 2^n o esta fuera de limite")
  }
  
  if(plotit==TRUE){
    x<-vector("double",2^n+1)
    for (i in 0:2^n) {
      x[i+1]<-i/(2^n)
    }
    y<-vector("double",2^n+1)
    for (i in 1:(2^n+1)) {
      y[i]<-Haar(n,k,x[i])
    }
    plot(x,y,type = "s",col="BLUE")
    abline(h=0)
    abline(v=0)
  }
  
  if(t>=(k-1)/2^n && t<k/2^n){
    return(2^((n-1)/2))
  }
  
  if(t>=k/(2^n) && t<(k+1)/(2^n)){
    return(-2^((n-1)/2))
  }
  
  else{
    return(0)
  }
}


snk<-function(n,k,t=0,plotit=FALSE){
  if(n==0 && k==1){
    if(plotit==TRUE){
      x<-c(0,1)
      y<-c(0,1)
      plot(x,y,type = "l",col="BLUE")
      abline(h=0)
      abline(v=0)
    }
    return (t)
  }
  
  if(k%%2==0 || k>=2^n){
    stop("El valor 'k' no es impar o es mayor igual que 2^n o esta fuera de limite")
  }
  
  if(plotit==TRUE){
    x<-c((k-1)/2^n,k/(2^n),(k+1)/2^n)
    y<-c(0,2^(-(n+1)/2),0)
    xl<-c(0,1)
    yl<-c(0,2^(-(n+1)/2))
    plot(x,y,type = "l",col="BLUE",xlim = xl,ylim = yl)
    abline(h=0)
    abline(v=0)
  }
  
  if(t>=(k-1)/2^n && t<k/2^n){
    return(2^((n-1)/2)*(t-(k-1)/(2^n)))
  }
  
  if(t>=k/(2^n) && t<(k+1)/(2^n)){
    return(2^(-(n+1)/2)-2^((n-1)/2)*(t-k/(2^n)))
  }
  
  else{
    return(0)
  }
}

impares<-function(n){
  imp<-c(1)
  if(n>0){
    return(seq(1,2^n-1,2))
  }
  return(imp)
}

Bn<-function(n,t=0,plotit=FALSE){
  sume<-0
  N<-matrix(NA,n+1,2^n)
  for (i in 0:n) {
    for (j in impares(i)) {
      N[i+1,j]<-rnorm(1)
    }
  }
  for (m in 0:n) {
    for (k in impares(m)) {
      sume<-sume+N[m+1,k]*snk(m,k,t)
    }
  }
  h<-500
  if(plotit==TRUE){
    x<-seq(0,1,length.out = h)
    y<-vector("double",h)
    for (i in 1:h) {
      y[i]<-0
      for (m in 0:n) {
        for (k in impares(m)) {
          y[i]<-y[i]+N[m+1,k]*snk(m,k,x[i])
        }
      }
    }
    plot(x,y,type = "l",col="BLUE")
    abline(h=0,v=0)
  }
  return(sume)
}

