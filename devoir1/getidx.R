getidx <- function(M, i, j) {
    n<-nrow(M)
    index<-(j-1)* n+ i
    return((as.vector(M)[[index]]))
}

M<-matrix(1:9,nrow=3,ncol=3)
print(M)
getidx(M,2,3)


M1<-matrix(1:16,nrow = 4,ncol=4)
print(M1)
getidx(M1,3,4)
