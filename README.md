makeCacheMatrix<-function(x=matrix()){
        inve<-NULL
        set<-function(y){
                x<<-y
                inve<<-NULL
        }
        get<-function()x
        setinverseM<-function(inverse) inve<<-inverse
        getinverseM<-function()inve
        list(set=set,get=get,setinverseM=setinverseM,getinverseM=getinverseM)
}

cacheSolve<-function(x,...){
        inve<-x$getinverseM()
        if(!is.null(inve)){
                message("getting cached data")
                return(inve)
        }
        matr<-x$get()
        inve<-solve(matr,...)
        x$setinverseM(inve)
        inve
}
