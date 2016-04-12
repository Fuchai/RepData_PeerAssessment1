newsteps<-apply(cpy,1,function(x){
    steps=x[1]
    interval=x[2]
    interval<-as.character(interval)
    if (is.na(steps)){
        meanWithoutNA[[interval]]
    }
    else{
        x[1]
    }
})