if(!file.exists("ProgAsg.zip"))
{
        download.file("https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip",
                      destfile = "ProgAsg.zip")
}

unzip("ProgAsg.zip",overwrite = FALSE)

## read csv file into raw data categories
hosraw<-read.csv("hospital-data.csv",header = TRUE,na.strings = "Not Available
")
careraw<-read.csv("outcome-of-care-measures.csv",header=TRUE,na.strings = "Not Available
")

## subsetting useful columns data
care<-careraw[,c(2,7,11,17,23)]
names<-c("name","stat","heart attack","heart failure",'pneumonia')
colnames(care)<-names



##changing the class from char to num
class(care$`heart failure`)<-"numeric"
class(care$`heart attack`)<-"numeric"
class(care$`pneumonia`)<-"numeric"


## define rankhospital function
rankhospital <- function(state,outcome,num) {
        
        ## Check that state and outcome are valid "heart failure"|"heart attack"|"pneumonia"
        if(state%in%care$stat){
                if(outcome%in%c("heart failure","heart attack","pneumonia"))
                {
                        ## Return hospital name in that state with lowest 30-day death rate
                        n<-match(outcome,colnames(care))
                        carefilter<-subset(care,stat==state)
                        carefilter<-carefilter[order(carefilter[,1]),]
                        carefilter<-carefilter[order(carefilter[,n],na.last = NA),]
                        
                        
                                
                                ##accommodate for "best and "worst
                                if(num=="best"){m<-1}
                                else if(num=="worst"){m<-nrow(carefilter)}
                                else   {m<-num}
                        
                        carefilter[m,'name']
                }
                else 
                {
                        stop("invalid outcome")
                }
        }
        else
        { 
                stop("invalid state")
        }
        
}
