if(!file.exists("ProgAsg.zip"))
{
        download.file("https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip",
                      destfile = "ProgAsg.zip")
        
        unzip("ProgAsg.zip",overwrite = FALSE)
}

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
rankall <- function(outcome,num) {
        
        ## Check that state and outcome are valid "heart failure"|"heart attack"|"pneumonia"
        
                if(outcome%in%c("heart failure","heart attack","pneumonia"))
                {carestat<-split(care,care$stat)
                        
                        ## create an empty df to store looping result
                        finaldf<-data.frame(character(),character())
                        colnames(finaldf)<-c("hospital","state")
                        
                        
                
                ## Return hospital name in that state with lowest 30-day death rate
                for(h in names(carestat))
                {
                      
                      ## subsetting state,and arranging in order
                      carefilter<-carestat[[h]]
                      carefilter<-carefilter[order(carefilter[,1]),]
                      n<-match(outcome,colnames(carefilter))
                      carefilter<-carefilter[order(carefilter[,n],na.last = NA),]
                      
                      ## accommodate for "best and "worst
                      if(num=="best"){num<-1}
                      else if(num=="worst"){num<-nrow(carefilter)}
                      else   {num<-num}
                      
                      ## locating result
                      i<-carefilter[num,"stat"]#extract rate
                      j<-carefilter[num,"name"]#extract name
                      
                      ##accumulating result
                      part_df<-data.frame(hospital=j,state=i)
                      finaldf<-rbind(finaldf,part_df)
                        
                }
                
                       print(finaldf) 
                        
                }
                else 
                {
                        stop("invalid outcome")
                }
        
       
}
