#!/usr/bin/env Rscript


#pasing the argument in to the rscript
args <- commandArgs(T)
if (length(args) == 0){
  print("$Usage Rsript PedigreeEngine.R sample.txt")
  args <- "E:\\study\\binf6112\\project\\test.txt"
  #stop("exit")
  
}else if (length(args) == 1){
  print(args)
} else{
  print("$Usage Rsript PedigreeEngine.R sample.txt")
  stop("exit")
}

#reading each line of the text file 
con <- file(args,"r")

#read line one by one by the provindg file
line <- readLines(con,n = 1)
linenum <- 1
ID <- 1

#init dataframe
df <- data.frame(ped=NA,id=NA,father=NA,mother=NA,sex=NA,affected=NA,ava=NA,node=NA,name=NA,dob=NA,partner=NA,sg=NA)
#print(df)

while( length(line) != 0 ) {
  
  #if the line was describing a attributes
  if (grepl("_is",line) == TRUE ){
    
    print(paste("line: ",linenum, " attributes:",line))
    aline <- unlist(strsplit(line," "))
    
    if(aline[2] == "gender_is"){
      if(aline[3] == "male"){
        gender = 1
      }else if(aline[3]=="female"){
        gender = 2
      }else{
        gender = 3
      }
      temp <- df$node == aline[1]
      if (is.na(temp)){
        print("not exist. ADDING NEW ROW")
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = gender,affected=NA,ava=NA,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
        print(df)
      } else{
      
      }
    
    
    }else if(aline[2] == "name_is"){
      
    }
    
    #TO-DO
    
    
  #if the line was describing a relation
  } else if(grepl("_of",line) == TRUE ){
    
    print(paste("line: ",linenum,"relation: ",line))
    rline <- unlist(strsplit(line," "))
    if(rline[2] == "father_of"){
      
    }else if(rline[2] == "mother_of"){
      
    }
    
    #TO-DO
    
    
    
    
  #alertuser error when line can not be understand
  } else{
    showerror <- paste("line ",linenum, " has syntax error")
    print(showerror)
    break
  }
  #can have nore function
  
  #Set to nextline
  line<-readLines(con,n=1)
  linenum <- linenum + 1
}
#close connection to the file
close(con)

#function that output the .ped file
print_ped <- function(){
  
}