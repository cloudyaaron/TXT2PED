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
while( length(line) != 0 ) {
  
  #if the line was describing a attributes
  if (grepl("_is",line) == TRUE ){
    
    print(paste("attributes:",line))
    aline <- unlist(strsplit(line," "))
    if(aline[2] == "gender_is"){
      
    }else if(){
      
    }
    
  #if the line was describing a relation
  } else if(grepl("_of",line) == TRUE ){
    
    print(paste("relation: ",line))
    rline <- unlist(strsplit(line," "))
    if(rline[2] == "father_of"){
      
    }else if(){
      
    }
    
    
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