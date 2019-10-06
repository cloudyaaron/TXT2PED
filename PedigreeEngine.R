#!/usr/bin/env Rscript


#pasing the argument in to the rscript
args <- commandArgs(T)
if (length(args) == 0){
  print("$Usage Rsript PedigreeEngine.R sample.txt")
  args <- "E:\\study\\binf6112\\project\\test.txt"
  #stop("exit")
  
}else if (length(args) == 1){
  print(length(args))
  print(args)
} else{
  print("$Usage Rsript PedigreeEngine.R sample.txt")
  stop("exit")
}

#reading each line of the text file 
con <- file(args,"r")

#read line one by one by the provindg file
line <- readLines(con,n = 1)
while( length(line) != 0 ) {
  
  #if the line was describing a attributes
  if (grepl("_is",line)== TRUE ){
    print("attributes")
    print(line)
    
    
  #if the line was describing a relation
  } else if(grepl("_of",line)== TRUE ){
    print("relation")
    print(line)
  }#can have nore function
  
  
  
  
  
  #Set to nextline
  line=readLines(con,n=1)
}
#close connection to the file
close(con)
