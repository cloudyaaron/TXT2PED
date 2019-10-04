#!/usr/bin/env Rscript


#pasing the argument in to the rscript
args = commandArgs(T)
if (length(args) == 0){
  print("$Usage Rsript PedigreeEngine.R sample.txt")
  stop("exit")
}else if (length(args) == 1){
  print(length(args))
  print(args)
} else{
  print("$Usage Rsript PedigreeEngine.R sample.txt")
  stop("exit")
}

#reading the text file into DATA
data = read.table(args)