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
    
    
    #if attributes was gender
    if(aline[2] == "gender_is"){
      
      #gender 1 is male, 2 is female, 3 is unknown
      if(aline[3] == "male"){
        gender = 1
      }else if(aline[3]=="female"){
        gender = 2
      }else{
        gender = 3
      }
      temp <- df$node == aline[1]
      
      #if not found in previous record
      if (is.element(TRUE,temp) == FALSE){
        #print("not exist. ADDING NEW ROW")
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = gender,affected=NA,ava=NA,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
       
        #otherwise edit the current record
      } else{
        #print("record exist")
        index <- which(temp == TRUE)
        
        #col 5 is gender
        df[index,5] <- gender
      }
      #suggest gender to other relate people (wife or husband)
      
      
    #if attributes is name
    }else if(aline[2] == "name_is"){
      temp <- df$node == aline[1]
      
      #if user input both first and last name
      if (length(aline) == 4){
        tname <- paste(aline[3],aline[4])
      } else {
        tname <- aline[3]
      }
      
      # if no record has been found
      if (is.element(TRUE,temp) == FALSE){
        #print("not exist. ADDING NEW ROW")
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = NA,affected=NA,ava=NA,node=aline[1],name=tname,dob=NA,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
        
        #otherwise edit the current record
      } else{
        #print("record exist")
        index <- which(temp == TRUE)
        
        #col 9 is name
        df[index,9] <- tname
      }
    }
    
    #TO-DO
    
    
  #if the line was describing a relation
  } else if(grepl("_of",line) == TRUE ){
    
    print(paste("line: ",linenum,"relation: ",line))
    
    #relationship always happened within 2 people, rline[1] & rline[3] store this two node 
    rline <- unlist(strsplit(line," "))
    
    #handle relationship parent/son
    if(rline[2] == "father_of" || rline[2] == "mother_of"){
      coln <- 0
      if (rline[2] == "father_of"){
        coln <- 3
      } else{
        coln <- 4
      }
      
      temp <- df$node == rline[1]
      temp2 <- df$node == rline[3]
      
      #check first person exist?
      if (is.element(TRUE,temp) == FALSE){
        print("First person record not exists")
        
        #since coln is the column that parent store, coln-2 is gender
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = coln-2,affected=NA,ava=NA,node=rline[1],name=NA,dob=NA,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
      }
        
      #check if second person has exist
      if(is.element(TRUE,temp2) == FALSE){
        print("second person record not exists")
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = NA,affected=NA,ava=NA,node=rline[3],name=NA,dob=NA,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
          
      }
      
      #edit current record
      temp <- df$node == rline[1]
      temp2 <- df$node == rline[3]
      indexa <- which(temp == TRUE)
      indexb <- which(temp2 == TRUE)
          
      #should suggest gender in here?
      #gender has been suggest aboved
      df[indexb,coln] <- df[indexa,2]
      
    }else if(rline[2] == "partner_of"){
      temp <- df$node == rline[1]
      temp2 <- df$node == rline[3]
      
      #check first person exist?
      if (is.element(TRUE,temp) == FALSE){
        print("First person record not exists")
        
        #since coln is the column that parent store, coln-2 is gender
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = NA,affected=NA,ava=NA,node=rline[1],name=NA,dob=NA,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
        
      }
      
      #check if second person has exist
      if (is.element(TRUE,temp2) == FALSE){
        print("second person record not exists")
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = NA,affected=NA,ava=NA,node=rline[3],name=NA,dob=NA,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
      }
      
      #get index
      temp <- df$node == rline[1]
      temp2 <- df$node == rline[3]
      indexa <- which(temp == TRUE)
      indexb <- which(temp2 == TRUE)
      
      #assgin partner
      df[indexa,'partner'] = df[indexb,'id']
      df[indexb,'partner'] = df[indexa,'id']
      
      
      
    }
    
    #TO-DO
    
    
    
    
  #alertuser error when line can not be understand
  } else{
    showerror <- paste("line ",linenum, " has syntax error")
    print(showerror)
    break
  }
  #can have nore function
  
  #show the dataframe after reading eachline
  print(df)
  #Set to nextline
  line<-readLines(con,n=1)
  linenum <- linenum + 1
}
#close connection to the file
close(con)

#function that output the .ped file
print_ped <- function(){
  
}