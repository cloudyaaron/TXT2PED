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
df <- data.frame(matrix(ncol = 12, nrow = 0))
column_names <- c("ped", "id", "father", "mother", "sex", "affected", "ava", "node", "name", "dob", "partner", "sg")
colnames(df) <- column_names
#print(df)

while( length(line) != 0 ) {

  
# ======================================================
# attributes paraphase
# ======================================================  
  #if the line was describing a attributes
  if (grepl("_is",line) == TRUE ){
    
    print(paste("line: ",linenum, " attributes:",line))
    aline <- unlist(strsplit(line," "))
    
# ======================================================
# attribute: gender_is
# ======================================================    
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
<<<<<<< Updated upstream
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = gender,affected=NA,ava=NA,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA)
=======
        newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = gender,affected=0,ava=0,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA)
>>>>>>> Stashed changes
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
      
# ======================================================
# attribute: name_is
# ======================================================      
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
<<<<<<< Updated upstream
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = NA,affected=NA,ava=NA,node=aline[1],name=tname,dob=NA,partner=NA,sg=NA)
=======
        newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,ava=0,node=aline[1],name=tname,dob=NA,partner=NA,sg=NA)
>>>>>>> Stashed changes
        ID <- ID + 1
        df<-rbind(df,newrow)
        
        #otherwise edit the current record
      } else{
        #print("record exist")
        index <- which(temp == TRUE)
        
        #col 9 is name
        df[index,9] <- tname
      }
      
# ======================================================
# attribute: decaeased_is
# ======================================================
    } else if (aline[2] == "decaeased_is") {
      input_node <- aline[1]
      aline[3] <- toupper(aline[3])
      if (aline[3] == "TRUE") {
        dead = 1
      } else {
        dead = 0
      }
      
      # check if this person exist
      if (input_node %in% df$node) {
        # add new row for this person
<<<<<<< Updated upstream
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = NA,affected=NA,ava=dead,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA)
=======
        index <- which(df$node == input_node)
        df$ava[index] = dead
      } else {
        newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,ava=dead,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA)
>>>>>>> Stashed changes
        ID <- ID + 1
        df<-rbind(df,newrow)
      } else {
<<<<<<< Updated upstream
        index <- which(df$node == input_node)
        df$ava[index] = dead
=======
        a <- 0
      }
      # check if this person exist
      if (input_node %in% df$node) {
        # add new row for this person
        index <- which(df$node == input_node)
        df$affected[index] <- a
      } else {
        newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected = a,ava=0,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
      }
      
      
      
# ======================================================
# attribute: DOB_is
# ======================================================        
    } else if (aline[2] == "DOB_is"){
      input_node <- aline[1]
      DOB <- aline[3]
      # check if this person exist
      if (input_node %in% df$node) {
        # add new row for this person
        index <- which(df$node == input_node)
        df$dob[index] = DOB
      } else {
        newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,ava=0,node=aline[1],name=NA,dob=DOB,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
>>>>>>> Stashed changes
      }
    }
    
    
    
# ======================================================
# Relation paraphase
# ======================================================      
  #if the line was describing a relation
  } else if(grepl("_of",line) == TRUE ){
    
    print(paste("line: ",linenum,"relation: ",line))
    
    #relationship always happened within 2 people, rline[1] & rline[3] store this two node 
    rline <- unlist(strsplit(line," "))
    
    
# ======================================================
# Relation: father/mother
# ======================================================  
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
<<<<<<< Updated upstream
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = coln-2,affected=NA,ava=NA,node=rline[1],name=NA,dob=NA,partner=NA,sg=NA)
=======
        newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = coln-2,affected=0,ava=0,node=rline[1],name=NA,dob=NA,partner=NA,sg=NA)
>>>>>>> Stashed changes
        ID <- ID + 1
        df<-rbind(df,newrow)
      }
        
<<<<<<< Updated upstream
      #check if second person has exist
      if(is.element(TRUE,temp2) == FALSE){
        print("second person record not exists")
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = NA,affected=NA,ava=NA,node=rline[3],name=NA,dob=NA,partner=NA,sg=NA)
        ID <- ID + 1
        df<-rbind(df,newrow)
          
=======
      #if there is more than one children
      if (grepl(',', rline[3]) == TRUE){
        children <<- unlist(strsplit(rline[3],','))
        print('many childrens')
        for (child in children){
          if (!(child %in% df$node)){
            newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,ava=0,node = child,name=NA,dob=NA,partner=NA,sg=NA)
            ID <- ID + 1
            df<-rbind(df,newrow)
          }
        }
      } else {
        children <- c(rline[3])
      #check if second person has exist
        if(is.element(TRUE,temp2) == FALSE){
          print("second person record not exists")
          newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,ava=0,node=rline[3],name=NA,dob=NA,partner=NA,sg=NA)
          ID <- ID + 1
          df<-rbind(df,newrow)
          }
>>>>>>> Stashed changes
      }
      
      #edit current record
      temp <- df$node == rline[1]
      temp2 <- df$node == rline[3]
      indexa <- which(temp == TRUE)
      indexb <- which(temp2 == TRUE)
          
      #should suggest gender in here?
      #gender has been suggest aboved
      df[indexb,coln] <- df[indexa,2]

      
# ======================================================
# Relation: Partner
# ======================================================      
    }else if(rline[2] == "partner_of"){
      temp <- df$node == rline[1]
      temp2 <- df$node == rline[3]
      
      #check first person exist?
      if (is.element(TRUE,temp) == FALSE){
        print("First person record not exists")
        
        #since coln is the column that parent store, coln-2 is gender
<<<<<<< Updated upstream
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = NA,affected=NA,ava=NA,node=rline[1],name=NA,dob=NA,partner=NA,sg=NA)
=======
        newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,ava=0,node=rline[1],name=NA,dob=NA,partner=NA,sg=NA)
>>>>>>> Stashed changes
        ID <- ID + 1
        df<-rbind(df,newrow)
        
      }
      
      #check if second person has exist
      if (is.element(TRUE,temp2) == FALSE){
        print("second person record not exists")
<<<<<<< Updated upstream
        newrow <- data.frame(ped=NA,id = ID,father=NA,mother=NA,sex = NA,affected=NA,ava=NA,node=rline[3],name=NA,dob=NA,partner=NA,sg=NA)
=======
        newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,ava=0,node=rline[3],name=NA,dob=NA,partner=NA,sg=NA)
>>>>>>> Stashed changes
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
       
      #suggest gender if gender was assgined (should also assign father and mother)
      
      
<<<<<<< Updated upstream
=======
      # if partner is male fine father col, vv
      partnerindex <- which(df$node == rline[3])
      if (df[partnerindex,'sex'] == 1){
        checkchild <- df$father == df[partnerindex,'id']
        col <- 4
      } else {
        checkchild <- df$mother == df[partnerindex,'id']
        col <- 3
      }
      
      i <- 1
      while(i< length(checkchild)){
        if (is.na(checkchild[i]) == FALSE && checkchild[i] == TRUE){
          df[i,col] <- df[indexa,'id']
        }
        i <- i+1
      }
    
# ======================================================
# Relation: Brother/Sister
# ====================================================== 
    } else if (rline[2] == "brother_of" || rline[2] == "sister_of"){
      node1 = rline[1]
      node2 = rline[3]
      # check if these two persons in the dataframe
      if (node1 %in% df$node && node2 %in% df$node){
        # check if they have parents, they should have same parents
        index1 = which(df$node == node1)
        index2 = which(df$node == node2)
        if (is.na(df$father[index1]) && !is.na(df$father[index2])) {
          df$father[index1] = df$father[index2]
        } else if (is.na(df$father[index2]) && !is.na(df$father[index1])) {
          df$father[index2] = df$father[index1]
        }
        if (is.na(df$mother[index1]) && !is.na(df$mother[index2])) {
          df$mother[index1] = df$mother[index2]
        } else if (is.na(df$father[index2]) && !is.na(df$father[index1])) {
          df$mother[index2] = df$mother[index1]
        }
        # else they both don't have parents in dataframe, leave the values as NA
        
        # add same generation to each other, node name as string
        if (is.na(df$sg[index1])) {
          df$sg[index1] = as.character(node2)
        } else {
          df$sg[index1] = paste(df$sg[index1], node2, sep = ",")
        }
        if (is.na(df$sg[index2])) {
          df$sg[index2] = as.character(node1)
        } else {
          df$sg[index2] = paste(df$sg[index2], node1, sep = ",")
        }
      # if node1 not in dataframe
      } else if (!(node1 %in% df$node)) {
        f = df$father[index2]
        m = df$mother[index2]
        if (rline[2] == "brother_of") { # brother: male
          s = 1
        } else {                        # sister: female
          s = 2
        }
        newrow <- data.frame(ped=familyid,id=ID,father=f,mother=m,sex = 3,affected=0,ava=0,node=node2,name=NA,dob=NA,partner=NA,sg=c(node1))
        ID <- ID + 1
        df<-rbind(df,newrow)
      # if node2 not in dataframe
      } else if (!(node2 %in% df$node)) {
        f = df$father[index1]
        m = df$mother[index1]
        newrow <- data.frame(ped=familyid,id=ID,father=f,mother=m,sex = 3,affected=0,ava=0,node=node1,name=NA,dob=NA,partner=NA,sg=c(node2))
        ID <- ID + 1
        df<-rbind(df,newrow)
      # if both not in dataframe
      } else {
        newrow <- data.frame(ped=familyid,id=ID,father=NA,mother=NA,sex = 3,affected=0,ava=0,node=node1,name=NA,dob=NA,partner=NA,sg=c(node2))
        ID <- ID + 1
        df<-rbind(df,newrow)
        newrow <- data.frame(ped=familyid,id=ID,father=NA,mother=NA,sex = 3,affected=0,ava=0,node=node2,name=NA,dob=NA,partner=NA,sg=c(node1))
        ID <- ID + 1
        df<-rbind(df,newrow)
      }
>>>>>>> Stashed changes
    }
    
    #TO-DO
    
    
    
# ======================================================
# Alert user when syntax error with line #
# ======================================================
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


# ======================================================
# Function that print .ped file
# ======================================================
<<<<<<< Updated upstream
#function that output the .ped file
print_ped <- function(){
  
}
=======

out <- tryCatch({
  pedAll <- pedigree(id = df$id, dadid = df$father, momid = df$mother, 
                     sex = df$sex, famid = df$ped, affected = df$affected, status = df$ava)
  ped1basic <- pedAll["1"]
  jpeg('./output/output.jpg')#,res = 100 , pointsize = 0.1)
  plot(ped1basic,cex = 0.9, id = df$node,bg = ifelse(df$affected == 0 , 2, 1) )
  pedigree.legend(ped1basic,location = 'bottomright', radius = 0.05)
  dev.off()
  
  },
  error = function(cond){
    msg <-  "fatal error occur, plz remember what has been input and contact the dev group"
    print(msg)
    print(cond)
    pedAll <- pedigree(id = df$id, dadid = df$father, momid = df$mother, 
                       sex = df$sex, famid = df$ped, affected = df$affected, status = df$ava)
    ped1basic <- pedAll["1"]
    jpeg('./output/output.jpg')#,res = 100 , pointsize = 0.1)
    plot(ped1basic,cex = 0.9, id = df$node,bg = ifelse(df$affected == 0 , 2, 1) )
    pedigree.legend(ped1basic,location = 'bottomright', radius = 0.05)
    dev.off()
  },
  warning  = function(cond){
    msg <- 'warnning occur'
    print(msg)
    print(cond)
    pedAll <- pedigree(id = df$id, dadid = df$father, momid = df$mother, 
                       sex = df$sex, famid = df$ped, affected = df$affected, status = df$ava)
    ped1basic <- pedAll["1"]
    jpeg('./output/output.jpg')#,res = 100 , pointsize = 0.1)
    plot(ped1basic,cex = 0.9, id = df$node,bg = ifelse(df$affected == 0 , 2, 1) )
    pedigree.legend(ped1basic,location = 'bottomright', radius = 0.05)
    dev.off()
  },
  finallly = {
    write.csv(df[,1:10],'./output/log.txt')
    dev.off}
)










>>>>>>> Stashed changes
