#!/usr/bin/env Rscript
library(quadprog)
library(Matrix)
library(kinship2)
#require(quadprog)
#require(Matrix)
#require(kinship2)




# ======================================================
# give a .ped file in the same folder
# ======================================================
print_ped <- function(df){
  write.csv(df[,1:7],file = "./output/output.ped")
}


preview <- function(path){

  file_name = as.character(path)
  con <- file(file_name,"r")
  line <- readLines(con, n = 1)
  t <- ""
 
  while( length(line) != 0 ) {
    t <- paste(t,line,"\n",sep = "")
    
    line<-readLines(con,n=1)
    
  }
  close(con)
  return(t)
}


getlog <- function(){
  
  con <- file("./output/log.txt","r")
  line <- readLines(con, n = 1)
  t <- ""
  
  while( length(line) != 0 ) {
    t <- paste(t,line,"\n",sep = "")
    
    line<-readLines(con,n=1)
    
  }
  close(con)
  return(t)
}


producePED <- function(inFile) {
  
  #print(inFile)
  file_name = as.character(inFile)
  #print(file_name)
  con <- file(file_name,"r")
  logcon <- file("./output/log.txt")
  
  #read line one by one by the provindg file
  line <- readLines(con, n = 1)
  linenum <- 1
  ID <- 1
  familyid <- 1
  logtext <- ""
  #init dataframe
  df <- data.frame(matrix(ncol = 14, nrow = 0))
  column_names <- c("ped", "id", "father", "mother", "sex", "affected", "deceased","twin", "node", "name", "dob", "partner", "sg","ad")
  colnames(df) <- column_names
  #print(df)
  
  while( length(line) != 0 ) {
  
    
  # ======================================================
  # attributes paraphase
  # ======================================================  
    #if the line was describing a attributes
    if (grepl("_is",line) == TRUE ){
      
      print(paste("line: ",linenum, " attributes:",line))
      logtext <- paste(logtext,"\n", "line: ",linenum, " attributes:",line) 
      
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
        if (grepl(",",aline[1]) == TRUE){
          people <- unlist(strsplit(aline[1],','))
          
          for (pe in people){
            peindex <- which(df$node == pe)
            
            df[peindex,'sex'] <- gender
          }
        } else {
          
          
          temp <- df$node == aline[1]
          
          #if not found in previous record
          if (is.element(TRUE,temp) == FALSE){
            #print("not exist. ADDING NEW ROW")
            newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = gender,affected=0,deceased=0,twin=NA,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA,ad=NA)
            ID <- ID + 1
            df<-rbind(df,newrow)
            
            #otherwise edit the current record
          } else{
            #print("record exist")
            index <- which(temp == TRUE)
            
            #col 5 is gender
            df[index,5] <- gender
          }
          
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
          newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,deceased=0,twin=NA,node=aline[1],name=tname,dob=NA,partner=NA,sg=NA,ad=NA)
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
  # attribute: deceased_is
  # ======================================================
      } else if (aline[2] == "deceased_is"){
        input_node <- aline[1]
        aline[3] <- toupper(aline[3])
        if (aline[3] == "TRUE") {
          dead = 1
        } else {
          dead = 0
        }
        if (grepl(",",aline[1]) == TRUE){
          people <- unlist(strsplit(aline[1],','))
          
          for (pe in people){
            peindex <- which(df$node == pe)
            df[peindex,'deceased'] <- dead
          }
        } else {
          if (input_node %in% df$node) {
            # add new row for this person
            index <- which(df$node == input_node)
            df$deceased[index] = dead
          } else {
            newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,deceased=dead,twin=NA,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA,ad=NA)
            ID <- ID + 1
            df<-rbind(df,newrow)
          }
        }
        # check if this person exist

  
  # ======================================================
  # attribute: affected_is
  # ======================================================  
      } else if(aline[2] == "affected_is"){
        input_nodes <- aline[1]
        affected <- aline[3]
        ds <- unlist(strsplit(affected,","))
        print(ds)
       
        if (length(ds)>4 || length(names(df)) > 17){
          logtext <- paste(logtext,"\n", "There are more than 4 dieases")
        }else{
          for (d in ds) {
            ns <- unlist(strsplit(input_nodes,","))
            if(d %in% names(df)){
              print("d exists")
              for(n in ns){
                index <- which(df$node == n)
                df[index,d] <- 1
                df$affected[index] <-1
              }
              
            }else{
              print(paste(d,"not exists"))
              
              df[,d] <- 0
              
              for(n in ns){
                index <- which(df$node == n)
                df[index,d] <- 1
                df$affected[index] <-1
              }
            }
          }
          print(df)
        }
 
        
        
        # check if this person exist
#        if (input_node %in% df$node) {
 ##        # add new row for this person
   #       index <- which(df$node == input_node)
  #        df[index,'affected'] <- a
   #     } else {
    #      newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected= a,deceased=0,node=aline[1],name=NA,dob=NA,partner=NA,sg=NA,ad=NA)
     #     ID <- ID + 1
      #    df<-rbind(df,newrow)
       # }
        
        
        
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
          newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,deceased=0,twin=NA,node=aline[1],name=NA,dob=DOB,partner=NA,sg=NA,ad=NA)
          ID <- ID + 1
          df<-rbind(df,newrow)
        }
  # ======================================================
  # attribute: ad_is
  # ======================================================            
        # need more spacve
      }else if (aline[2] == "ad_is"){
        ad<-""
        if (length(aline)>3){
          i <- 3
          while(i <= length(aline) ){
            ad <- paste(ad,aline[i])
            i = i+1
          }
        } else{
          ad <- aline[3]
          
        }
        noden <- aline[1]
        if (noden %in% df$node){
          index <- which(df$node == noden)
          df$ad[index] = ad
        }else{
          logtext <- paste(logtext,"\n", noden,"doesn't exist") 
        }
        
      
      
      
      
      } else {
        showerror <- paste("line ",linenum, " has unexpected attributes")
        logtext <- paste(logtext,"\n", "line ",linenum, " has unexpected attributes") 
        print(showerror)
        break
      }
      
      #suggest the info with current info
    
  
      
  # ======================================================
  # Relation paraphase
  # ======================================================      
    #if the line was describing a relation
    } else if(grepl("_of",line) == TRUE ){
      
      print(paste("line: ",linenum,"relation: ",line))
      logtext <- paste(logtext,"\n", "line: ",linenum,"relation: ",line)
      
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
          #print("First person record not exists")
          
          #since coln is the column that parent store, coln-2 is gender
          newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = coln-2,affected=0,deceased=0,twin=NA,node=rline[1],name=NA,dob=NA,partner=NA,sg=NA,ad=NA)
          ID <- ID + 1
          df<-rbind(df,newrow)
        }
          
        #if there is more than one children
        if (grepl(',', rline[3]) == TRUE){
          children <<- unlist(strsplit(rline[3],','))
          #print('many childrens')
          for (child in children){
            if (!(child %in% df$node)){
              newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,deceased=0,twin=NA,node = child,name=NA,dob=NA,partner=NA,sg=NA,ad=NA)
              ID <- ID + 1
              df<-rbind(df,newrow)
            }
          }
        } else {
          children <- c(rline[3])
        #check if second person has exist
          if(is.element(TRUE,temp2) == FALSE){
            #print("second person record not exists")
            newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,deceased=0,twin=NA,node=rline[3],name=NA,dob=NA,partner=NA,sg=NA,ad=NA)
            ID <- ID + 1
            df<-rbind(df,newrow)
            }
        }
        #print(children)
        
        # change parent into appropriet gender
        parentindex <- which(df$node == rline[1])
        df[parentindex,'sex'] <- coln-2
        
        #change children or single child parent's col
        for(child in children){
          childindex <- which(df$node == child)
          df[childindex,coln] <- df[parentindex,2]
        }
  
        
  # ======================================================
  # Relation: Partner
  # ======================================================      
      }else if(rline[2] == "partner_of"){
        temp <- df$node == rline[1]
        temp2 <- df$node == rline[3]
        
        #check first person exist?
        if (is.element(TRUE,temp) == FALSE){
          #print("First person record not exists")
          
          #since coln is the column that parent store, coln-2 is gender
          newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,deceased=0,twin=NA,node=rline[1],name=NA,dob=NA,partner=NA,sg=NA,ad=NA)
          ID <- ID + 1
          df<-rbind(df,newrow)
          
        }
        
        #check if second person has exist
        if (is.element(TRUE,temp2) == FALSE){
          #print("second person record not exists")
          newrow <- data.frame(ped=familyid,id = ID,father=NA,mother=NA,sex = 3,affected=0,deceased=0,twin=NA,node=rline[3],name=NA,dob=NA,partner=NA,sg=NA,ad=NA)
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
        # suggest gender from partner
        if (df[indexb,'sex'] == 1){
          df[indexa,'sex'] <- 2
        } else if (df[indexb,'sex'] == 2){
          df[indexa,'sex'] <- 1
        } 
        
        
        #assign father/mother for children
        
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
        node1_sex = 0
        if (rline[2] == "brother_of") { # brother: male
          node1_sex = 1
        } else {                        # sister: female
          node1_sex = 2
        }
        # check if these two persons in the dataframe
        if (node1 %in% df$node && node2 %in% df$node){
          index1 = which(df$node == node1)
          index2 = which(df$node == node2)
          # add gender for node 1
          df$sex[index1] = node1_sex
          # check if they have parents, they should have same parents
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
            # if node2 is not in node1's sg
            if (!grepl(node2, df$sg[index1])) {
              df$sg[index1] = paste(df$sg[index1], node2, sep = ",")
            }
            
          }
          if (is.na(df$sg[index2])) {
            df$sg[index2] = as.character(node1)
          } else {
            # if node1 is not in node2's sg
            if (!grepl(node1, df$sg[index2])) {
              df$sg[index2] = paste(df$sg[index2], node1, sep = ",")
            }
          }
        # if node1 not in dataframe
        } else if (!(node1 %in% df$node)) {
          f = df$father[index2]
          m = df$mother[index2]
          newrow <- data.frame(ped=familyid,id=ID,father=f,mother=m,sex=node1_sex,affected=0,deceased=0,twin=NA,node=node2,name=NA,dob=NA,partner=NA,sg=c(node1),ad=NA)
          ID <- ID + 1
          df<-rbind(df,newrow)
        # if node2 not in dataframe
        } else if (!(node2 %in% df$node)) {
          f = df$father[index1]
          m = df$mother[index1]
          newrow <- data.frame(ped=familyid,id=ID,father=f,mother=m,sex = 3,affected=0,deceased=0,twin=NA,node=node1,name=NA,dob=NA,partner=NA,sg=c(node2),ad=NA)
          ID <- ID + 1
          df<-rbind(df,newrow)
        # if both not in dataframe
        } else {
          newrow <- data.frame(ped=familyid,id=ID,father=NA,mother=NA,sex=node1_sex,affected=0,twin=NA,deceased=0,node=node1,name=NA,dob=NA,partner=NA,sg=c(node2),ad=NA)
          ID <- ID + 1
          df<-rbind(df,newrow)
          newrow <- data.frame(ped=familyid,id=ID,father=NA,mother=NA,sex = 3,affected=0,deceased=0,twin=NA,node=node2,name=NA,dob=NA,partner=NA,sg=c(node1),ad=NA)
          ID <- ID + 1
          df<-rbind(df,newrow)
        }
      }
      
      
  # ======================================================
  # Action: Change Node Name
  # ======================================================  
    } else if (grepl("change_",line) == TRUE) {
      print(paste("line: ",linenum, " actions:",line))
      logtext <- paste(logtext,"\n", "line: ",linenum, " actions:",line) 
      act_line <- unlist(strsplit(line," "))
      
      if (act_line[2] == "change_node_name") {
        request_node <- act_line[1]
        new_name <- act_line[3]
        # first check if this node is in dataframe
        if (request_node %in% df$node) {
          # change node's name
          # by default, df$node is Factor, to edit it, we need to change df$node to Character
          df$node <- as.character(df$node)
          node_index = which(df$node == request_node)
          df$node[node_index] <- new_name
          # change node's name in df$sg
          df$sg <- gsub (request_node, new_name, df$sg)
          #print(str(df))
        #if this node does not exist in dataframe
        } else {
          showerror <- paste("line ", linenum, " this node does not exist")
          logtext <- paste(logtext,"\n", showerror) 
          print(showerror)
          break
        }
      }
    
  # ======================================================
  # Alert user when syntax error with line #
  # ======================================================
    #alertuser error when line can not be understand
    } else{
      showerror <- paste("line ",linenum, " has syntax error")
      logtext <- paste(logtext,"\n", showerror) 
      print(showerror)
      break
    }
    #can have nore function
    
    #show the dataframe after reading eachline
    
    
    #print(df)
    
    #Set to nextline
    line<-readLines(con,n=1)
    linenum <- linenum + 1
  }
  
  writeLines(logtext,logcon)
  #print_ped(df)
  close(con)
  close(logcon)
  write.table(df,'./output/log.txt', append = TRUE, sep = "\t")
  return(df)
}

producegraph <- function(df,d,position, arg) {
  # ======================================================
  # graph
  # ======================================================
  out_jpg = './output/output.jpg'
  idc <- df$node
  #print(arg)
  if (length(arg)>0){
    if ("name" %in% arg){
      idc <- paste(idc,df$name,sep = "\n")
    }
    if ("id" %in% arg){
      idc <- paste(idc,df$id,sep = "\n")
      
    }
    if ('dob' %in% arg){
      print(df$dob)
      
      idc <- paste(idc,df$dob ,sep = "\n")
    }
    if("affect" %in% arg){
      idc <- paste(idc,ifelse(df$affected,"affected","Not affected"),sep = "\n")
      
    }
    if("ad" %in% arg){
      idc <- paste(idc,df$ad,sep = "\n")
      
    }
  }
   # print(idc)
  d <- 2.1 - d
  
  diseasenumber <- length(names(df)) - 14
  if (diseasenumber == 1){
    aff <- df[,15]
  } else if (diseasenumber == 2){
    aff <- df[,15:16]
  } else if(diseasenumber == 3){
    aff <- df[,15:17]
  }else if (diseasenumber == 4){
    aff <- df[,15:18]
  } else{
    aff <- df$affected
  }
  print(diseasenumber)

  
  out <- tryCatch({
    pedAll <- pedigree(id = df$id, dadid = df$father, momid = df$mother, 
                       sex = df$sex, famid = df$ped, affected = as.matrix(aff), status = df$deceased)
    ped1basic <- pedAll["1"]
    
    
    jpeg(out_jpg)#,res = 100 , pointsize = 0.1)
    plot(ped1basic,cex = d, id = idc)# ,col = ifelse(df$affected == 0 , 1, 2))
    pedigree.legend(ped1basic,location = position, radius = d/5)
    dev.off()
    
  },
  error = function(cond){
    print(cond)
   
    write.table(c(cond[1]),'./output/log.txt', append = TRUE)
    pedAll <- pedigree(id = df$id, dadid = df$father, momid = df$mother, 
                       sex = df$sex, famid = df$ped, affected = as.matrix(aff), status = df$deceased)
    ped1basic <- pedAll["1"]
    write.table(c(cond[1],cond[2]),'./output/log.txt', append = TRUE)
    jpeg(out_jpg)#,res = 100 , pointsize = 0.1)
    plot(ped1basic,cex = d, id = idc)# ,col = ifelse(df$affected == 0 , 1, 2))
    pedigree.legend(ped1basic,location = position, radius = d/5)
    dev.off()
  },
  warning  = function(cond){
    msg <- 'warnning occur'
    print(msg)
    

    write.table(c(cond[1]),'./output/log.txt', append = TRUE)
    
    pedAll <- pedigree(id = df$id, dadid = df$father, momid = df$mother, 
                       sex = df$sex, famid = df$ped, affected = as.matrix(aff), status = df$deceased)
    ped1basic <- pedAll["1"]
    jpeg(out_jpg)#,res = 100 , pointsize = 0.1)
    plot(ped1basic,cex = d, id = idc)# ,col = ifelse(df$affected == 0 , 1, 2))
    pedigree.legend(ped1basic,location = position, radius = d/5)
    dev.off()
  },
  finallly = {
    # write.csv(df[,1:10],'./output/log.txt')
    dev.off}
  )
  
  
  
  return(out_jpg)
}

# #reading each line of the text file 
# con <- file(args[1],"r")
# 
# producePED(con)
# 
# #close connection to the file

# ======================================================
# paraphase finished
# ======================================================


# # ======================================================
# # graph
# # ======================================================
# 
# out <- tryCatch({
#   pedAll <- pedigree(id = df$id, dadid = df$father, momid = df$mother, 
#                      sex = df$sex, famid = df$ped, affected = df$affected, status = df$deceased)
#   ped1basic <- pedAll["1"]
#   jpeg('./output/output.jpg')#,res = 100 , pointsize = 0.1)
#   plot(ped1basic,cex = 0.9, id = df$node,col = ifelse(!(is.na(df$affected)) && (df$affected == 0) , 'red', 'black'))
#   pedigree.legend(ped1basic,location = 'bottomright', radius = 0.05)
#   dev.off()
#   
#   },
#   error = function(cond){
#     msg <-  "fatal error occur, plz remember what has been input and contact the dev group"
#     print(msg)
#     print(cond)
#     pedAll <- pedigree(id = df$id, dadid = df$father, momid = df$mother, 
#                        sex = df$sex, famid = df$ped)
#     ped1basic <- pedAll["1"]
#     jpeg('./output/output.jpg')#,res = 100 , pointsize = 0.1)
#     plot(ped1basic,cex = 0.9, id = df$node,col = ifelse(!(is.na(df$affected)) && (df$affected == 0) , 'red', 'black'))
#     pedigree.legend(ped1basic,location = 'bottomright', radius = 0.05)
#     dev.off()
#   },
#   warning  = function(cond){
#     msg <- 'warnning occur'
#     print(msg)
#     print(cond)
#     pedAll <- pedigree(id = df$id, dadid = df$father, momid = df$mother, 
#                        sex = df$sex, famid = df$ped, status = df$deceased)
#     ped1basic <- pedAll["1"]
#     jpeg('./output/output.jpg')#,res = 100 , pointsize = 0.1)
#     plot(ped1basic,cex = 0.9, id = df$node,col = ifelse(!(is.na(df$affected)) && (df$affected == 0) , 'red', 'black'))
#     pedigree.legend(ped1basic,location = 'bottomright', radius = 0.05)
#     dev.off()
#   },
#   finallly = {
#     write.csv(df[,1:10],'./output/log.txt')
#     dev.off}
# )










