library(stringr)
library(RCurl)
## Loading required package: bitops
library(XML)
library(plyr)
library(xml2)

#input
inputBacteria <- c("Akkermansia", "Blastocatellaceae", "Flavobacteriia")
inputDisease <- c("leukaemia", "T2D", "Crohn's disease", "ulcerative colitis")

getQueryTerms <- function(inputList1,inputList2){
  
  #finding the no of elements in both the input files
  lengthOfinput1 <- length(inputList1)
  lengthOfinput2 <- length(inputList2)
  
  #Checking if the input is valid or not 
  if(lengthOfinput1 == 0 || lengthOfinput2 == 0){
    
    message("Please enter valid input as either one or both the inputs have no values in it")
  }
  
  #creating an empty vector to store the query terms 
  myQuery <- vector(mode = "character")
  
  #Looping through elements of list1
  for (i in inputList1) {
    
    tempQuery <- paste(i,inputDisease[1:length(inputList2)],sep = " AND ")
    tempQuery <- as.character(tempQuery)
    tempQuery <- gsub(" ", "+", tempQuery, fixed = TRUE)
    myQuery <-append(myQuery,tempQuery)
    
    
  }
  
  return(myQuery)
}

getDataFromPubMed <- function(query){
  
  api_key = NULL
  #Creating Url
  myPubmedURL <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?", 
                       "db=pubmed&term=", query, "&usehistory=y", sep = "")
  
  if (!is.null(api_key)) {
    myPubmedURL <- paste(myPubmedURL, "&api_key=", api_key, sep = "")
  }
  
  #Connecting to Pubmed through E-Utils
  pubMedconnect <- url(myPubmedURL, open = "rb", encoding = "UTF8")
  
  #Collecting XML file
  pubmedXML <- readLines(pubMedconnect, warn = FALSE, encoding = "UTF8")
  pubmedXML <- paste(pubmedXML, collapse = "")
  #closing the connection
  on.exit(close(pubMedconnect))
  
  #Parsing through the xml file generated in the above step
  #try(pubEsearch <- xmlTreeParse(pubmedXML_list, asText = TRUE),silent = T)
  #pubEsearch <- xmlTreeParse(pubmedXML_list, asText = TRUE)
  pubEsearch <- read_xml(pubmedXML)
  
  #Get the number of hit for the search query
  #pubCount <- as.numeric(xmlValue(pubEsearch[["doc"]][["eSearchResult"]][["Count"]]))
  countNodes <- xml_find_all(pubEsearch,xpath = ".//Count")
  pubCount <- as.numeric(xml_text(countNodes[[1]]))
  #Get the WebEnv-string which contains all the info of the query search
  #webEvnirnmnt <- xmlValue(pubEsearch[["doc"]][["eSearchResult"]][["WebEnv"]])
  webEnvNodes <- xml_find_all(pubEsearch,xpath = ".//WebEnv")
  webEnvirnmnt <- xml_text(webEnvNodes)
  #Idlist <- xmlValue(pubEsearch[["doc"]][["eSearchResult"]][["IdList"]])
  # Show how many articles are present
  cat("Getting data from ", pubCount, "articles \n")
  
  #Checks if there are any hits
  if(pubCount == 0){
    
    cat("No records available for",query)
    myRecordList <- list(pubCount)
    
  } else{
    
    RetStart <- 0
    #If we want to collect more than 15 articles in one step we should change this value
    RetMax <- 200
    
    
    myUrl <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?", 
                   "db=pubmed&WebEnv=",webEnvirnmnt,
                   "&query_key=1&retmode=xml&retstart=",
                   RetStart, "&retmax=", RetMax, sep = "")
    
    #connecting to get the pubmed records data  
    recordsConnect <- url(myUrl, open = "rb", encoding = "UTF8")#closing do
    recordsXML <- readLines(recordsConnect, warn = FALSE, encoding = "UTF8")
    recordsXML <- paste(recordsXML, collapse = "")
    on.exit(close(recordsConnect))
    
    #recordsData <- xmlTreeParse(recordsXML, useInternalNodes = TRUE)
    recordsData <- read_xml(recordsXML)
    
    #getting the article titles     
    
    #titles <- xpathSApply(recordsData,"//PubmedArticle/MedlineCitation/Article/ArticleTitle",
    #                      xmlValue)
    
    titleNodes <- xml_find_all(recordsData,
                               "//PubmedArticle/MedlineCitation/Article/ArticleTitle")
    titles <- xml_text(titleNodes)
    
    #getting abstract(in some abstracts there is copy right information)
    
    #abstract_unrefined <- xpathSApply(recordsData,
    #                                  "//PubmedArticle/MedlineCitation/Article/Abstract",
    #                                 xmlValue)
    
    abstractNodes <- xml_find_all(recordsData,
                                  "//PubmedArticle/MedlineCitation/Article/Abstract")
    abstractUnrefined <- xml_text(abstractNodes)
    
    #creating Null vector to store refined abstract
    abstract <- rep(NA,length(abstractUnrefined))
    
    #Looping through unrefined abstract and processing it 
    for(i in 1:length(abstractNodes)){
      
      tempNode <- xml_find_all(abstractNodes[i],"AbstractText")
      tempText <- xml_text(tempNode)
      abstract[i] <- paste0(tempText,collapse = "\n",sep = "  ")
    }
    
    #Getting PMID
    
    #pmIdList <- xpathSApply(recordsData,
    #                  "//PubmedArticle/PubmedData/ArticleIdList/ArticleId[@IdType='pubmed']",
    #                 xmlValue)
    
    pmIdNodes <- xml_find_all(recordsData,
                              "//PubmedArticle/PubmedData/ArticleIdList/ArticleId[@IdType='pubmed']                         ")
    pmIdList <- xml_text(pmIdNodes)
    
    
    #Getting doiList
    
    #doiList <- xpathSApply(recordsData,
    # "//PubmedArticle/PubmedData/ArticleIdList/ArticleId[@IdType='doi']", xmlValue)   
    
    doiNodes <- xml_find_all(recordsData,
                             "//PubmedArticle/PubmedData/ArticleIdList/ArticleId[@IdType='doi']")
    doiList <- xml_text(doiNodes)
    
    myRecordList <- list(pubCount,titles,abstract,pmIdList,doiList)
  }
  
  return(myRecordList)
}

#####################################################################################
#Takes in aquery term and preprocess it to have a proper file name and give a list 
#which the full path and the file name as well 
# Usage : myQUeryList <- getFileName(query)
#####################################################################################

getFileName <- function(queryInput){
  
  fileName <- queryInput
  #splits the input based on " AND " with spaces 
  #fileName <- as.vector(str_split_fixed(fileName,pattern = " AND ",n = Inf))
  #pastes the words together by joining them with + sign
  #fileName <- paste(fileName[1],fileName[2],sep = "+")
  #replaces the spaces with underscore in the file name
  #fileName <- gsub(" ","_",fileName)
  #creates a full path for the file in txt format
  myFullFileName <- paste(outputDir,"/",fileName,".txt",sep = "")
  myFileNameList <- c(fileName,myFullFileName)
  return(myFileNameList)
}

#####################################################################################
#Takes in a list of queries and prints the record info collected from PUBMED into 
#txt files with appropriate names 
# Usage : myQUeryList <- printRecordsToFiles(queryList)
#####################################################################################
printRecordsToFiles <- function(queryList){
  
  lengthOfQueryList <- length(queryList)
  
  for (i in 1:lengthOfQueryList) {
    
    inputFileList <- getFileName(queryList[i]) #queryList[i]
    
    fileNameShort <- inputFileList[1]
    #get filename for each query
    queryFileName <- inputFileList[2]
    #open a file connection to write into the file
    queryFileConn <- file(queryFileName,"w")
    
    tempRecordList <- getDataFromPubMed(query = queryList[i])
    
    if(tempRecordList[[1]]== 0){
      
      cat("\n")
      write(paste("No records found for :",queryList[i]),queryFileConn,append = T)
      
    }else{
      
      
      cat("Printing",tempRecordList[[1]],"records into the file",fileNameShort,".txt","\n")
      
      #looping through the records and write it to txt files
      for (i in 1:tempRecordList[[1]]) {
        
        write(paste("Paper Title:",tempRecordList[[2]][i]),queryFileConn,append = T)
        #write(paste("PaperAbstract:",tempRecordList[[3]][i]),queryFileConn,append = T)
        cat("PaperAbstract:",tempRecordList[[3]][i],"\n",file = queryFileConn,append = T)
        write(paste("Paper PMID:",tempRecordList[[4]][i]),queryFileConn,append = T)
        write(paste("Paper PMID or DOI:",tempRecordList[[5]][i]),queryFileConn,append = T)
        cat("\n\n",file = queryFileConn,append = T)
      }    
    }
    close(queryFileConn)
  }
}

#creates the ouput directory and tells us if the directory is already esisting
mainDir <- getwd()          
subDir <- "OUTPUT1"

outputDir <- file.path(mainDir, subDir)

if (!dir.exists(outputDir)){
  
  dir.create(outputDir)
  
} else {
  
  print("Dir already exists!")
}

queryTerms <- getQueryTerms(inputBacteria,inputDisease)
printRecordsToFiles(queryTerms)

###A quick look at some files generated
fileConn1 <-file("OUTPUT1/Akkermansia+AND+Crohn's+disease.txt","r")
readLines(fileConn1, n = 25)

