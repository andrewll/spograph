spograph<-function(){
  
  #################################
  ##
  ##  The purpose of this script is to find the projects associated with each spo PRD project
  ##  The primary input file is generated from the sproc derareport.report.ProjectContentReport
  ##
  #################################
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  library(stringi)
  library(igraph)
  
  # basic set up clear all existing variables 
  rm(list = ls(all=T))
  
  #setup EG variable for lookup
  EG<-c("O365 SharePoint")
  egpropertygroup<-c("BOSG - SPO-S", "FAST Search", "BOSG - Federal SharePoint")
  azureclusternames<-c("PrdApp01","PrdStr01")
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  
  ##define the deloyments file
  file1 <- "DeliveryProjectContentReport.csv"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = c("","NA"), stringsAsFactors = TRUE)
  
  ##convert fields to proper format
  pids$ActualCloseDate <- as.Date(pids$ActualCloseDate, format = "%m/%d/%Y")
  pids$CreationDate <- as.Date(pids$CreationDate, format = "%m/%d/%Y")
  pids$ActualDockMax <- as.Date(pids$ActualDockMax, format = "%m/%d/%Y")
  pids$ProjectDelivered <- as.Date(pids$ProjectDelivered, format = "%m/%d/%Y")
  pids$Tags<-stri_replace_all_fixed(pids$Tags," ","")  ##remove all whitespace
  pids$Tags<-stri_trans_toupper(pids$Tags)             ##change strings to upper case to simplify grep later
   
  ##filter out the cancelled
  pids2<-pids[which(pids$ProjectStatusName!="Cancelled"),]  ##this is a jumping point for creating new data frames
  
  ##filter on Shared Networking
  pids5<-pids2[which(pids2$EngineeringGroup=="Shared Networking"),]
  
  ##filter out PIDs for time frame desired
  pids7<-pids5[which(pids5$CreationDate > '2016-08-01'),]
  
  ##filter out only the MOR PIDs
  morpids<-pids7[grepl("^Fabric \\| M",pids7$ProjectTitle),]
  
  ##change date field names for morpids, removing periods
  mornames <- gsub("^","mor",names(morpids))
  colnames(morpids) <- c(mornames)
  
  ##extract list of active SPO PRD PIDs
  pids9<-pids2[which(pids2$DeploymentClass=="New Deployment"),]
  pids11<-pids9[which(pids9$EngineeringGroup %in% EG),]
  pids13<-pids11[which(is.na(pids11$ProjectDelivered)),]
  pids15<-pids13[which(pids13$ProjectCategory=="PRD"),]
  
  ##isolate the MOR and Network tags for each O365 PID
  pids16<-pids15[which(!is.na(pids15$Tags)),]
  pids18<-pids16[grepl("MORPID",pids16$Tags),]
  pids20<-mutate(pids18, assocmorpid = "", assocnetworkpid = "", assocazurepid = "")
  pids20$assocmorpid<-stri_sub(unlist(stri_match_first_regex(pids20$Tags, "MORPID=[0-9]{6}")),from=8)  ##MOR PID tag
  pids20$assocnetworkpid<-stri_sub(unlist(stri_match_first_regex(pids20$Tags, ",NWPID=[0-9]{6}")),from=8) ##Network PID tag
  pids22<-pids20[which(!is.na(pids20$assocmorpid)),]
  
  ##merge O365 PIDs with morpids
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.EngineeringGroup
  ,p.DataCenter
  ,p.DemandID
  ,p.ProjectTitle
  ,p.CreationDate
  ,p.RequestedDelivery
  ,p.CommittedDelivery
  ,p.EstimatedRTEGDate
  ,p.ProjectDelivered
  ,p.ActualDockMax
  ,p.assocmorpid
  ,p.assocnetworkpid
  ,w.morDeliveryNumber
  ,w.morCreationDate
  ,w.morActualDockMax
  ,w.morProjectDelivered
  ,w.morCommittedDelivery
  ,w.morEstimatedRTEGDate
  ,w.morRequestedDelivery
  
  FROM pids22 p
  LEFT JOIN morpids w
  ON p.assocmorpid = w.morDeliveryNumber"
  
  mergedpids <- sqldf(SQLQuery1)
  
  ##filter out NA
  mergedpids3<-mergedpids[which(!is.na(mergedpids$morDeliveryNumber)),]
  
  
  ##generate datasheets by Month_Created
  ##write.csv(mergedpids9,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/spograph_by_MonthCreated_summarized.csv")
  
  ##create data frame of Network PIDs
  networkpids<-pids2[which(pids2$ProjectCategory=="Network"),]
  networkpids3<-networkpids[which(networkpids$DeploymentClass=="New Deployment"),]
  networkpids5<-networkpids3[which(is.na(networkpids3$ProjectDelivered)),]
  networkpids7<-networkpids5[which(networkpids5$PropertyGroup %in% egpropertygroup),]
  
  ##change date field names for network pids, removing periods
  netnames <- gsub("^","net",names(networkpids7))
  colnames(networkpids7) <- c(netnames)
  
  ##merge O365 PIDs with netpids
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.EngineeringGroup
  ,d.DataCenter
  ,d.DemandID
  ,d.ProjectTitle
  ,d.CreationDate
  ,d.RequestedDelivery
  ,d.CommittedDelivery
  ,d.EstimatedRTEGDate
  ,d.ProjectDelivered
  ,d.ActualDockMax
  ,d.assocmorpid
  ,d.assocnetworkpid
  ,d.morDeliveryNumber
  ,d.morCreationDate
  ,d.morActualDockMax
  ,d.morProjectDelivered
  ,d.morRequestedDelivery
  ,d.morCommittedDelivery
  ,e.netDeliveryNumber
  ,e.netEngineeringGroup
  ,e.netCreationDate
  ,e.netActualDockMax
  ,e.netProjectDelivered
  ,e.netCommittedDelivery
  ,e.netEstimatedRTEGDate

  FROM mergedpids3 d
  LEFT JOIN networkpids7 e
  ON d.assocnetworkpid = e.netDeliveryNumber"
  
  mergedpids5 <- sqldf(SQLQuery1)
  
  #dataframe for Azure Prod01 and Stor01 PIDs
  azurepids1<-pids2[which(pids2$EngineeringGroup=="Azure"),]
  azurepids2<-azurepids1[which(azurepids1$ProjectCategory=="PRD"),]
  azurepids3<-azurepids2[which(azurepids2$DeploymentClass=="New Deployment"),]
  ##azurepids3<-azurepids1[grepl(paste(azureclusternames,collapse = "|"),azurepids1$ClusterName),]  not sure how to use this data frame
  azurepids5<-azurepids3[!grepl("PrdApp01-02",azurepids3$ClusterName),]  ##remove any row with -02 in the clustername
  azurepids6<-azurepids5[!grepl("PrdApp01-03",azurepids5$ClusterName),]  ##remove any row with -03 in the clustername
  azurepids7<-azurepids6[!grepl("PrdApp01-04",azurepids6$ClusterName),]  ##remove any row with -04 in the clustername
  azurepids8<-azurepids7[!grepl("PrdApp01-05",azurepids7$ClusterName),]  ##remove any row with -05 in the clustername
  azurepids9<-azurepids8[grepl("PrdApp01",azurepids8$ClusterName),]
  azurepids10<-azurepids3[grepl("PrdStr01",azurepids3$ClusterName),]
  
  #modify column names for Azure App01 data frame
  azureapp01names <- gsub("^","azureapp01",names(azurepids9))
  colnames(azurepids9) <- c(azureapp01names)
  azurestr01names <- gsub("^","azurestr01",names(azurepids10))
  colnames(azurepids10) <- c(azurestr01names)
  
  #merge Azure  App01 PIDs into main data frame
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.ProjectTitle
  ,d.DemandID
  ,e.azurestr01ClusterName
  ,e.azurestr01DeliveryNumber
  ,d.assocmorpid
  ,d.assocnetworkpid
  ,d.morDeliveryNumber
  ,d.netDeliveryNumber
  ,d.DataCenter
  ,e.azurestr01DataCenter


  FROM mergedpids5 d
  LEFT JOIN azurepids10 e
  ON d.DataCenter = e.azurestr01DataCenter"
  
  mergedpids7 <- sqldf(SQLQuery1)
  
  
  #merge Azure  Str01 PIDs into main data frame
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.DataCenter
  ,e.azureapp01DataCenter
  ,e.azureapp01DeliveryNumber
  ,e.azureapp01ClusterName

  FROM mergedpids5 d
  LEFT JOIN azurepids9 e
  ON d.DataCenter = e.azureapp01DataCenter"
  
  mergedpids9 <- sqldf(SQLQuery1)
  
  ##mergedpids7 links the PRD to the MOR, Network, and Str01
  ##mergedpids9 links the PRD to App01
  
  ##link the PRD pairs
  

  
  #generate data frame for graph
  ##start outer for loop 
  ##on row, read PRD1-Net1, add to DF
  ##on row, read Net1-MOR1, add to DF
  ##on row, read PRD1-STR01, add to DF
  ##on row, read PRD1-APP01, add to DF
  ##on row, read PRD1-PRD2, add to DF
  ##next row
  mygraphdata<-data.frame(stringsAsFactors = default.stringsAsFactors())
  for(i in 1:nrow(mergedpids11)){
    
  }
  
  
  
  
  
  
  
  
}
