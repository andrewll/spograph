spograph2<-function(){
  
  
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
  
  #setup variables
  EG<-c("O365 SharePoint")
  egpropertygroup<-c("BOSG - SPO-S", "FAST Search", "BOSG - Federal SharePoint")
  azureclusternames<-c("PrdApp01","PrdStr01")
  azurestr01color<-c("#0000FF") ##blue
  prdcolor<-c("#FFFF00") ##yellow
  networkcolor<-c("#800000") ##maroon
  
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
  
  ##filter out only the MOR PIDs
  morpids<-pids2[grepl("^Fabric \\| M",pids2$ProjectTitle),]
  
  ##change date field names for morpids, removing periods
  mornames <- gsub("^","mor",names(morpids))
  colnames(morpids) <- c(mornames)
  
  ##extract list of active SPO PRD PIDs
  pids9<-pids2[which(pids2$DeploymentClass=="New Deployment"),]
  pids11<-pids9[which(pids9$EngineeringGroup %in% EG),]
  pids13<-pids11[which(pids11$CreationDate > '2017-05-01'),]
  pids15<-pids13[which(pids13$ProjectCategory=="PRD"),]
  
  ##isolate the MOR and Network tags for each O365 PID
  pids16<-pids15[which(!is.na(pids15$Tags)),]
  pids18<-pids16[grepl("MORPID",pids16$Tags),]
  pids20<-mutate(pids18, assocmorpid = "", assocnetworkpid = "", assocazurepid = "")
  pids20$assocmorpid<-stri_sub(unlist(stri_match_first_regex(pids20$Tags, "MORPID=[0-9]{6}")),from=8)  ##MOR PID tag
  pids20$assocnetworkpid<-stri_sub(unlist(stri_match_first_regex(pids20$Tags, ",NWPID=[0-9]{6}")),from=8) ##Network PID tag
  pids22<-pids20[which(!is.na(pids20$assocmorpid)),]
  
  ##create dataframe of Network PIDs with RTEG dates
  data_networkpids<-pids22$assocnetworkpid
  data_networkpids3<-as.integer(data_networkpids[which(!is.na(data_networkpids))])
  data_networkpids5<-pids2[which(pids2$DeliveryNumber %in% data_networkpids3),]
  
  ##change date field names for morpids, removing periods
  netnames <- gsub("^","net",names(data_networkpids5))
  colnames(data_networkpids5) <- c(netnames)
  
  
  ##merge mor pids dataframe with pids22 to get MOR rteg dates
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.DemandID
  ,d.ProjectTitle
  ,d.EngineeringGroup
  ,d.DataCenter
  ,d.EstimatedRTEGDate
  ,d.assocnetworkpid
  ,d.assocmorpid
  ,e.morDeliveryNumber
  ,e.morEstimatedRTEGDate
  ,e.morActualCloseDate

  
  FROM pids22 d
  LEFT JOIN morpids e
  ON d.assocmorpid = e.morDeliveryNumber"
  
  pids24 <- sqldf(SQLQuery1)
  
  ##merge network pids dataframe with pids24 to get network rteg dates
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.DemandID
  ,d.ProjectTitle
  ,d.EngineeringGroup
  ,d.DataCenter
  ,d.EstimatedRTEGDate
  ,d.assocmorpid
  ,d.morDeliveryNumber
  ,d.morEstimatedRTEGDate
  ,d.morActualCloseDate
  ,d.assocnetworkpid
  ,e.netDeliveryNumber
  ,e.netEstimatedRTEGDate
  ,e.netActualCloseDate
  
  FROM pids24 d
  LEFT JOIN data_networkpids5 e
  ON d.assocnetworkpid = e.netDeliveryNumber"
  
  pids26 <- sqldf(SQLQuery1)
  
  
  ##create main dataframe with PRD-to-Network PID link
  mygraphdata<-subset(pids26,select = c("DeliveryNumber","assocnetworkpid","EstimatedRTEGDate","netEstimatedRTEGDate"))
  mygraphdata<-mutate(mygraphdata, sourcecolor = prdcolor)
  
  #fix column names
  mygraphnames <- gsub("assocnetworkpid","assocDeliveryNumber",names(mygraphdata))
  colnames(mygraphdata)<-c(mygraphnames)
  mygraphnames <- gsub("^EstimatedRTEGDate","SourceEstimatedRTEGDate",names(mygraphdata))
  colnames(mygraphdata)<-c(mygraphnames)
  mygraphnames <- gsub("^netEstimatedRTEGDate","DestinationEstimatedRTEGDate",names(mygraphdata))
  colnames(mygraphdata)<-c(mygraphnames)
  
  ##create dataframe with Network-to-MOR PID link, and append to main dataframe
  networkandmor<-subset(pids26,select = c("assocnetworkpid","assocmorpid", "netEstimatedRTEGDate","morEstimatedRTEGDate"), stringsAsFactors = default.stringsAsFactors())
  networkandmor<-mutate(networkandmor, sourcecolor=networkcolor)
  
  ##fix column names for new dataframe to match the column names in main dataframe
  networkandmornames<-gsub("assocnetworkpid","DeliveryNumber",names(networkandmor)) 
  colnames(networkandmor)<-c(networkandmornames)
  networkandmornames<-gsub("assocmorpid","assocDeliveryNumber",names(networkandmor))
  colnames(networkandmor)<-c(networkandmornames)
  networkandmornames <- gsub("^netEstimatedRTEGDate","SourceEstimatedRTEGDate",names(mygraphdata))
  colnames(networkandmor)<-c(networkandmornames)
  networkandmornames <- gsub("^morEstimatedRTEGDate","DestinationEstimatedRTEGDate",names(mygraphdata))
  colnames(networkandmor)<-c(networkandmornames)
  
  ##create main dataframe with PRD-Network, Network-MOR links
  mygraphdata2<-rbind(mygraphdata,networkandmor)
  
  ##create dataframe with PRD-to-MOR link
  prdandmor <- subset(pids26, select = c("DeliveryNumber","assocmorpid","EstimatedRTEGDate","morEstimatedRTEGDate"))
  prdandmor <- mutate(prdandmor, sourcecolor = prdcolor)
  
  ##fix column names for new dataframe to match the column names in main dataframe
  prdandmornames<-gsub("assocmorpid","assocDeliveryNumber",names(prdandmor)) 
  colnames(prdandmor)<-c(prdandmornames)
  prdandmornames <- gsub("^EstimatedRTEGDate","SourceEstimatedRTEGDate",names(mygraphdata))
  colnames(prdandmor)<-c(prdandmornames)
  prdandmornames <- gsub("^morEstimatedRTEGDate","DestinationEstimatedRTEGDate",names(mygraphdata))
  colnames(prdandmor)<-c(prdandmornames)
  

  ##create main dataframe with PRD-Network, Network-MOR, PRD-MOR links
  mygraphdata3<-rbind(mygraphdata2,prdandmor)
  
  ##create a dataframe PRD with DemandID info, with method of linking the pair PRD
  prdpids<-arrange(subset(pids15,select = c("DeliveryNumber","DemandID","ProjectTitle", "EstimatedRTEGDate")),DemandID)
  prdpids$DemandID<-as.character(prdpids$DemandID)  ##convert Factors into chr
  prdpids$DemandID<-as.integer(prdpids$DemandID)    ##then convert chr into int
  prdpids$ProjectTitle<-stri_replace_all_fixed(prdpids$ProjectTitle," ","")
  prdpids$ProjectTitle<-stri_trans_toupper(prdpids$ProjectTitle)
  prdpids3<-mutate(prdpids,PairName = "")
  prdpids3$PairName<-gsub("^.*CAGG","",prdpids3$ProjectTitle)
  
  ##create secondary dataframe of PRD
  prdpids5<-prdpids3
  
  ##replace column names
  prdpid5names <- gsub("^","assoc",names(prdpids5))
  colnames(prdpids5) <- c(prdpid5names)
  
  ##merge the two prd dataframes (match on DID)
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.DemandID
  ,d.ProjectTitle
  ,d.PairName
  ,d.EstimatedRTEGDate
  ,e.assocDeliveryNumber
  ,e.assocDemandID
  ,e.assocProjectTitle
  ,e.assocPairName
  ,e.assocEstimatedRTEGDate

  FROM prdpids3 d
  LEFT JOIN prdpids5 e
  ON d.DemandID = e.assocDemandID"
  
  prdpids9 <- sqldf(SQLQuery1)

  ##clean up prdpids, remove rows where PID is linked to itself
  prdpids11<-prdpids9[which(prdpids9$DeliveryNumber!=prdpids9$assocDeliveryNumber),]
  prdpids13<-subset(prdpids11,select = c("DeliveryNumber","assocDeliveryNumber","EstimatedRTEGDate","assocEstimatedRTEGDate"))
  prdpids14<-mutate(prdpids13, sourcecolor = prdcolor)
  
  ##clean up prdpids, remove rows where PID is linked in reverse from the row above it
  oddrows<-seq(1,dim(prdpids14)[1],2)
  cleanupprdpids<-data.frame()
  cleanupprdpids<-mutate(cleanupprdpids, DeliveryNumber="",assocDeliveryNumber = "",sourcecolor = "#FFFF00") 
  cleanupprdpids<-data.frame(prdpids14[oddrows,])
  prdpids14<-cleanupprdpids
  
  #fix column names for prdpids
  
  
  ##create main dataframe with PRD-Network, Network-MOR, PRD-MOR, PRD-PRD(cluster pair matched on DID) links
  mygraphdata5<-rbind(mygraphdata3,prdpids14)
  
  ##create another dataframe for PRD-to-PRD linking (match not on DID)
  prdpids15<-subset(prdpids13,select = c("DeliveryNumber"))
  prdpids17<-as.character(prdpids15$DeliveryNumber)
  prdpids19<-prdpids3[which(!prdpids3$DeliveryNumber %in% prdpids17),]
  prdpids21<-prdpids19
  
  ##replace column names
  prdpid21names <- gsub("^","assoc",names(prdpids21))
  colnames(prdpids21) <- c(prdpid21names)
  
  ##merge the two new dataframes (match on cluster pair name)
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.DemandID
  ,d.ProjectTitle
  ,d.PairName
  ,e.assocDeliveryNumber
  ,e.assocDemandID
  ,e.assocProjectTitle
  ,e.assocPairName
  
  FROM prdpids19 d
  LEFT JOIN prdpids21 e
  ON d.PairName = e.assocPairName"
  
  prdpids23 <- sqldf(SQLQuery1)
  
  ##clean up prdpids, remove rows where PID is linked to itself
  prdpids25<-prdpids23[which(prdpids23$DeliveryNumber!=prdpids23$assocDeliveryNumber),]
  prdpids27<-subset(prdpids25, select = c("DeliveryNumber","assocDeliveryNumber"))
  prdpids29<-mutate(prdpids27, sourcecolor = prdcolor)
  
  ##clean up prdpids, remove rows where PID is linked in reverse from the row above it
  oddrows<-seq(1,dim(prdpids29)[1],2)
  cleanupprdpids2<-data.frame()
  cleanupprdpids2<-mutate(cleanupprdpids, DeliveryNumber="",assocDeliveryNumber = "",sourcecolor = "#FFFF00") 
  cleanupprdpids2<-data.frame(prdpids29[oddrows,])
  prdpids29<-cleanupprdpids2
  
  ##create main dataframe with PRD-Network, Network-MOR, PRD-MOR, PRD-PRD(matched on DID, on pair name) links
  mygraphdata7<-rbind(mygraphdata5,prdpids29)
  
  ##PRD-to-PRD linking, case of no match on DID or PairName
  ##look at data on prdpids23
  
  ##Create dataframes for Azure App01 and Str01
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
  
  ##change column names for app01 and str01 dataframe
  azurepids9names <- gsub("^","assoc",names(azurepids9))
  colnames(azurepids9) <- c(azurepids9names)
  azurepids10names <- gsub("^","assoc",names(azurepids10))
  colnames(azurepids10) <- c(azurepids10names)
  
  ##merge PRD dataframe with Str01 dataframe, match on DC name
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.ProjectTitle
  ,e.assocClusterName
  ,e.assocDeliveryNumber
  ,d.DataCenter
  ,e.assocDataCenter

  FROM pids15 d
  LEFT JOIN azurepids10 e
  ON d.DataCenter = e.assocDataCenter"
  
  prdandstr01_data1 <- sqldf(SQLQuery1)
  
  ##cleanup of str01 dataframe
  prdandstr01_data3 <- subset(prdandstr01_data1, select = c("DeliveryNumber","assocDeliveryNumber"))
  prdandstr01_data5 <- prdandstr01_data3[which(!is.na(prdandstr01_data3$assocDeliveryNumber)),]  #remove rows with NA
  prdandstr01_data5$DeliveryNumber<-as.character(prdandstr01_data5$DeliveryNumber)  ##format correctly before merging
  prdandstr01_data5$assocDeliveryNumber<-as.character(prdandstr01_data5$assocDeliveryNumber)  ##format correctly before merging
  prdandstr01_data7<-mutate(prdandstr01_data5, sourcecolor = azurestr01color)
  
  ##create main dataframe with PRD-Network, Network-MOR, PRD-MOR, PRD-PRD(matched on DID, on pair name), PRD-STRO1 links
  mygraphdata9<-rbind(mygraphdata7,prdandstr01_data7)
  
  ##remove the assoc column labels in the STR01 dataframe so it becomes the source in the next join
  azurepids10names <- gsub("assoc","",names(azurepids10))
  colnames(azurepids10) <- c(azurepids10names)
  
  ##create new STR01 dataframe of just the ones linked to SPO
  ##we don't want to use the list of all STR01 because it creates orphaned nodes
  azurepids12<-azurepids10[which(azurepids10$DeliveryNumber %in% prdandstr01_data7$assocDeliveryNumber),]
  
  ##merge PRD dataframe with App01 dataframe, match on DC name
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.ProjectTitle
  ,e.assocClusterName
  ,e.assocDeliveryNumber
  ,d.DataCenter
  ,e.assocDataCenter
  
  FROM azurepids12 d
  LEFT JOIN azurepids9 e
  ON d.DataCenter = e.assocDataCenter"
  
  prdandapp01_data1 <- sqldf(SQLQuery1)
  
  ##cleanup of app01 dataframe
  prdandapp01_data3 <- subset(prdandapp01_data1, select = c("DeliveryNumber","assocDeliveryNumber"))
  prdandapp01_data5 <- prdandapp01_data3[which(!is.na(prdandapp01_data3$assocDeliveryNumber)),]  #remove rows with NA
  prdandapp01_data5$DeliveryNumber<-as.character(prdandapp01_data5$DeliveryNumber)  ##format correctly before merging
  prdandapp01_data5$assocDeliveryNumber<-as.character(prdandapp01_data5$assocDeliveryNumber)  ##format correctly before merging
  prdandapp01_data7 <- mutate(prdandapp01_data5, sourcecolor = azurestr01color)
  
  ##create main dataframe with PRD->Network, Network->MOR, PRD->MOR, PRD->PRD(matched on DID, on pair name), PRD->STR01, STR01->app01 links
  mygraphdata11<-rbind(mygraphdata9,prdandapp01_data7)
  
  ##final cleanup of mygraphdata
  mygraphdata13<-mygraphdata11[which(!is.na(mygraphdata11$assocDeliveryNumber)),]
  mygraphdata15<-mygraphdata13[which(!is.na(mygraphdata13$DeliveryNumber)),]
  
  ##write output to a csv
  write.csv(mygraphdata15,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/spographdata.csv")
  
  
  ##plot it
  myfirstnetwork<-graph.data.frame(mygraphdata15,directed = FALSE)
  plot(myfirstnetwork)
  
}