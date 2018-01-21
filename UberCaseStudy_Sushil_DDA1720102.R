## Packages and Libraris
install.packages("stringi")
install.packages("gridExtra")
library(dplyr)
library(stringi)
library(ggplot2)


############   Data Load      ###############

uberdata<-read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)

###############   Data Cleaning  ################
# Making  date formats consistent(from "/" to "-)
  uberdata$Request.timestamp<-stri_replace_all(uberdata$Request.timestamp,"-",fixed = "/")
  uberdata$Drop.timestamp<-stri_replace_all(uberdata$Drop.timestamp,"-",fixed = "/")
 
# Derived new column for keeping correct Request date time {Intension is to not disturb existing column}
    uberdata<-uberdata%>%mutate(RequestDatetime=as.POSIXct(Request.timestamp, format = "%d-%m-%Y %H:%M"))
  
# Derived new column for keeping correct Drop date time
    uberdata<-uberdata%>%mutate(DropDatetime=as.POSIXct(uberdata$Drop.timestamp, format = "%d-%m-%Y %H:%M"))
                              
#######  Data Quality Issues Check #################
    
   #a)If Trip completed but driverId is null
   filter(uberdata,uberdata$Status=="Trip Completed" & uberdata$Driver.id=="NA")
    
   #b)If trip completed but Drop.timestamp is null
   filter(uberdata,uberdata$Status=="Trip Completed" & as.integer(uberdata$DropDatetime)=="NA")
   
   #c)If trip completed but Request.timestamp is null
   filter(uberdata,uberdata$Status=="Trip Completed" & as.integer(uberdata$RequestDatetime)=="NA")
    
   #d)If requester Id is null
    filter(uberdata,uberdata$Request.id=="NA")
    
    #e)If Pick Point is null
    filter(uberdata,uberdata$Pickup.point=="NA")
    
    #f)If "No Cars Available" and driverId is not null
    filter(uberdata,uberdata$Status=="No Cars Available" & uberdata$Driver.id!="NA")
    
    #g)Compare date on derived column and existing column
    filter(uberdata,as.POSIXct(uberdata$Request.timestamp,format="%d-%m-%Y %H:%M")!=as.POSIXct(uberdata$RequestDatetime))
    filter(uberdata,as.POSIXct(uberdata$Drop.timestamp,format="%d-%m-%Y %H:%M")!=as.POSIXct(uberdata$DropDatetime))
    
    #h.Check request count on different dates and cross verify from xlsx
    uberdata%>%group_by(Reqdate=substring(RequestDatetime,1,10))%>%summarise(count=n())

########Derived Matrics###############  
#1. Derived new column for keeping Request Date and time on seperate column{Split date and time and store on new column ReqDate and ReqTime}
    #Extract Date
    uberdata$ReqDate<-substring(uberdata$RequestDatetime,1,10)
    #uberdata$ReqTime<-substring(uberdata$RequestDatetime,12,13)
    #Extract hours
    uberdata$ReqTime<-format(as.POSIXct(uberdata$RequestDatetime,format="%Y-%m-%d %H:%M:%S"),"%H")
    
# 2.Derived new column for keeping Drop Date and time on seperate column{Split date and time and store on new column DropDate and DropTime}
    #Extract Date
    uberdata$DropDate<-substring(uberdata$DropDatetime,1,10)
    #uberdata$DropTime<-substring(uberdata$DropDatetime,12,13)
    #Extract hours
    uberdata$DropTime<-format(as.POSIXct(uberdata$DropDatetime,format="%Y-%m-%d %H:%M:%S"),"%H")                    
    
# 3. Derived column for Request timeslot Using Request Hour to create buckets of timeslots(Here is different time slot considering Hours)
       #Early morning    5 to 8 am
       #morning          8 to 12 am
       #Afternoon        12 pm to 17 pm
       #Early evening    17 to 20 pm
       #Evening          20 pm to 21 pm
       #Night            21 pm to 24 
       #Late Night       00 am to 4
      
  #This Derived matrics is for Segmented Analysis      
    uberdata$ReqSlot<-ifelse((as.numeric(uberdata$ReqTime)>=05 & as.numeric(uberdata$ReqTime)<08), "Early Morning", 
                      ifelse((as.numeric(uberdata$ReqTime)>=08 & as.numeric(uberdata$ReqTime)<12), "Morning", 
                      ifelse((as.numeric(uberdata$ReqTime)>=12 & as.numeric(uberdata$ReqTime)<17), "Afternoon",
                      ifelse((as.numeric(uberdata$ReqTime)>=17 & as.numeric(uberdata$ReqTime)<20), "Early Evening",
                      ifelse((as.numeric(uberdata$ReqTime)>=20 & as.numeric(uberdata$ReqTime)<21), "Evening",
                      ifelse((as.numeric(uberdata$ReqTime)>=21 & as.numeric(uberdata$ReqTime)<=24), "Night",
                      ifelse((as.numeric(uberdata$ReqTime)>=00 & as.numeric(uberdata$ReqTime)<=04), "Late Night", 
                      uberdata$ReqTime == "NA")))))))
       
  #5.Derived column for Gap and Supply Analysis, 
        #Demand-Total cab request whether those have completed or cancelled or no car available will be the Demand
        #Supply -Total completed request{Trip completed} are the Supply
        #So Adding new colunmn Supply, it will be "Supply" if trip has completed and "Gap" if it has not.
        uberdata$Supply<-ifelse(uberdata$Status=="Trip Completed","Supply","Gap")
        
#############    Plotting  #########################################
 
    
  ##############   Analysis on Cancellation Trip Problem  #################### 
 
  #Looking into data, it observed that, Trip cancellation is a major problem for Uber so we filtered out Cancellation trip and performing analysis
  
  # Trip Cancellation data, segmentation on Date
  ggplot(subset(uberdata,uberdata$Status=="Cancelled"),aes(x=ReqDate,fill=Status)) + geom_bar(position = "dodge")+xlab("Trip Date") + ylab("Trip Request")+ ggtitle("Trip Request of Cancelled Trip against Trip Date") + geom_text(stat = "count",aes(label=abs(..count..)))
  
  # Looking into the data, most cancellation happened on date 13/07/2017, 
  # Trip Cancellation data on 13/07/2017 date has segmented on Time Slot{Performed more Granular level Segmented Analysis at different time slots on particular Date 13/07/2017}
  ggplot(subset(uberdata,Status=="Cancelled" & ReqDate=="2016-07-13"),aes(ReqSlot,fill=Status))+geom_histogram(stat = "count") +xlab("Time Slot") + ylab("Trip Request") + ggtitle("Trip Request of Cancelled Trip against Time Slot on 13/07/2017") + geom_text(stat = "count",aes(label=abs(..count..)))
  
  # Looking into the data, most cancellation happened on date 13/07/2017 and at Early morning Time slot    
  # Most Trip Cancelled on 13/07/2017 and at Early morning Time Slot.So adding one more dimension attribute for more detail level of Analysis
  #There are 2 ways of ride on this data,(Airport to City and City to Airport) so for identifying problem, it requires to add one more dimension attribute on this analysis 
  ggplot(subset(uberdata,Status=="Cancelled" & ReqDate=="2016-07-13" & ReqSlot %in% c("Early Morning")),aes(Pickup.point,fill=Status))+geom_histogram(stat = "count") +xlab("Time Slot") + ylab("Trip Request") + ggtitle("Trip Request of Cancelled Trip against Pickup Point on 13/07/2017 and Time Slot 'Early Morning'") + geom_text(stat = "count",aes(label=abs(..count..)))        
  # So Trip Cancellation problem persists on trip which starts from City to Airport at Ealry morning and morning time(5 to 8)                   
  
  ##############   Analysis on 'No Cars Available' Problem  ####################
  
  #Looking into data, it observed that, "No Cars Available" is a another major problem for Uber so here we filtered out "No Cars Available" trip and performing analysis
  
  # Trip "No Cars Available" data, Segmentation on Date
  ggplot(subset(uberdata,uberdata$Status=="No Cars Available"),aes(x=ReqDate,fill=Status)) + geom_bar(position = "dodge")+xlab("Trip Date") + ylab("Trip Request")+ ggtitle("Trip Request of 'No Cars Available Trip' against Trip Date") + geom_text(stat = "count",aes(label=abs(..count..)))
  
  # Looking into plot data, most 'No Cars Available' trips were on dated 15/07/2017, 
  # Trip 'No Cars Available' data on 15/07/2017 date has segmented on Time Slot{Performed more Granular level Segmented Analysis at different time slots on particular Date 15/07/2017}
  ggplot(subset(uberdata,Status=="No Cars Available" & ReqDate=="2016-07-15"),aes(ReqSlot,fill=Status))+geom_histogram(stat = "count") +xlab("Time Slot") + ylab("Trip Request") + ggtitle("Trip Request of 'No Cars Available' Trip against Time Slot on 15/07/2017") + geom_text(stat = "count",aes(label=abs(..count..)))
  
  # Looking into plot data, Most 'No Cars Available' trip happened on date 15/07/2017 and at Early Evening Time slot    
  # So adding one more dimension attribute for more detail level of Analysis
  #There are 2 ways of ride on this data,(Airport to City and City to Airport) so for identifying problem, it requires to add one more dimension attribute(PickupPoint) on this analysis 
  ggplot(subset(uberdata,Status=="No Cars Available" & ReqDate=="2016-07-15" & ReqSlot %in% c("Early Evening")),aes(Pickup.point,fill=Status))+geom_histogram(stat = "count") +xlab("Time Slot") + ylab("Trip Request") + ggtitle("Trip Request of 'No Cars Available' Trip against Pickup Point on 15/07/2017 and Time Slot 'Early Evening'") + geom_text(stat = "count",aes(label=abs(..count..)))        
  # So 'No Cars Available' Trip problem on trip which starts from Airport to City at Ealry Evening (17 to 20 pm) 
  
  ################### Supporting Analysis ######################
  
  #Identify the most problematic types of requests (city to airport / airport to city etc.).Cross check on above data
    ggplot(uberdata,aes(x=Pickup.point,fill=Status)) + geom_bar(position = "stack")+xlab("Pick Point") + ylab("Trip Request") + ggtitle("Trip Status at different Pickup Point") + geom_text(stat = "count",aes(label=abs(..count..)),position = "stack")
  
  #Identify the most problematic types of requests (city to airport / airport to city etc.) on the time slots (early mornings, late evenings etc.).
    ggplot(subset(uberdata),aes(x=ReqSlot,fill=Status)) + geom_bar(position = "stack")+xlab("Time Slot") + ylab("Trip Request") + ggtitle("Trip Request on different Time Slot") + geom_text(stat = "count",aes(label=abs(..count..)),position = "stack")
     
    ######################Gap and Supply Analysis #######################
  
  # Find the time slots when the highest gap exists(Gap=No Supply(Cancellation + No Cars Available),Supply=Trip Completed)
  ggplot(uberdata,aes(x=ReqSlot,fill=Supply)) + geom_bar(position = "dodge")+xlab("Time Slot") + ylab("Trip Request") + ggtitle("Supply and Gap on different Time Slot") + geom_text(stat = "count",aes(label=abs(..count..)))
      
 # Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
  #City-Airport
  ggplot(subset(uberdata,uberdata$Pickup.point=="City"),aes(x=ReqSlot,fill=Supply)) + geom_bar(position = "dodge")+xlab("Time Slot") + ylab("Trip Request") + ggtitle("Supply and Gap on different Time Slot from City to Airport") + geom_text(stat = "count",aes(label=abs(..count..)))
        
  #Airport-City
  ggplot(subset(uberdata,uberdata$Pickup.point=="Airport"),aes(x=ReqSlot,fill=Supply)) + geom_bar(position = "dodge")+xlab("Time Slot") + ylab("Trip Request") + ggtitle("Supply and Gap on different Time Slot from Airport to City") + geom_text(stat = "count",aes(label=abs(..count..)))