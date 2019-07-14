library(readxl)
library(ggplot2)
library(imputeTS)

fill.replace <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  return(x)
}

# Reading data from xl file and adding a hours timestamp
Data_19B <- read_excel('/home/pennyworth/Documents/Bus/Data/19B-Data_raw.xlsx')
Data_19B$...286=NULL
Data_19B$Date...287=NULL
Data_19B$`Weekday ID`=NULL
Data_19B$`Trip Index`=NULL
Data_19B$Starting_Time=NULL
colnames(Data_19B)=c(1:282)
Data_19B$day<- as.POSIXlt(Data_19B$'1')$wday
Data_19B$td=(Data_19B$`2`)/3600

# Noting down the max and min values for td
tdmax<-max(Data_19B$td)
tdmin<-min(Data_19B$td)

#Removing outliers:
for (col in 3:282){
  Data_19B[[col]]=fill.replace(Data_19B[[col]])
}

# Noting down the max and min values for every segment
Mat=matrix(NA, nrow = 2, ncol = 281)
Minmax=as.data.frame(Mat)  
for (segno in 1:280){
  Minmax[segno]=c(max(Data_19B[segno+2]),min(Data_19B[segno+2]))
}
write.csv(Minmax,file = "Minmax.csv",row.names = FALSE,col.names = FALSE)

# Normalizing data be segments
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dfNorm <- as.data.frame(lapply(Data_19B[3:282], normalize))

#Adding necessary columns
dfNorm$day<- normalize(as.POSIXlt(Data_19B$'1')$wday)
dfNorm$td=normalize((Data_19B$`2`)/3600)
dfNorm$date<-Data_19B$`1`

# Getting the window and bin size and calculating the NA tensor shape (Hrs) (4am to 10pm)
Bin_size=1
st=4
et=22

# Creating the forexport df to export binned data to pandas
forexport <- data.frame(matrix(ncol = 282, nrow = 0))
x <- c(1:282)
colnames(forexport) <- x

#Looping over dataframes corresponding to each date
start <- min(as.Date(dfNorm$date))
end <- max(as.Date(dfNorm$date))
dedate <- start
nos=0
while (dedate <= end)
{
  print(paste("Sebsetting df for the date:",dedate))
  dayno<-as.POSIXlt(dedate)$wday
  #1<-sunday
  #6<-saturday
  print(dayno)
  if (dayno %in% c(1:5)){
    dayno<-1
  }
  else{
    dayno<-0
  }
  print(dayno)
    
  
  # Creating an NA matrix for binning
  Mat=matrix(NA, nrow = (et-st)/Bin_size, ncol = 282)
  binned=as.data.frame(Mat)
  
  #Selecting data for a date
  sel<-subset(dfNorm,as.Date(dfNorm$date)==dedate)
  
  # Making the time bins and normalizing them
  timo<-seq(4,22,Bin_size)
    timo<-((timo - tdmin) / (tdmax - tdmin))
  if (dim(sel)[1]!=0){
    nos=nos+1
  }
  for (ti in 1:(length(timo)-1)){
    print(paste("ti=",ti))
    
    selb<-as.data.frame(subset(sel,td<timo[ti+1] & td>=timo[ti]))
    #View(selb)
    selb<-selb[,1:ncol(selb)-1]
    if (dim(selb)[1]!=0){
      mean_vector=as.vector(colMeans(selb))
      binned[ti,]=mean_vector
      binned[ti,281]=dayno
    }
    #To put in normallized day and time even if segment data is unavailable
    else{
#      print("Filling only week and day..")
      binned[ti,281]=dayno
#      print(timo[ti+1])
      binned[ti,282]=(timo[ti+1]+timo[ti])/2
    }
  }
  
  # Adding the binned df to the binned forexport df
  colnames(binned)<-c(1:282)
  forexport<-rbind(forexport,binned)
  dedate <- dedate + 1
}

#Fill in empty values using Moving average
for (col in 1:(ncol(forexport)-2)){
  original<-forexport[[col]]
  forexport[[col]]<-na.ma(original, k = 3, weighting = "simple")
}

print(paste("The percentage of Na values is:",sum(is.na(forexport$`1`))/length(forexport$`1`)*100))

write.csv(forexport,file = "from_R_19B_1Hr_n&f.csv",row.names = FALSE,col.names = FALSE)
