

Impact of Storm Events on U.S. Population Health and Economy
================================================================================
<br/>

### Synopsis
This data analysis was conducted to determine the most damaging and lethal weather
phenomena in the United States.  Results from the analysis can lead to better
government services and preventative measures to reduce property damage and fatalities.
The NOAA Storm Database was utilized for the analysis that contained storm data from 
1950 to 2011.  Two questions were asked for the analysis and are listed below.  

1. Across the United States, which types of events are most harmful with respect to population health?  
2. Across the United States, which types of events have the greatest economic consequences?  
<br/>

### Data Processing
The NOAA Storm Database was loaded into R.  Additionally, libraries were loaded to ensure
all R tools were available for analysis.


```r
library(dplyr)
library(ggplot2)
library(sqldf)
library(devtools)
library(maps)
library(mapproj)
library(DataCombine)
library(data.table)
library(stringr)

storm<-read.csv("repdata_data_StormData.csv",header=TRUE,sep=",")
```
<br/>
General exploratory data analysis revealed an erroneous entry for flood property damage in California during January 2006.  Also, the latitude and longitude information was not formatted in degrees/decimal degrees.  The code below was written to correct these problems.


```r
# Change the latitude and longitude format
storm<-filter(storm,REFNUM!=605943) 
Lat1<-data.frame(as.numeric(substr(storm$LATITUDE,start=1,stop=2)))
Lat2<-data.frame(as.numeric(substr(storm$LATITUDE,start=3,stop=4))/60)
LATITUDE<-droplevels(Lat1+Lat2)
colnames(LATITUDE)<-"LATITUDE"
Long1<-data.frame(as.numeric(substr(storm$LONGITUDE,start=1,stop=nchar(storm$LONGITUDE)-2))*-1)
Long2<-data.frame(as.numeric(substr(storm$LONGITUDE,start=nchar(storm$LONGITUDE)-1,stop=nchar(storm$LONGITUDE)))/-60)
LONGITUDE<-droplevels(Long1+Long2)
colnames(LONGITUDE)<-"LONGITUDE"
storm15<-subset(storm,select=-c(LATITUDE,LONGITUDE))
storm15<-cbind(storm15,LATITUDE,LONGITUDE)
rm("Lat1","Lat2","LATITUDE","Long1","Long2","LONGITUDE") 
```
<br/>
Further examination of the data revealed that the property damage information was inputed with a three digit number in the *PROPDMG* variable followed by an alphabetical character (*PROPDMGEXP*) signifying the magnitude of the number, i.e., 1.55B for $1,550,000,000.  The following code combines the *PROPDMG* and 
*PROPDMGEXP* variable to get the numerical property damage value.


```r
#multiply property damage value by its magnitude
Replaces<-data.frame(from=c("K","k", "M","m","B","b",""), to=c(1000,1000,1e+06,1e+06,1e+09,1e+09,0))
storm16<- FindReplace(data = storm15, Var = "PROPDMGEXP", replaceData = Replaces,from = "from", to = "to")
storm16$PROPDMGEXP<-as.numeric(storm16$PROPDMGEXP)
storm16<-mutate(storm16,PROPDMG=PROPDMG*PROPDMGEXP)
```
<br/>
More cleanup was performed on the dataset by filtering out the storm events that had no property damage and no fatalities.  The resulting dataset was called *storm19*. 


```r
# filter dataframe for property damage or fatality events.
storm19<-filter(storm16,PROPDMG>0 | FATALITIES>0)
```
<br/>
During the exploratory phase two additional discrepancies were discovered.  First, the event *THUNDERSTORM WIND* was synonymous with the variable *TSTM WIND*.  To correct the problem the *TSTM WIND* was renamed *THUNDERSTORM WIND*.   The second discrepancy encountered was the date variable was not in a "date" format that R could use.  Therefore the variable was manipulated and a new variable, *YEAR*, was created to examine the data by year.  Also, data before the year 1993 was discarded due to the fact that there were inconsistencies between storm event types.


```r
# Combine 2 TSTM WIND and THUNDERSTORM WIND columns into one
Replaces<-data.frame(from="TSTM WIND", to="THUNDERSTORM WIND")
storm19 <- FindReplace(data = storm19, Var = "EVTYPE", replaceData = Replaces,from = "from", to = "to")
# Add YEAR column
storm19$BGN_DATE<-as.character(storm19$BGN_DATE)
storm19<-mutate(storm19,"DATE"=str_trim(substr(BGN_DATE,start=1,stop=nchar(BGN_DATE)-7)))
storm19<-mutate(storm19,"DATE"=as.Date(BGN_DATE, format="%m/%d/%Y"),YEAR=year(DATE)) 
# Filter out years before 1993 due to inconsistent data for event types
storm19<-filter(storm19,YEAR>1992)
```
<br/>
The data was filtered to focus the analysis on the lower 48 states and unnecessary dataframes were removed.


```r
# Select only lower 48 states
storm19<-filter(storm19,LATITUDE>=24.396308,LATITUDE<=49.384358,LONGITUDE>=-124.848974,LONGITUDE<=-66.885444)

# Remove unneccessary dataframes
rm("storm15","storm16")
```
<br/>
The data was transformed further to create a bubble chart depicting property damage and fatalities.  Specifically, storm19 dataset was grouped by event type, property damage and fatalities were summed and a count of storm events was conducted.  The code below shows the transformation.


```r
# Group data by event type
storm_wkng44<- # Use this for determing the top 3 events
    storm19%>%
    group_by(EVTYPE)%>%
    summarize(PROPDMG=sum(PROPDMG)/1e9,FATALITIES=sum(FATALITIES),COUNT=n())%>%
    arrange(desc(PROPDMG))

# filter out property damage less than $250 million
storm_wkng45<-filter(storm_wkng44,PROPDMG>.25)
```
<br/>
Property damage due to thunderstorm wind/tornadoes and floods was investigated.  The complete storm dataset, *storm19*, was filtered for thunderstorm wind and tornado then grouped by state in order to see the property damage across the US.  The resulting dataset was used for a graphical depiction of property damage on a US map shown in the results section.  The same approach was used for the flood data.  The code to create the thunderstorm wind/tornado and flood dataset is illustrated below. 


```r
# Property damage due to tornadoes and thunderstorm wind
storm_wkng3<-
    storm19%>%
    filter(EVTYPE=="TORNADO" | EVTYPE=="THUNDERSTORM WIND")%>%
    group_by(STATE)%>%
    summarize(PROPDMG=sum(PROPDMG)/1e9,FATALITIES=sum(FATALITIES))%>%
    arrange(desc(PROPDMG))

#Property damage due to floods
storm_wkng4<-
    storm19%>%
    group_by(STATE,EVTYPE)%>%
    filter(EVTYPE=="FLOOD")%>%
    summarize(PROPDMG=sum(PROPDMG)/1e9,FATALITIES=sum(FATALITIES))
```
<br/>
The remaining code shows how to create the figures shown in the results section.


```r
#****************************Bubble chart***************************************

# create the bubble chart
ggplot(storm_wkng45, aes(x=FATALITIES, y=PROPDMG, size=COUNT/1000, label=EVTYPE),guide=FALSE)+
    geom_point(colour="white", fill="#cc0000", shape=21)+ scale_size_area(max_size = 18,
    name="number of events ('000s)")+
    scale_x_continuous(name="Fatalities",limit=c(0,2000))+
    scale_y_continuous(name="Property Damage ($ billions)") +
    geom_text(size=4,hjust=-.2,vjust=-.15)+labs(title="Storm Events 1993-2011")+
    theme_bw(base_family = "Helvetica")

#**************Create a map of tornado/thunderstorm wind data ******************

# Load state data to make US charts
states<-map_data("state")
states<-rename(states,STATE=region)

# Merge tornado/thunderstorm wind data with the state data
storm_wkng3$STATE<-tolower(state.name[match(storm_wkng3$STATE,state.abb)])
storm_wkng3<-merge(states,storm_wkng3,sort=FALSE, by="STATE")
storm_wkng3<-arrange(storm_wkng3,order)

# Load map data
us <- map_data("state")
us <- fortify(us, region="region")

# Theme_map
devtools::source_gist("33baa3a79c5cfef0f6df")

# Plot data
gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region, group=group),
                    fill="#ffffff", color="black", size=.25)  #"#7f7f7f"
gg<-gg+geom_polygon(data=filter(storm_wkng3),
                    aes(x=long,y=lat,group=group,fill=PROPDMG))+
    scale_fill_gradient(low = "#EEEEEE", high = "#830202","Property Damage ($ bil)")
gg <- gg + coord_map("albers", lat0=39, lat1=45)
gg <- gg + theme_map()
gg <- gg + theme(legend.position="right",legend.title=element_text(size=9),plot.title = element_text(size = 14))+labs(title="Property Damage due to Tornadoes and Thunderstorm Wind\n1993-2011")
gg

#**************************Create a map of flood data **************************
# NOTE: Tennessee had the highest flood damage due to record 2010 and 2011 floods

# Merge flood data with the state data
storm_wkng4$STATE<-tolower(state.name[match(storm_wkng4$STATE,state.abb)])
storm_wkng4<-merge(states,storm_wkng4,sort=FALSE, by="STATE")
storm_wkng4<-arrange(storm_wkng4,order)

# Plot data
gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region, group=group),
                    fill="#ffffff", color="black", size=.25)  
gg<-gg+geom_polygon(data=storm_wkng4,aes(x=long,y=lat,group=group,fill=PROPDMG))+
    scale_fill_gradient(low = "#EEEEEE", high = "#830202","Property Damage ($ bil)")
gg <- gg + coord_map("albers", lat0=39, lat1=45)
gg <- gg + theme_map()
gg <- gg + theme(legend.position="right",legend.title=element_text(size=9),plot.title = element_text(size = 14))+labs(title="Property Damage due to Floods\n1993-2011")
gg
```
<br/>
### Results
A bubble chart was created to determine which storm events were most damaging and fatal.  The chart was filtered for property damage greater than $250 million for years between 1993 and 2011 (figure 1).  Tornadoes are the most fatal and cause the greatest economic impact in the US.  However, floods and thunderstorm wind are also worth examining further due to the high fatality counts and property damage.

<img src="figure/unnamed-chunk-10-1.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" style="display: block; margin: auto;" />

<center>figure 1.</center>
<br/>
<br/>
The results from the bubble chart focused the data analysis on tornadoes, thunderstorm wind and floods.  To determine which areas of the US were most affected by the storm events a heat map was created shown in figure 2 and figure 3.  Tornadoes and thunderstorm wind have significant impact on the southeast and midwest whereas floods affect California the southeast and the northeast.
<br/>
<br/>
<br/>
![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

<center>figure 2.</center>
<br/>
<br/>
<br/>
<br/>
![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

<center>figure 3.</center>
<br/>
<br/>






