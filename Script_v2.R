## LEBAF Data Reporting Workflow ##
## Created for Lake Erie Basin Assesment Framework ## 
## Created by Gabriel Watson 10.18.22 ## 
## Pulls LEBAF downloaded data from WaterReporter and Threshold data ## 
## from googlesheets to generate a series of report ready charts ## 
## This represents a first pass for review on Wednesday 10.26.22 w/ LEBAF ## 
## Goals 
##    - Create set of charts accross all parameters for Huron River ## 
##    - Evaluate different charting options 
##    - Evaluate pitfalls for incorporating rollup workflows 


## TO DO 
## Get data from water reporter 
## Import data from googlesheets 
## create charts 
## export into file structure 

library(tibble)
library(gtable)
library(gt)
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(googlesheets4)
library(dplyr)
library(plyr)
library(lubridate)
library(sumtable)

### Fetching Data Source Data from Google Sheets ### 
gs4_deauth()
SheetURL <- "https://docs.google.com/spreadsheets/d/1zrSc_Cd2-V1k73lE3TrqX-QqYJkt117AMAb-O-kuhsA"

TempWarm <- read_sheet(SheetURL, sheet = "TempWarm")%>%
           mutate(Month = ymd(Month))

TempCold <- read_sheet(SheetURL, sheet = "TempCold")%>%
            mutate(Month = ymd(Month))

DOThreshold <- read_sheet(SheetURL, sheet = "DO")

CondThreshold <- read_sheet(SheetURL, sheet = "Conductivity")

## Importing Water Quality Data from HURON ### 
HuronRaw <- read.csv("Data/WaterReporterData2022/Huron_10_20_2022.csv")

## Cleaning Huron Data ## 
## Changing annoying column stuff ## 
## Notes 
##  - Conductivity is in uS/cm - microsiemens per centimeter
##  - Missing percent saturation data 
## Calculating TDS ##
## Calculating Salinity ## 
## Calculating Chloride ## 
## Turning date into date format 
HuronCleaned <- HuronRaw %>%
                dplyr::rename("Dissolved Oxygen" = Dissolved.oxygen..DO...p.3546.,
                       "Conductivity" = Conductivity..p.3545.,
                       "Water Temperature" = Temperature..water..p.3547.,
                       "pH" = pH..p.3544.)%>%
                select(c("collection_date","Dissolved Oxygen","Conductivity","Water Temperature","pH","station_id","station_name"))%>%
                mutate(TDS = Conductivity*.55)%>%
                mutate(Salinity = ((Conductivity / 10000)^1.0878)*.4665)%>%
                mutate(Chloride = Conductivity * 4.928)%>%
                mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
                mutate(`Dissolved Oxygen` = ifelse(`Dissolved Oxygen` > 25, NA,`Dissolved Oxygen`))
            
#### VIZUALIZATIONS #### 

## TABLE ## 
## Calculate table of max min and mean for all variables ##
# Function for generating data table
TableMaker <- function(inputDF,station)
{
  df <- inputDF %>%
    filter(station_name == station)%>%
    select(-c("collection_date","station_id","station_name"))
  
  Mean <- sapply(df, mean, na.rm=TRUE)
  Median <- sapply(df, median, na.rm=TRUE)
  suppressWarnings(Minimum <- sapply(df, min, na.rm=TRUE))
  suppressWarnings(Maximum <- sapply(df, max, na.rm=TRUE))
  
  #Binding and rounding to 2 digits
  df <- round(cbind(Mean,Median,Minimum,Maximum),2)
  
  # Turning infintes to NA
  is.na(df) <- sapply(df, is.infinite)
  # Turning NAs to -
  df[is.na(df)] = "-"
  return(df)
}


## Table Loop ##
## TO DO: make this a function and loop for all stations in a group 10.20.22 
TableTest <- data.frame(TableMaker(HuronCleaned,"N. Territorial Rd."))%>%
             tibble::rownames_to_column(., "Parameter")%>%
             gt()%>%
             tab_source_note(source_note = "Data from Huron River Watershed Council")%>%
             tab_header(title = "N. Territorial Rd. Water Quality Summary Statistics",
                        subtitle = paste(min(HuronCleaned$collection_date), " to ",max(HuronCleaned$collection_date)))%>%
             tab_style(
             style = list(
               cell_text(style = "italic")),
               locations = cells_body(
               columns = Parameter))%>%
              tab_style(
                style = list(
                  cell_text(weight = "bold")),
                locations = cells_column_labels(columns = everything())
                )

### END TABLES ### 

## Box and Whisker ## 
## TO DO: Create loop for all stations and all parameters ## 
BoxPlotMaker <- function(df,inStation_name, inParameter)
    {
    ChartData <- df %>%
                 filter(station_name == inStation_name)%>%
                 select(c("station_name",inParameter))%>%
                 dplyr::rename("Value" = inParameter)
    
   plot <- ggplot() + 
           geom_boxplot(data = ChartData, aes(y = Value)) + 
           scale_x_discrete() +
           labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", inParameter),
                subtitle = paste("Data from:", min(df$collection_date), "to",max(df$collection_date)) ,
           y = inParameter)+
          theme_minimal()
    
    return(plot)
}

BoxPlotMaker(HuronCleaned,"Shetland Dr.", "pH")


## END BOX AND WHISKER ## 

## Temperature Chart ## 
TempChartMaker <- function(df, inStation_name, inTresholds)
{
  
  ChartData <- df %>%
    filter(station_name == inStation_name)%>%
    filter(collection_date > ymd("2022-03-01"))%>%
    filter(collection_date < ymd("2022-11-01")) 

plot <-  ggplot()+
    geom_point(data = ChartData, aes(x=collection_date, y =`Water Temperature`), shape = 21, fill = "#b3b3b3", size = 3)+
    theme_minimal()+
    geom_line(data = inTresholds, aes(x = Month, y = UpperBound,color = "#fc090a"))+
    geom_line(data = inTresholds, aes(x = Month, y = LowerBound, color = "#0533ff"))+
    geom_ribbon(data = inTresholds, aes(x = Month, ymin=LowerBound,ymax=UpperBound), fill="#1aaf54", alpha=0.25)+
    scale_color_manual(name = "",
                     breaks=c('Within Threshold*'),
                     values=c('Within Threshold*'='#1aaf54'))+
    xlab("")+
    labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Water Temperature - C"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "*Threshold generated from LEBAF standards")

  return(plot)
}

TempChartMaker(HuronCleaned, "Chalmers", TempCold)

### END TEMP CHART ### 
## Threshold Charts ##

## Dissolved Oxygen ## 
DOChartMaker <- function(df, inStation_name, inStreamType, ThresholdData)
  {
  ThresholdValue <- ThresholdData %>%
                    filter(StreamType == inStreamType)%>%
                    pull(Value)
  ChartData <- df %>%
    filter(station_name == inStation_name)%>%
    filter(collection_date > ymd("2022-03-01"))%>%
    filter(collection_date < ymd("2022-11-01"))%>%
    mutate(Color = ifelse(`Dissolved Oxygen` > ThresholdValue, "Above","Below"))
  
plot <-  ggplot(data = ChartData, aes(x=collection_date, y = `Dissolved Oxygen`, color = factor(Color)))+
    geom_point(size = 3)+
    geom_hline(yintercept=ThresholdValue, linetype="dashed", color = "Black")+
    theme_minimal()+
    xlab("")+
    ylab("Dissolved Oxygen - mg/L")+
    scale_color_manual(name = 'Thresholds', values = c("#1aaf54", "#f53e46"))+
  labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Dissolved Oxygen - mg/L"),
       subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
       caption = "Threshold generated from LEBAF standards")

  return(plot)
}

DOChartMaker(HuronCleaned, "N. Territorial Rd.", "Cold",DOThreshold)


### END DISSOLVED OXYGEN ### 


## Conductivity ## 
CondChartMaker <- function(df, inStation_name,ThresholdData)
   {
   ThresholdValueUpper <- ThresholdData %>%
                          filter(Threshold == "High")%>%
                          pull(Value)
   
   ThresholdValueLower <- ThresholdData %>%
                          filter(Threshold == "Low")%>%
                          pull(Value)
   ChartData <- df %>%
     filter(station_name == inStation_name)%>%
    #filter(collection_date > ymd("2022-03-01"))%>%
     #filter(collection_date < ymd("2022-11-01"))%>%
     mutate(Color = "Good")%>%
     mutate(Color = ifelse(Conductivity > ThresholdValueUpper, "Poor",Color))%>%
     mutate(Color = ifelse(Conductivity < ThresholdValueLower, "Poor",Color))
  print(ChartData)
   plot <-  ggplot(data = ChartData, aes(x=collection_date, y = Conductivity, color = factor(Color)))+
     geom_point(size = 3)+
     geom_hline(yintercept=ThresholdValueUpper, linetype="dashed", color = "Black")+
     geom_hline(yintercept=ThresholdValueLower, linetype="dashed", color = "Black")+
     theme_minimal()+
     xlab("")+
     ylab("Conductivity - uS/cm")+
     scale_color_manual(name = 'Thresholds', values = c("#1aaf54","#f53e46"))+
     labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Conductivity - uS/cm"),
          subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
          caption = "Threshold generated from LEBAF standards")
  
  
   return(plot)
  }

CondChartMaker(HuronCleaned, "N. Territorial Rd.", CondThreshold)



### !!!! ### 





