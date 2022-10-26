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
library(stringr)
library(webshot2)

### Fetching Data Source Data from Google Sheets ### 
gs4_deauth()
SheetURL <- "https://docs.google.com/spreadsheets/d/1zrSc_Cd2-V1k73lE3TrqX-QqYJkt117AMAb-O-kuhsA"

### Pulling in Threshold Data ### 
DO_Cold <- read_sheet(SheetURL, sheet = "DO_Cold")
DO_Warm <- read_sheet(SheetURL, sheet = "DO_Warm")
TempWarm <- read_sheet(SheetURL, sheet = "TempWarm")%>%
           mutate(Month = ymd(Month))
TempCold <- read_sheet(SheetURL, sheet = "TempCold")%>%
            mutate(Month = ymd(Month))
CondThreshold <- read_sheet(SheetURL, sheet = "Conductivity")
pHThreshold <- read_sheet(SheetURL, sheet = "pH")
TDSThreshold <- read_sheet(SheetURL, sheet = "TDS")
ChlorideThreshold <- read_sheet(SheetURL, sheet = "Chloride")
CondReference <- read_sheet(SheetURL, sheet = "ConductivityReference")
## Creating master list of thresholds for passing into Table Maker
ThresholdList <- list(DO_Cold,DO_Warm,TempWarm,TempCold,CondThreshold,pHThreshold,TDSThreshold,ChlorideThreshold)

## Pulling in Station Reference Data ## 
HuronReference <- read_sheet(SheetURL, sheet = "HuronReference")%>%
                  select(-c(station_name))


## Importing Water Quality Data from HURON ### 
HuronRaw <- read.csv("Data/WaterReporterData2022/Huron_10_20_2022.csv")

## Importing Water Quality Data from Doan Brok ### 
DoanRaw <- read.csv("Data/WaterReporterData2022/Doan_10_26_2022")

### END DATA IMPORT ### 


### DATA CLEANING #### 

## Cleaning Huron Data ## 
## Changing annoying column stuff ## 
## Notes 
##  - Conductivity is in uS/cm - microsiemens per centimeter
##  - Missing percent saturation data 
##  - For 10.26.22 Meeting, Are these conversions correct for Conductivity?
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
                mutate(Salinity = (Conductivity^1.0878)*.4665)%>%
                mutate(Chloride = ((Conductivity / 1000) * 4.928)*100)%>%
                mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
                mutate(`Dissolved Oxygen` = ifelse(`Dissolved Oxygen` > 25, NA,`Dissolved Oxygen`))%>%
                filter(collection_date > ymd("2022-03-01"))%>%
                filter(collection_date < ymd("2022-11-01"))%>%
                left_join(HuronReference, by = "station_id")

HuronStations <- HuronCleaned%>%
                 select(station_name,station_id)%>%
                 distinct(station_name, .keep_all = TRUE)

### END DATA CLEANING ### 
            
#### VIZUALIZATIONS #### 
## TO DO: Add conductivity table 
## Make charts stylistically consistent 
## Need data on dissolved oxygen 

## TABLE ## 
## Calculate table of max min and mean for all variables ##
# Function for generating data table
TableDataMaker <- function(inputDF,station)
{
  print(inputDF)
  df <- inputDF %>%
    filter(station_name == station)%>%
    select_if(is.numeric)
  #  select_if(-c("collection_date","station_id","station_name","WaterbodyName" "StreamSize","Ecoregion","Temp"))
  
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

TableMaker <- function(df, inStation_name, inThresholds)
{
  ## Joining in Water Thresholds 
  ## TO DO: Add logic for Warm vs Cold Water
  WaterThreshold <- inThresholds[[3]]%>%
    mutate(Month = month(Month))
  
  ## Exceedence table for Water
  Water_Ex <- df %>%
    select(c(`Water Temperature`,collection_date))%>%
    mutate(Month = month(collection_date))%>%
    left_join(WaterThreshold)%>%
    mutate(Water_Ex = ifelse(`Water Temperature` > LowerBound & `Water Temperature` < UpperBound,0,1))%>%
    pull(Water_Ex)
  
  ## All other vars and pulling in Water
  ExceedTable <- df %>%
                select(-c(collection_date,station_id,station_name))%>%
                ## Temp warm vs Temp Cold logic here ## 
                 mutate(DO_Ex = ifelse(`Dissolved Oxygen` > as.numeric(inThresholds[[2]][1,2]),0,1)) %>% 
                 mutate(Cond_Ex = ifelse(Conductivity < as.numeric(inThresholds[[5]][1,2]) & Conductivity > as.numeric(inThresholds[[5]][3,2]),0,1))%>%
                 mutate(Water_Ex = Water_Ex)%>%
                 mutate(pH_Ex = ifelse(pH < as.numeric(inThresholds[[6]][1,2]) & pH > as.numeric(inThresholds[[6]][3,2]),0,1))%>%
                 mutate(TDS_Ex = ifelse(TDS >= as.numeric(inThresholds[[7]][3,2]),1,0))%>%
                 mutate(Chloride_Ex = ifelse(Chloride >= as.numeric(inThresholds[[8]][4,2]),1,0))%>%
                 mutate(Salinity_Ex = NA)%>%
                 select(c(DO_Ex,Cond_Ex,Water_Ex,pH_Ex,TDS_Ex,Salinity_Ex,Chloride_Ex))%>%
                 summarise_all(.,~sum(.x,na.rm = TRUE))
  
  ## a little cleaning 
  ExceedTable <- as.data.frame(t(ExceedTable))%>%
                 dplyr::rename(NumExceed = V1)%>%
                 mutate(PerExceed = as.character(round(NumExceed / nrow(df),3)*100))%>%
                 remove_rownames()
  
  ## Joining to other data 
  Table <- data.frame(TableDataMaker(df,inStation_name)) %>%
    tibble::rownames_to_column(., "Parameter") %>%
    mutate(`# Exceedence` = ExceedTable$NumExceed)%>%
    mutate(`% Exceedence` = ExceedTable$PerExceed)%>%
    gt()%>%
    tab_source_note(source_note = paste("Data from Huron River Watershed Council:", min(df$collection_date), " to ",max(df$collection_date)))%>%
    tab_header(title = paste(inStation_name, "Station", "Water Quality Summary Statistics"))%>%

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
  return(Table)
}

Table <- TableMaker(HuronCleaned,"Broadway St.", ThresholdList)
### END TABLES ### 

## Box and Whisker ## 
BoxPlotMaker <- function(df,inStation_name)
    {
    ChartData <- df %>%
                 filter(station_name == inStation_name)%>%
                 select_if(is.numeric)%>%
                 gather()

   plot <- ggplot() +
           geom_boxplot(data = ChartData, aes(y = value)) +
           facet_wrap(~ key, scales = "free")+
           scale_x_discrete() +
           labs(title = paste("Huron River Watershed Council:", inStation_name, "Station Summary Plots"),
                subtitle = paste("Data from:", min(df$collection_date), "to",max(df$collection_date)))+
           theme_classic()+
           ylab("")
   
    return(plot)
}

BoxPlotMaker(HuronCleaned,"Shetland Dr.")

## END BOX AND WHISKER ## 

## Temperature Chart ## 
TempChartMaker <- function(inGroup_data, inStation_name, inTresholds)
{
  
  ChartData <- inGroup_data 

plot <-  ggplot()+
  geom_line(data = inTresholds, aes(x = Month, y = UpperBound,color =  "#fc090a"))+
  geom_line(data = inTresholds, aes(x = Month, y = LowerBound, color = "#0533ff"))+
  geom_ribbon(data = inTresholds, aes(x = Month, ymin=LowerBound,ymax=UpperBound), fill="#1aaf54", alpha=0.25)+
  scale_color_manual(name = "",
                     breaks=c('Threshold'),
                     values=c('Threshold'='#1aaf54'))+
    geom_point(data = ChartData, aes(x=collection_date, y =`Water Temperature`), shape = 21, fill = "#b3b3b3", size = 3)+
    theme_classic()+
    ylab("Water Temperature C")+
    xlab("")+
    labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Water Temperature - C"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards")+
   ylim(0,max(ChartData$`Water Temperature`)*1.3)
  

  return(plot)
}

TempChartMaker(HuronCleaned, "Chalmers", TempCold)

### END TEMP CHART ### 
## Threshold Charts ##

## Dissolved Oxygen ## 
## TO DO add logic for determining warm or cold 
DOChartMaker <- function(inGroup_data, inStation_name, inThreshold_data)
  {
  ChartData <- inGroup_data %>%
    mutate(Color = as.character(inThreshold_data[1,1]))%>%
    mutate(Color = ifelse(`Dissolved Oxygen` > as.numeric(inThreshold_data[2,2]), as.character(inThreshold_data[2,1]),Color))%>% ## Good 
    filter(!is.na(`Dissolved Oxygen`))
  
  ## Getting a factor list of Threshold Colors
  ThresholdColors <- setNames(as.character(inThreshold_data$Color), inThreshold_data$Threshold)
  
  plot <-  ggplot(data = ChartData, aes(x=collection_date, y = `Dissolved Oxygen`, color = factor(Color)))+
    geom_point(size = 3)+
    geom_hline(yintercept=as.numeric(inThreshold_data[1,2]), linetype="dashed", color = as.character(inThreshold_data[1,3]))+ ## Poor
    theme_classic()+
    xlab("")+
    ylab("Dissolved Oxygen - mg/L")+
    scale_color_manual(name = 'Thresholds', values = ThresholdColors)+
    labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Dissolved Oxygen - mg/L"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards.")+
    ylim(0,max(ChartData$`Dissolved Oxygen` *1.2))
  
  return(plot)
}

DOChartMaker(HuronCleaned, "N. Territorial Rd.",DO_Warm)
### END DISSOLVED OXYGEN ### 


### Temperature Vs. Dissolved Oxygen ## 
Temp_DO_ChartMaker <- function(inGroup_data, inStation_name)
{
  ChartData <- inGroup_data %>%
    filter(station_name == inStation_name) %>% 
    filter(collection_date > ymd("2022-03-01"))%>%
    filter(collection_date < ymd("2022-11-01"))%>%
    filter(!is.na(`Water Temperature`))%>%
    filter(!is.na(`Dissolved Oxygen`))
  
plot <-  ggplot(data = ChartData, aes(x = collection_date))+
    geom_smooth(aes(y=`Water Temperature`), se = FALSE, color = "#da222b", alpha = .1)+
    geom_point(aes(y=`Water Temperature`),fill = "#da222b", size = 1.5, alpha = .75, shape = 21,color = "black")+
    geom_smooth(aes(y=`Dissolved Oxygen`), se = FALSE, color = "#0098d8", alpha = .1)+
    geom_point(aes(y=`Dissolved Oxygen`),fill = "#0098d8", size = 1.5, alpha = .75, shape = 21, color = "black")+
    theme_classic()+
  
    scale_y_continuous(
      name = "Water Temperature C",
      sec.axis = sec_axis(~., name="Dissolved Oxygen - mg/L"))+
    theme(
      axis.title.y = element_text(size=13),
      axis.text.y = element_text(color = "#da222b", size = 10),
      axis.title.y.right = element_text(size=13, vjust = 4),
      axis.text.y.right = element_text(color = "#0098d8", size = 10),
      axis.title.x = element_text(size = 13),
      axis.text.x = element_text(size = 10),
      plot.margin = unit(c(1,1,1,1),"cm"))+
      labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Temperature and Dissolved Oxygen"),
       subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)))
  return(plot)
}

Temp_DO_ChartMaker(HuronCleaned, "Broadway St.")
### End Temperature and Dissolved Oxygen  ### 

### Temperature Vs. pH ## 
## Recommend not doing this chart!! ##
Temp_pH_ChartMaker <- function(inGroup_data, inStation_name)
{
  ChartData <- inGroup_data %>%
    filter(station_name == inStation_name) %>% 
    filter(collection_date > ymd("2022-03-01"))%>%
    filter(collection_date < ymd("2022-11-01"))%>%
    filter(!is.na(`Water Temperature`))%>%
    filter(!is.na(pH))
  
  plot <-  ggplot(data = ChartData, aes(x = collection_date))+
    geom_smooth(aes(y=`Water Temperature`), se = FALSE, color = "#da222b", alpha = .1)+
    geom_point(aes(y=`Water Temperature`),fill = "#da222b", size = 1.5, alpha = .75, shape = 21,color = "black")+
    geom_smooth(aes(y=pH), se = FALSE, color = "#38761d", alpha = .1)+
    geom_point(aes(y=pH),fill = "#38761d", size = 1.5, alpha = .75, shape = 21, color = "black")+
    theme_classic()+
    
    scale_y_continuous(
      name = "Water Temperature C",
      sec.axis = sec_axis(~., name="pH"))+
    theme(
      axis.title.y = element_text(size=13),
      axis.text.y = element_text(color = "#da222b", size = 10),
      axis.title.y.right = element_text(size=13, vjust = 4),
      axis.text.y.right = element_text(color = "#38761d", size = 10),
      axis.title.x = element_text(size = 13),
      axis.text.x = element_text(size = 10),
      plot.margin = unit(c(1,1,1,1),"cm"))+
    labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Temperature and pH"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)))
  return(plot)
}

Temp_pH_ChartMaker(HuronCleaned, "Broadway St.")
### End Temperature and pH ### 


## Conductivity ## 
## FIXED ## 
CondChartMaker <- function(inGroup_data, inStation_name,inThreshold_data)
   {
  ChartData <- inGroup_data %>%
    filter(station_name == inStation_name) %>%
    filter(collection_date > ymd("2022-03-01"))%>%
    filter(collection_date < ymd("2022-11-01"))%>%
    mutate(Color = as.character(inThreshold_data[2,1]))%>%
    mutate(Color = ifelse(Conductivity > as.numeric(inThreshold_data[1,2]), as.character(inThreshold_data[1,1]),Color))%>% ## High 
    mutate(Color = ifelse(Conductivity < as.numeric(inThreshold_data[3,2]), as.character(inThreshold_data[3,1]), Color))%>% ## Low 
    filter(!is.na(Conductivity))
  
  ## Getting a factor list of Threshold Colors
  ThresholdColors <- setNames(as.character(inThreshold_data$Color), inThreshold_data$Threshold)
  
  plot <-  ggplot(data = ChartData, aes(x=collection_date, y = Conductivity, color = factor(Color)))+
    geom_point(size = 3)+
    geom_hline(yintercept=as.numeric(inThreshold_data[1,2]), linetype="dashed", color = as.character(inThreshold_data[1,3]))+ ## High
    geom_hline(yintercept=as.numeric(inThreshold_data[3,2]), linetype="dashed", color = as.character(inThreshold_data[2,3]))+ ## Low 
    theme_classic()+
    xlab("")+
    ylab("Conductivity - uS/cm")+
    scale_color_manual(name = 'Thresholds', values = ThresholdColors)+
    labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Conductivity"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards.")+
    ylim(0,max(ChartData$Conductivity *1.2))

   return(plot)
  }

## Testing 
CondChartMaker(HuronCleaned, "N. Territorial Rd.", CondThreshold)
## End Conductivity 


## Conductivity Boxplot W/ Reference and Survery Sites ## 
## TO DO: Add legened point ## 
CondBoxplotMaker <- function(inGroup_data, inStation_name, inThreshold_data)
  {
  
  ChartData <- inGroup_data %>%
                   left_join(inThreshold_data)
  
  ## Getting the correct conductivity reference - Again, I know there is a better way to do this, will improve later ## 
  CondRefBox <- ChartData %>%
                filter(StreamType == "Reference")%>%
                select(c(StreamType, x25, x50, x75, x90, x95))%>%
                distinct(.,.keep_all = TRUE)
  
  CondSurvBox <- ChartData %>%
                filter(StreamType == "Survey")%>%
                select(c(StreamType, x25, x50, x75, x90, x95))%>%
                distinct(.,.keep_all = TRUE)%>%
                rbind(CondRefBox)
Legend <- c("Reference" = "#0098d8","Survey" = "#1aaf54")

plot <-  ggplot() +
              geom_boxplot(data = CondSurvBox,  aes(x = StreamType, ymin = 400, lower = x25, middle = x50,upper = x75, ymax = x90, fill = StreamType), stat = "identity")+              
              geom_point(data = ChartData, aes(y=Conductivity, x = StreamType), size = 3, color = "Black")+
              labs(title = paste("Huron River Watershed Council:", inStation_name, "Conductivity and Control Sites"),
              subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)))+
              theme_classic()+
              ylab("Conductivity - uS/cm")+
              xlab("Stream Type")+
              scale_fill_manual(values = Legend)
  
  return(plot)
  
  
  }

CondBoxplotMaker(HuronCleaned, "Broadway St.",CondReference)


## PH ## 
pHChartMaker <- function(inGroup_data, inStation_name,inThreshold_data)
{
  
  ChartData <- inGroup_data %>%
    mutate(Color = as.character(inThreshold_data[2,1]))%>% ## Good
    mutate(Color = ifelse(pH > as.numeric(inThreshold_data[1,2]), as.character(inThreshold_data[1,1]),Color))%>% ## High 
    mutate(Color = ifelse(pH < as.numeric(inThreshold_data[3,2]), as.character(inThreshold_data[3,1]), Color))%>% ## Low 
    filter(!is.na(pH))
  
  ## Getting a factor list of Threshold Colors
  ThresholdColors <- setNames(as.character(inThreshold_data$Color), inThreshold_data$Threshold)
  
  plot <-  ggplot(data = ChartData, aes(x=collection_date, y = pH, color = factor(Color)))+
    geom_point(size = 3)+
    geom_hline(yintercept=as.numeric(inThreshold_data[1,2]), linetype="dashed", color = as.character(inThreshold_data[1,3]))+ ## High
    geom_hline(yintercept=as.numeric(inThreshold_data[3,2]), linetype="dashed", color = as.character(inThreshold_data[2,3]))+ ## Low 
    theme_classic()+
    xlab("")+
    ylab("pH")+
    scale_color_manual(name = 'Thresholds', values = ThresholdColors)+
    labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "pH"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards.")+
    ylim(0,15)
  
  return(plot)
  
}

## Testing ## 
pHChartMaker(HuronCleaned, "Shetland Dr.", pHThreshold)
## End pH ##


## TDS ## 
TDS_ChartMaker <- function(inGroup_data, inStation_name,inThreshold_data)
{

  ChartData <- inGroup_data %>%
     mutate(Color = as.character(inThreshold_data[3,1]))%>%
     mutate(Color = ifelse(TDS > as.numeric(inThreshold_data[2,2]), as.character(inThreshold_data[2,1]),Color))%>% ## chronic 
     mutate(Color = ifelse(TDS > as.numeric(inThreshold_data[1,2]), as.character(inThreshold_data[1,1]), Color))%>% ## Acute 
     filter(!is.na(TDS))


  ## Getting a factor list of Threshold Colors
  ThresholdColors <- setNames(as.character(inThreshold_data$Color), inThreshold_data$Threshold)
  
  plot <-  ggplot(data = ChartData, aes(x=collection_date, y = TDS, color = factor(Color)))+
     geom_point(size = 3)+
    geom_hline(yintercept=as.numeric(inThreshold_data[3,2]), linetype="dashed", color = as.character(inThreshold_data[3,3]))+
    geom_hline(yintercept=as.numeric(inThreshold_data[1,2]), linetype="dashed", color = as.character(inThreshold_data[1,3]))+
    theme_classic()+
   xlab("")+
  ylab("TDS")+
  scale_color_manual(name = 'Thresholds', values = ThresholdColors)+
  labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Total Dissolved Solids"),
       subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
       caption = "Threshold generated from LEBAF standards. TDS was calculated from Conductivity results: TDS = k EC (in 25 °C),")+
  ylim(0,max(ChartData$TDS *1.2))

  return(plot)
}

## Testing ## 
TDS_ChartMaker(HuronCleaned, "Broadway St.", TDSThreshold)

## End TDS ##


## Chloride ##
Chloride_ChartMaker <- function(inGroup_data, inStation_name,inThreshold_data)
{
  ChartData <- inGroup_data %>%
    mutate(Color = as.character(inThreshold_data[4,1]))%>%
    mutate(Color = ifelse(Chloride > as.numeric(inThreshold_data[3,2]), as.character(inThreshold_data[3,1]),Color))%>% ## Chronic 
    mutate(Color = ifelse(Chloride > as.numeric(inThreshold_data[2,2]), as.character(inThreshold_data[2,1]),Color))%>% ## Max
    mutate(Color = ifelse(Chloride > as.numeric(inThreshold_data[1,2]), as.character(inThreshold_data[1,1]), Color))%>% ## Acute 
    filter(!is.na(Chloride))
  
  ## Getting a factor list of Threshold Colors
  ThresholdColors <- setNames(as.character(inThreshold_data$Color), inThreshold_data$Threshold)
  
  plot <-  ggplot(data = ChartData, aes(x=collection_date, y = Chloride, color = factor(Color)))+
    geom_point(size = 3)+
    geom_hline(yintercept=as.numeric(inThreshold_data[4,2]), linetype="dashed", color = as.character(inThreshold_data[4,3]))+ ## Good
    geom_hline(yintercept=as.numeric(inThreshold_data[2,2]), linetype="dashed", color = as.character(inThreshold_data[2,3]))+ ## Chronic
    geom_hline(yintercept=as.numeric(inThreshold_data[1,2]), linetype="dashed", color = as.character(inThreshold_data[1,3]))+ ## Max
    theme_classic()+
    xlab("")+
    ylab("Chloride - mg/L")+
    scale_color_manual(name = 'Thresholds', values = ThresholdColors)+
    labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Chloride - mg/L"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards. Chloride was calculated from Conductivity results: [Cl-] = 4.928 EC")+
    ylim(0,max(ChartData$Chloride *1.2))
    
  return(plot)
}

## Testing ## 
Chloride_ChartMaker(HuronCleaned, "Broadway St.", ChlorideThreshold)

### End Chloride ###


### Salinity ### 
Salinity_ChartMaker <- function(inGroup_data, inStation_name)
{
  ChartData <- inGroup_data %>%
     filter(!is.na(Salinity))
  
  plot <-  ggplot(data = ChartData, aes(x=collection_date, y = Salinity))+
    geom_point(size = 3, color = "#3852a4")+
    theme_classic()+
    xlab("")+
    ylab("Salinity - ppm")+
    labs(title = paste("Huron River Watershed Council:", inStation_name, "Station", "Salinity - ppm"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Salinity was calculated from Conductivity results: S = EC ^ 1.0878 x 0.4665")+
    ylim(0,max(ChartData$Salinity *1.2))
  
  return(plot)
}
Salinity_ChartMaker(HuronCleaned, "Broadway St.")
### End Salinity ### 






#### END VISUALIZATIONS ###



#### GENERATING CHARTS #### 
## TO DO: Add section for looping over multiple groups ###
## Looping over stations ## 
for (row in 1:nrow(HuronStations))
{
## Selecting to the chart data we need 
ChartData <- HuronCleaned %>%
             filter(station_name == HuronStations$station_name[row])%>%
             filter(collection_date > ymd("2022-03-01"))%>%
             filter(collection_date < ymd("2022-11-01")) 

## Making filepath and removing special characters 
Filepath <- str_replace_all(string=HuronCleaned$station_name[row], pattern=" ", repl="")%>%
            str_replace_all(., "[[:punct:]]", "")

Filepath <- paste0(getwd(),"/Charts/",Filepath, "_", HuronStations$station_id[row])

#Making folder
dir.create(file.path(Filepath), recursive = TRUE)

  ### Summary Table 
   print("Printing Summary Table")
   TableMaker(ChartData,HuronStations$station_name[row], ThresholdList)%>%
   gtsave(filename = paste0(Filepath, "/", "Summary_Table.png"), expand = 5)

  ## Boxplot 
  ## TO DO: 
  ## Loop over and create for every parameter OR 
  ## Create array of box plots for each parameter 
  print("Printing BoxPlots")
  BoxPlot <- BoxPlotMaker(ChartData,HuronStations$station_name[row])
  ggsave(paste0(Filepath, "/", "BoxPlot.png"), plot = BoxPlot, width = 25, height = 25, units = c("cm"), dpi = 300)
  
  # ## Temperature Chart
  print("Printing Temperature Charts")
  Temp_Chart <- TempChartMaker(ChartData, HuronStations$station_name[row], TempCold)
   ggsave(paste0(Filepath, "/", "Temp_Chart.png"), plot = Temp_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## DO Chart
  ## Logic for handling Warm vs Cold !! 
  print("Printing DO Charts")
  DO_Chart <- DOChartMaker(ChartData, HuronStations$station_name[row],DO_Warm)
  ggsave(paste0(Filepath, "/", "DO_Chart.png"), plot = DO_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## Temp vs DO Chart
  print("Printing Temp_DO_Chart")
  Temp_DO_Chart <- Temp_DO_ChartMaker(ChartData, HuronStations$station_name[row])
  ggsave(paste0(Filepath, "/", "Temp_DO_Chart.png"), plot = Temp_DO_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## Temp vs pH Chart
  print("Printing Temp_DO_Chart")
  Temp_pH_Chart <- Temp_pH_ChartMaker(ChartData, HuronStations$station_name[row])
  ggsave(paste0(Filepath, "/", "Temp_pH_Chart.png"), plot = Temp_pH_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## Conductivity Timeseries
  print("Printing Conductivity Timeseries")
  Cond_Chart <- CondChartMaker(ChartData, HuronStations$station_name[row], CondThreshold)
  ggsave(paste0(Filepath, "/", "Cond_Chart.png"), plot = Cond_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)
  
  print("Printing Conductivity BoxPlot ")
  Cond_BoxPlot <- CondBoxplotMaker(ChartData, HuronStations$station_name[row],CondReference)
  ggsave(paste0(Filepath, "/", "Cond_BoxPlot.png"), plot = Cond_BoxPlot, width = 25, height = 25, units = c("cm"), dpi = 300)
  
  # ## pH Timeseries
  print("Printing pH_Chart")
  pH_Chart <- pHChartMaker(ChartData, HuronStations$station_name[row], pHThreshold)
  ggsave(paste0(Filepath, "/", "pH_Chart.png"), plot = pH_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## TDS Timeseries
  print("Printing TDS_Chart")
  TDS_Chart <- TDS_ChartMaker(ChartData, HuronStations$station_name[row], TDSThreshold)
  ggsave(paste0(Filepath, "/", "TDS_Chart.png"), plot = TDS_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## Chloride Timeseries
  print("Printing Chloride_Chart")
  Chloride_Chart <- Chloride_ChartMaker(ChartData, HuronStations$station_name[row], ChlorideThreshold)
  ggsave(paste0(Filepath, "/", "Chloride_Chart.png"), plot = Chloride_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## Salinity Timeseries
  print("Printing Salinity_Chart")
  Salinity_Chart <- Salinity_ChartMaker(ChartData, HuronStations$station_name[row])
  ggsave(paste0(Filepath, "/", "Salinity_Chart.png"), plot = Salinity_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)
}





### !!!! ### 





