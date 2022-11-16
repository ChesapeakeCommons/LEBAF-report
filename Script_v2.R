## LEBAF Data Reporting Workflow ##
## Created for Lake Erie Basin Assessment Framework ## 
## Created by Gabriel Watson 10.18.22 ## 
## Pulls LEBAF downloaded data from WaterReporter and Threshold data
## from googlesheets to generate a series of report ready charts ## 
## This represents a first pass for review on Wednesday 10.26.22 w/ LEBAF ## 


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
library(ggformula)
library(viridis)
library(leaflet)
library(tmap)
library(basemaps)
library(leaflegend)
library(leaflet.extras2)
library(mapview)
library(htmltools)

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
CondClorideConversion <- read_sheet(SheetURL, sheet = "Cond_Chloride_Conversion")
## Creating master list of thresholds for passing into Table Maker
ThresholdList <- list(DO_Cold,DO_Warm,TempWarm,TempCold,CondThreshold,pHThreshold,TDSThreshold,ChlorideThreshold)

## Pulling in Station Reference Data ##
## 1) Add reference sheet !! Please follow formatting !! - stored in this gsheet - "https://docs.google.com/spreadsheets/d/1zrSc_Cd2-V1k73lE3TrqX-QqYJkt117AMAb-O-kuhsA" 
## 2) Download data from Water Reporter and store in /Data/ of working directory Folder 
## 3) Add Import 
## 4) Add Cleaning Section 
## 5) Add Group name and dataframe to GroupData 
## 6) Run full scropt (ctrl A and click 'Run')

## Adding refernce data 
## Step 1) 
HuronReference <- read_sheet(SheetURL, sheet = "HuronReference")%>%
                       select(-c(station_name))
SUNYReference <- read_sheet(SheetURL, sheet = "SUNYReference")%>%
                       select(-c(station_name))
DoanReference <- read_sheet(SheetURL, sheet = "DoanReference")%>%
                       select(-c(station_name))
BuffaloReference <- read_sheet(SheetURL, sheet = "BuffaloNiagaraReference")%>%
                       select(-c(station_name))
ClintonReference <- read_sheet(SheetURL, sheet = "ClintonReference")%>%
                       select(-c(station_name))
MetroparksReference <- read_sheet(SheetURL, sheet = "ClevelandMetroparksReference")%>%
                       select(-c(station_name))
## Note that station ids do not match but station names do
ErieReference <- read_sheet(SheetURL, sheet = "ErieSWCDReference")

TinkersReference <- read_sheet(SheetURL, sheet = "TinkersReference")%>%
                    select(-c(station_name))


## Importing Water Quality Data from Water Reporter ### 
## Step 2) and Step 3) 
HuronRaw <- read.csv("Data/WaterReporterData2022/Huron_11_10_2022.csv")
SUNYRaw <- read.csv("Data/WaterReporterData2022/SUNY_11_8_2022.csv")
DoanRaw <- read.csv("Data/WaterReporterData2022/Doan_11_7_2022.csv")
BuffaloRaw <- read.csv("Data/WaterReporterData2022/Buffalo_11_7_2022.csv")
ClintonRaw <- read.csv("Data/WaterReporterData2022/ClintonWC_10_28_2022.csv")
MetroparksRaw <- read.csv("Data/WaterReporterData2022/Metroparks_11_7_2022.csv")
ErieRaw <- read.csv("Data/WaterReporterData2022/ErieSWCD_11_7_2022.csv")
TinkersRaw <- read.csv("Data/WaterReporterData2022/Tinkers_11_14_2022.csv")
### END DATA IMPORT ### 


### DATA CLEANING #### 
## Step 3) 
## The only thing to change is the 'left_join' at the end, and the exact parameter column names for the rename Tip: type colnames(DataRaw)
## Cleaning Data ## 
## Changing annoying column stuff ## 
## Notes 
##  - Conductivity is in uS/cm - microsiemens per centimeter
## Calculating TDS ##
## Calculating Salinity ## 
## Calculating Chloride ## 
## Turning date into date format 

ErieStations <- ErieRaw %>%
                distinct(station_name,station_id, latitude,longitude)%>%
                arrange(station_name)
            
write.csv(ErieStations, "ErieWaterReporterStations_v1.csv")

## Conversion function for Conductivity to Chloride
CondChlorideConvert <- function(inBasin,Value)
{
if(inBasin %in% CondClorideConversion$Basin)
{
Convert <- CondClorideConversion %>%
          filter(inBasin == Basin)

Chloride <- 10 ^ (Convert$Var1 + (Convert$Var2*log10(Value)))
}
else
{
Chloride <- ((Value / 1000) * 4.928)*100
}
}
CondChlorideConvertVect <- Vectorize(CondChlorideConvert)

### Cleaning Huron Data ### 
## QA/QC Notes 
## High DO and low conductivity (sub <300)
HuronCleaned <- HuronRaw %>%
                dplyr::rename("Dissolved Oxygen" = Dissolved.oxygen..DO...p.3546.,
                       "Conductivity" = Conductivity..p.3545.,
                       "Water Temperature" = Temperature..water..p.3547.,
                       "pH" = pH..p.3544.)%>%
                select(c("latitude","longitude", "collection_date","Dissolved Oxygen","Conductivity","Water Temperature","pH","station_id","station_name"))%>%
                mutate(TDS = Conductivity*.55)%>%
                mutate(Salinity = (Conductivity^1.0878)*.4665)%>%
                mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
                filter(collection_date > ymd("2022-03-01"))%>%
                filter(collection_date < ymd("2022-11-01"))%>%
                left_join(HuronReference, by = "station_id")%>%
                filter(!is.na(Temp))%>%
                mutate(Chloride = CondChlorideConvertVect(Basin,Conductivity))

### Cleaning SUNY Data ###
## QA/QC Notes 
## High DO and low Conductivity (TDS, Salinity, Chloride by extension)
SUNYCleaned <- SUNYRaw %>% 
                dplyr::rename("Dissolved Oxygen" = Dissolved.Oxygen..p.1523.,
                              "Conductivity" = Conductivity..p.3431.,
                              "Water Temperature" = Water.Temperature..p.1521.,
                              "pH" = pH..p.1522.)%>%
                select(c("latitude","longitude","collection_date","Dissolved Oxygen","Conductivity","Water Temperature","pH","station_id","station_name"))%>%
                mutate(TDS = Conductivity*.55)%>%
                mutate(Salinity = (Conductivity^1.0878)*.4665)%>%
                mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
                filter(collection_date > ymd("2022-03-01"))%>%
                filter(collection_date < ymd("2022-11-01"))%>%
                left_join(SUNYReference, by = "station_id")%>%
                filter(!is.na(Temp))%>%
                mutate(Chloride = CondChlorideConvertVect(Basin,Conductivity))


### Cleaning Doan Data ### 
DoanCleaned <- DoanRaw %>%
               dplyr::rename("Dissolved Oxygen" = Dissolved.oxygen..DO...p.3502.,
                "Conductivity" = Conductivity..p.3501.,
                "Water Temperature" = Temperature..water..p.3503.,
                "pH" = pH..p.3500.)%>%
                select(c("latitude","longitude","collection_date","Dissolved Oxygen","Conductivity","Water Temperature","pH","station_id","station_name"))%>%
                mutate(TDS = Conductivity*.55)%>%
                mutate(Salinity = (Conductivity^1.0878)*.4665)%>%
                mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
                filter(collection_date > ymd("2022-03-01"))%>%
                filter(collection_date < ymd("2022-11-01"))%>%
                left_join(DoanReference, by = "station_id")%>%
                filter(!is.na(Temp))%>%
                mutate(Chloride = CondChlorideConvertVect(Basin,Conductivity))


### Cleaning Buffalo Data ### 
BuffaloCleaned <- BuffaloRaw %>%
                  dplyr::rename("Dissolved Oxygen" = Dissolved.oxygen..DO...p.3487.,
                                "Conductivity" = Conductivity..p.3486.,
                                "Water Temperature" = Temperature..water..p.3488.,
                                "pH" = pH..p.3485.)%>%
                  select(c("latitude","longitude","collection_date","Dissolved Oxygen","Conductivity","Water Temperature","pH","station_id","station_name"))%>%
                  mutate(TDS = Conductivity*.55)%>%
                  mutate(Salinity = (Conductivity^1.0878)*.4665)%>%
                  mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
                  filter(collection_date > ymd("2022-03-01"))%>%
                  filter(collection_date < ymd("2022-11-01"))%>%
                  left_join(BuffaloReference, by = "station_id")%>%
                  filter(!is.na(Temp))%>%
                  mutate(Chloride = CondChlorideConvertVect(Basin,Conductivity))



ClintonCleaned <- ClintonRaw %>%
                dplyr::rename("Dissolved Oxygen" = Dissolved.oxygen..DO...p.3512.,
                              "Conductivity" = Conductivity..p.3511.,
                              "Water Temperature" = Temperature..water..p.3513.,
                              "pH" = pH..p.3510.)%>%
                 select(c("latitude","longitude","collection_date","Dissolved Oxygen","Conductivity","Water Temperature","pH","station_id","station_name"))%>%
                 mutate(TDS = Conductivity*.55)%>%
                 mutate(Salinity = (Conductivity^1.0878)*.4665)%>%
                 mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
                 filter(collection_date > ymd("2022-03-01"))%>%
                 filter(collection_date < ymd("2022-11-01"))%>%
                 left_join(ClintonReference, by = "station_id")%>%
                 filter(!is.na(Temp))%>%
                 mutate(Chloride = CondChlorideConvertVect(Basin,Conductivity))


MetroparksCleaned <- MetroparksRaw %>%
                dplyr::rename("Dissolved Oxygen" = Dissolved.oxygen..DO...p.3492.,
                "Conductivity" = Conductivity..p.3491.,
                "Water Temperature" = Temperature..water..p.3493.,
                "pH" = pH..p.3490.)%>%
                select(c("latitude","longitude","collection_date","Dissolved Oxygen","Conductivity","Water Temperature","pH","station_id","station_name"))%>%
                mutate(TDS = Conductivity*.55)%>%
                mutate(Salinity = (Conductivity^1.0878)*.4665)%>%
                mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
                filter(collection_date > ymd("2022-03-01"))%>%
                filter(collection_date < ymd("2022-11-01"))%>%
                left_join(MetroparksReference, by = "station_id")%>%
                filter(!is.na(Temp))%>%
                mutate(Chloride = CondChlorideConvertVect(Basin,Conductivity))


ErieCleaned <- ErieRaw %>%
              dplyr::rename("Dissolved Oxygen" = Dissolved.Oxygen..p.3337.,
                            "Conductivity" = Conductivity..p.3336.,
                            "Water Temperature" = Temperature..p.3355.,
                            "pH" = pH..p.3356.)%>%
              select(c("latitude","longitude","collection_date","Dissolved Oxygen","Conductivity","Water Temperature","pH","station_name"))%>%
              mutate(TDS = Conductivity*.55)%>%
              mutate(Salinity = (Conductivity^1.0878)*.4665)%>%
              mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
              filter(collection_date > ymd("2022-03-01"))%>%
              filter(collection_date < ymd("2022-11-01"))%>%
              left_join(ErieReference, by = "station_name")%>%
              filter(!is.na(Temp))%>%
              mutate(Chloride = CondChlorideConvertVect(Basin,Conductivity))

TinkersCleaned <- TinkersRaw %>%
              dplyr::rename("Dissolved Oxygen" = Dissolved.oxygen..DO...p.3522.,
                            "Conductivity" = Conductivity..p.3521.,
                            "Water Temperature" = Temperature..water..p.3523.,
                            "pH" = pH..p.3520.)%>%
              select(c("latitude","longitude","collection_date","Dissolved Oxygen","Conductivity","Water Temperature","pH","station_id","station_name"))%>%
              mutate(TDS = Conductivity*.55)%>%
              mutate(Salinity = (Conductivity^1.0878)*.4665)%>%
              mutate(collection_date = ymd(substr(collection_date,1,10)))%>%
              filter(collection_date > ymd("2022-03-01"))%>%
              filter(collection_date < ymd("2022-11-01"))%>%
              left_join(TinkersReference, by = "station_id")%>%
              filter(!is.na(Temp))%>%
              mutate(Chloride = CondChlorideConvertVect(Basin,Conductivity))


## Step 4)
## Creating Array of Group Data ##
## !! The GroupDataSource List and GroupName var need to be the same group order !! 
GroupDatasource <- list(HuronCleaned,SUNYCleaned,DoanCleaned,BuffaloCleaned, ClintonCleaned, MetroparksCleaned, ErieCleaned, TinkersCleaned)
GroupName <- c("Huron River Watershed Council", 
               "SUNY Fredonia", 
               "Doan Brook Watershed Partnership",
               "Buffalo Niagara Waterkeeper", 
               "Clinton River Watershed Council", 
               "Cleveland Metroparks",
               "Erie Soil and Water Conservation District", 
               "Tinkers Creek Watershed Partners")

GroupData <- data.frame(GroupName)

GroupData$GroupDatasource <- GroupDatasource

## Binding all data for River Summary
AllGroupData <- rbind(HuronCleaned,SUNYCleaned,DoanCleaned,BuffaloCleaned,ClintonCleaned,MetroparksCleaned,ErieCleaned, TinkersCleaned)

## Getting list of unique basins 
Basins <- AllGroupData %>%
          filter(!is.na(Basin))%>%
          distinct(Basin)
            
### END DATA CLEANING ### 







#### VIZUALIZATIONS #### 

## TABLE ## 
## Calculate table of max min and mean for all variables ##
# Function for generating data table
TableDataMaker <- function(inputDF)
{

  df <- inputDF %>%
    select_if(is.numeric)%>%
    select(-c("latitude","longitude"))
  
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

TableMaker <- function(df, inStation_name, inThresholds, Name)
{
  
  df <- df %>%
    relocate(c(`Dissolved Oxygen`,`Water Temperature`,Conductivity, TDS, pH, Chloride, Salinity))
  
  ### Warm vs. Cold Thresholds Logic ##   
  StreamTemp <- df %>%
               slice(1)%>%
               pull(Temp)

  ## Handling for Cold vs Warm
  if(StreamTemp == "Warm")
  {
  TempThreshold <- inThresholds[[3]]
  DOThreshold <- inThresholds[[2]]
  }
  else
  {
  TempThreshold <- inThresholds[[4]]
  DOThreshold <- inThresholds[[1]]
  }
  
  ## Joining in Water Thresholds 
  WaterThreshold <- TempThreshold %>%
                    mutate(Month = month(Month))
  
  ## Exceedence table for Water
  Water_Ex <- df %>%
    select(c(`Water Temperature`,collection_date))%>%
    mutate(Month = month(collection_date))%>%
    left_join(WaterThreshold)%>%
    mutate(Water_Ex = ifelse(`Water Temperature` < Value,0,1))%>%
    pull(Water_Ex)
  
  ## All other vars and pulling in Water Exceedence
  ExceedTable <- df %>%
                select(-c(collection_date,station_id,station_name))%>%
                ## Temp warm vs Temp Cold logic here ## 
                 mutate(DO_Ex = ifelse(`Dissolved Oxygen` > as.numeric(DOThreshold[1,2]),0,1)) %>% 
                 mutate(Cond_Ex = ifelse(Conductivity < as.numeric(inThresholds[[5]][1,2]) & Conductivity > as.numeric(inThresholds[[5]][3,2]),0,1))%>%
                 mutate(Water_Ex = Water_Ex)%>%
                 mutate(pH_Ex = ifelse(pH < as.numeric(inThresholds[[6]][1,2]) & pH > as.numeric(inThresholds[[6]][3,2]),0,1))%>%
                 mutate(TDS_Ex = ifelse(TDS >= as.numeric(inThresholds[[7]][3,2]),1,0))%>%
                 mutate(Chloride_Ex = ifelse(Chloride >= as.numeric(inThresholds[[8]][4,2]),1,0))%>%
                 mutate(Salinity_Ex = NA)%>%
                 select(c(DO_Ex,Water_Ex,Cond_Ex,TDS_Ex,pH_Ex,Chloride_Ex,Salinity_Ex))%>%
                 summarise_all(.,~sum(.x,na.rm = TRUE))
  
  DO_Ex <- sum(!is.na(df$`Dissolved Oxygen`))
  Water_Ex <- sum(!is.na(df$`Water Temperature`))
  Cond_Ex <- sum(!is.na(df$Conductivity))
  TDS_Ex <- sum(!is.na(df$TDS))
  pH_Ex <- sum(!is.na(df$pH))
  Chloride_Ex <- sum(!is.na(df$Chloride))
  Salinity_Ex <- sum(!is.na(df$Salinity))
  
  SampleCount <- c(DO_Ex, Water_Ex, Cond_Ex, TDS_Ex, pH_Ex, Chloride_Ex, Salinity_Ex)
  
  ExceedTable <- rbind(ExceedTable,SampleCount)

  ## a little cleaning 
  ExceedTable <- as.data.frame(t(ExceedTable))%>%
                 dplyr::rename(NumExceed = V1)%>%
                 mutate(PerExceed = as.character(round(NumExceed / V2,3)*100))%>%
                select(-c(V2))
  
  ## Adding units 
  df <- df %>%
        dplyr::rename("Chloride - mg/L" = Chloride,
                  "Conductivity - uS/cm" = Conductivity,
                  "Dissolved Oxygen - mg/L" = `Dissolved Oxygen`,
                  "Salinity - ppm" = Salinity,
                  "Total Dissolved Solids - mg/L" = TDS,
                  "Water Temperature - C" =  `Water Temperature`)
  
  
  
  ## Joining to other data and creating table 
  Table <- data.frame(TableDataMaker(df)) %>%
    tibble::rownames_to_column(., "Parameter") %>%
    mutate(`N Exceed` = ExceedTable$NumExceed)%>%
    mutate(`% Exceed` = paste0(ExceedTable$PerExceed, "%"))%>%
    gt()%>%
    tab_source_note(source_note = paste(Name, min(df$collection_date), " to ",max(df$collection_date)))%>%
    tab_header(title = paste(inStation_name, "Summary Statistics -", nrow(df), "Samples", ifelse(length(unique(df$station_name)) > 1, paste(length(unique(df$station_name)), "Stations"),"")))%>%
    
   
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
  print("Done Summary Chart")
  return(Table)
}

#Table <- TableMaker(ClintonCleaned %>% filter(station_name == "Paint Creek 1"),"Paint Creek 1", ThresholdList, "Name")
### END TABLES ### 

## Box and Whisker ## 
BoxPlotMaker <- function(df,inStation_name, Name)
    {
    ChartData <- df %>%
                 dplyr::rename("Chloride - mg/L" = Chloride,
                        "Conductivity - uS/cm" = Conductivity,
                        "Dissolved Oxygen - mg/L" = `Dissolved Oxygen`,
                        "Salinity - ppm" = Salinity,
                        "Total Dissolved Solids - mg/L" = TDS,
                        "Water Temperature - C" =  `Water Temperature`)%>%
                 filter(station_name == inStation_name)%>%
                 select_if(is.numeric)%>%
                 select(-c("latitude","longitude"))%>%
                 gather()

   plot <- ggplot() +
           geom_boxplot(data = ChartData, aes(y = value, fill = key), width = .25) +
           facet_wrap(~ key, scales = "free")+
           scale_x_discrete() +
           labs(title = paste(Name, inStation_name, "Station Summary Plots"),
                subtitle = paste("Data from:", min(df$collection_date), "to",max(df$collection_date)))+
           theme_classic()+
            theme(legend.position="none") +
           ylab("")
   
    return(plot)
}
#BoxPlotMaker(HuronCleaned,"Shetland Dr.")

## END BOX AND WHISKER ## 

## Temperature Chart ## 
TempChartMaker <- function(inGroup_data, inStation_name, inThresholds, Name)
{
  
  TempThreshold <- inThresholds %>%
       mutate(Month = month(Month))

  ChartData <- inGroup_data %>%
    mutate(Month = month(collection_date))%>%
    left_join(TempThreshold)%>%
    mutate(Color = "Poor")%>%
    mutate(Color = ifelse(`Water Temperature` < as.numeric(Value), "Good", Color))%>% ## Good 
    filter(!is.na(`Water Temperature`))
  
  
  Colors <- c("Poor" = "#f53e46", "Good" = "#1aaf54")
  
  plot <-  ggplot()+
    geom_line(data = inThresholds, aes(x = Month, y = Value), color = "#1aaf54", linetype = "dashed", show.legend = F)+
    geom_point(data = ChartData, aes(x=collection_date, y =`Water Temperature`, color = factor(Color)), size = 3)+
    scale_color_manual(name = "Thresholds", values=Colors)+
    theme_classic()+
    ylab("Water Temperature C")+
    xlab("")+
    labs(title = paste(Name, inStation_name, "Station", "Water Temperature - C"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards")+
    ylim(0,34)
  
  
  return(plot)
}
#TempChartMaker(MetroparksCleaned %>% filter(station_name == "Schaefer Park"),"Schaefer Park", TempWarm, "Test")

### END TEMP CHART ### 
## Threshold Charts ##

## Dissolved Oxygen ## 
DOChartMaker <- function(inGroup_data, inStation_name, inThreshold_data, Name)
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
    labs(title = paste(Name, inStation_name, "Station", "Dissolved Oxygen - mg/L"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards.")+
    ylim(0,max(ChartData$`Dissolved Oxygen` *1.2))
  
  return(plot)
}
#DOChartMaker(HuronCleaned %>% filter(station_name == "N. Territorial Rd."), "N. Territorial Rd.",DO_Warm, "Nick Conklin")
### END DISSOLVED OXYGEN ### 


### Temperature Vs. Dissolved Oxygen ## 
Temp_DO_ChartMaker <- function(inGroup_data, inStation_name, Name)
{
  ChartData <- inGroup_data %>%
    filter(station_name == inStation_name) %>% 
    filter(collection_date > ymd("2022-03-01"))%>%
    filter(collection_date < ymd("2022-11-01"))%>%
    filter(!is.na(`Water Temperature`))%>%
    filter(!is.na(`Dissolved Oxygen`))

plot <-  ggplot(data = ChartData, aes(x = collection_date))+
    geom_smooth(aes(y=`Water Temperature`), se = FALSE, color = "#f53e46", alpha = .1)+
    geom_point(aes(y=`Water Temperature`),color = "#f53e46")+
    geom_smooth(aes(y=`Dissolved Oxygen`), se = FALSE, color = "#1aaf54", alpha = .1)+
    geom_point(aes(y=`Dissolved Oxygen`),color = "#1aaf54")+
    theme_classic()+
    xlab("")+
    scale_y_continuous(
      name = "Water Temperature C",
      sec.axis = sec_axis(~., name="Dissolved Oxygen - mg/L"))+
    theme(
      axis.title.y = element_text(size=13),
      axis.text.y = element_text(color = "#f53e46", size = 10),
      axis.title.y.right = element_text(size=13, vjust = 4),
      axis.text.y.right = element_text(color = "#1aaf54", size = 10),
      axis.title.x = element_text(size = 13),
      axis.text.x = element_text(size = 10),
      plot.margin = unit(c(1,1,1,1),"cm"))+
      labs(title = paste(Name, inStation_name, "Station", "Temperature and Dissolved Oxygen"),
       subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)))

  return(plot)
}
#Temp_DO_ChartMaker(MetroparksCleaned, "Schaefer Park", "Cleveland Metroparks")
### End Temperature and Dissolved Oxygen  ### 

### Temperature Vs. pH ## 
## Recommend not doing this chart!! ##
Temp_pH_ChartMaker <- function(inGroup_data, inStation_name, Name)
{
  ChartData <- inGroup_data %>%
    filter(station_name == inStation_name) %>% 
    filter(collection_date > ymd("2022-03-01"))%>%
    filter(collection_date < ymd("2022-11-01"))%>%
    filter(!is.na(`Water Temperature`))%>%
    filter(!is.na(pH))
  
  plot <-  ggplot(data = ChartData, aes(x = collection_date))+
    geom_smooth(aes(y=`Water Temperature`), se = FALSE, color = "#f53e46", alpha = .1)+
    geom_point(aes(y=`Water Temperature`),color = "#f53e46")+
    geom_smooth(aes(y=pH), se = FALSE, color = "#38761d", alpha = .1)+
    geom_point(aes(y=pH),color = "#38761d")+
    theme_classic()+
    xlab("")+
    scale_y_continuous(
      name = "Water Temperature C",
      sec.axis = sec_axis(~., name="pH"))+
    theme(
      axis.title.y = element_text(size=13),
      axis.text.y = element_text(color = "#f53e46", size = 10),
      axis.title.y.right = element_text(size=13, vjust = 4),
      axis.text.y.right = element_text(color = "#38761d", size = 10),
      axis.title.x = element_text(size = 13),
      axis.text.x = element_text(size = 10),
      plot.margin = unit(c(1,1,1,1),"cm"))+
    labs(title = paste(Name, inStation_name, "Station", "Temperature and pH"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)))
  return(plot)
}
#Temp_pH_ChartMaker(MetroparksCleaned, "Schaefer Park", "Cleveland Metroparks")
### End Temperature and pH ### 


## Conductivity ## 
## FIXED ## 
CondChartMaker <- function(inGroup_data, inStation_name,inThreshold_data, Name)
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
    labs(title = paste(Name, inStation_name, "Station", "Conductivity"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards.")+
    ylim(0,max(ChartData$Conductivity *1.2))

   return(plot)
  }
## Testing 
#CondChartMaker(HuronCleaned, "N. Territorial Rd.", CondThreshold)
## End Conductivity 


## Conductivity Boxplot W/ Reference and Survery Sites ## 
CondBoxplotMaker <- function(inGroup_data, inStation_name, inThreshold_data,Name)
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

CondMedian <- c(median(ChartData$Conductivity),median(ChartData$Conductivity))
StreamType <- c("Reference","Survey")
CondMedianChart <- data.frame(CondMedian,StreamType)

plot <-  ggplot() +
              geom_boxplot(data = CondSurvBox,  aes(x = StreamType, ymin = 400, lower = x25, middle = x50,upper = x75, ymax = x90, fill = StreamType), stat = "identity")+              
              geom_point(data = ChartData, aes(y=Conductivity, x = StreamType), size = 3, color = "Black")+
              geom_point(data = CondMedianChart, aes(y=CondMedian, x = StreamType), size = 4, color = "#f53e46")+
              labs(title = paste(Name, inStation_name, "Conductivity and Control Sites"),
              subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)))+
              theme_classic()+
              ylab("Conductivity - uS/cm")+
              xlab("Stream Type")+
              scale_fill_manual(values = Legend)
  
  return(plot)
  

  }
## Testing 
#CondBoxplotMaker(TinkersCleaned %>% filter(station_name == "Broadway Trailhead"), "Broadway Trailhead",CondReference,"Test")


## PH ## 
pHChartMaker <- function(inGroup_data, inStation_name,inThreshold_data, Name)
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
    labs(title = paste(Name, inStation_name, "Station", "pH"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards.")+
    ylim(0,15)
  
  return(plot)
  
}
## Testing ## 
#pHChartMaker(HuronCleaned, "Shetland Dr.", pHThreshold)
## End pH ##


## TDS ## 
TDS_ChartMaker <- function(inGroup_data, inStation_name,inThreshold_data, Name)
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
  ylab("Total Dissolved Solids - mg/L")+
  scale_color_manual(name = 'Thresholds', values = ThresholdColors)+
  labs(title = paste(Name, inStation_name, "Station", "Total Dissolved Solids - mg/L"),
       subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
       caption = "Threshold generated from LEBAF standards. TDS was calculated from Conductivity results: TDS = k EC (in 25 °C),")+
  ylim(0,max(ChartData$TDS *1.2))

  return(plot)
}
## Testing ## 
#TDS_ChartMaker(HuronCleaned, "Broadway St.", TDSThreshold)
## End TDS ##


## Chloride ##
Chloride_ChartMaker <- function(inGroup_data, inStation_name,inThreshold_data, Name)
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
    labs(title = paste(Name, inStation_name, "Station", "Chloride - mg/L"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Threshold generated from LEBAF standards. Chloride was calculated from Conductivity results: [Cl-] = 4.928 EC")+
    ylim(0,max(ChartData$Chloride *1.2))
    
  return(plot)
}
## Testing ## 
#Chloride_ChartMaker(HuronCleaned, "Broadway St.", ChlorideThreshold)
### End Chloride ###


### Salinity ### 
Salinity_ChartMaker <- function(inGroup_data, inStation_name, Name)
{
  ChartData <- inGroup_data %>%
     filter(!is.na(Salinity))
  
  plot <-  ggplot(data = ChartData, aes(x=collection_date, y = Salinity))+
    geom_point(size = 3, color = "#3852a4")+
    theme_classic()+
    xlab("")+
    ylab("Salinity - ppm")+
    labs(title = paste(Name, inStation_name, "Station", "Salinity - ppm"),
         subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)),
         caption = "Salinity was calculated from Conductivity results: S = EC ^ 1.0878 x 0.4665")+
    ylim(0,max(ChartData$Salinity *1.2))
  
  return(plot)
}
## Testing ## 
#Salinity_ChartMaker(HuronCleaned, "Broadway St.")
### End Salinity ### 

#### Basin Level Charts ####
Basin_ChartMaker <- function(inBasinData, inBasinName,c)
{

ParameterName <- str_replace(colnames(inBasinData)[c],"[.]", " ") 
  
ChartData <- inBasinData %>%
             dplyr::rename(Values = c)

plot <- ggplot(ChartData, aes(x=collection_date, y=Values, color=as.factor(station_name)))+
    geom_smooth(se = FALSE)+
    geom_point(shape = 20, size = 3)+
    scale_color_viridis(discrete = TRUE, option = "D")+
    labs(color = "Stations")+
    ylab(ParameterName)+
    xlab("")+
    labs(title = paste(inBasinName, ParameterName, "Summary"),
       subtitle = paste("Data from:", min(ChartData$collection_date), "to",max(ChartData$collection_date)))+
    theme_classic()+
    theme(plot.title = element_text(size=22))

return(plot)
}
#### END VISUALIZATIONS ###

MapMaker <- function(inData,inName, inThresholds, Labels)
{
  
  df <- inData %>%
    relocate(c(`Dissolved Oxygen`,`Water Temperature`,Conductivity, TDS, pH, Chloride, Salinity))
  
  ### Warm vs. Cold Thresholds Logic ##   
  StreamTemp <- df %>%
    slice(1)%>%
    pull(Temp)
  
  ## Handling for Cold vs Warm
  if(StreamTemp == "Warm")
  {
    TempThreshold <- inThresholds[[3]]
    DOThreshold <- inThresholds[[2]]
  }
  else
  {
    TempThreshold <- inThresholds[[4]]
    DOThreshold <- inThresholds[[1]]
  }
  
  ## Joining in Water Thresholds 
  WaterThreshold <- TempThreshold %>%
    mutate(Month = month(Month))
  
  ## Exceedence table for Water
  Water_Ex <- df %>%
    select(c(`Water Temperature`,collection_date))%>%
    mutate(Month = month(collection_date))%>%
    left_join(WaterThreshold)%>%
    mutate(Water_Ex = ifelse(`Water Temperature` < Value,0,1))%>%
    pull(Water_Ex)
  
  Stations <- df %>%
    select(c(station_id,latitude,longitude))%>%
    group_by(station_id,latitude,longitude) %>%
    dplyr::mutate(count = n()) %>% 
    unique()
  
  SampleCount <- df %>%
    select(c(`Dissolved Oxygen`,Conductivity,`Water Temperature`, pH, TDS, Salinity, Chloride, station_id))%>%
    group_by(station_id)%>%
    summarise_all(funs(sum(!is.na(.))))
  
    colnames(SampleCount) <- paste(colnames(SampleCount), "Count", sep = "_")
  
    SampleCount <- SampleCount  %>%
                  dplyr::rename("station_id" = station_id_Count)
    
    print(SampleCount)
  
  ## All other vars and pulling in Water Exceedence
  MapData <- df %>%
    select(c(`Dissolved Oxygen`,Conductivity,`Water Temperature`, pH, TDS, Salinity, Chloride, station_id))%>%
    mutate(`Dissolved Oxygen` = ifelse(`Dissolved Oxygen` > as.numeric(DOThreshold[1,2]),0,1)) %>% 
    mutate(Conductivity = ifelse(Conductivity < as.numeric(inThresholds[[5]][1,2]) & Conductivity > as.numeric(inThresholds[[5]][3,2]),0,1))%>%
    mutate(`Water Temperature` = Water_Ex)%>%
    mutate(pH = ifelse(pH < as.numeric(inThresholds[[6]][1,2]) & pH > as.numeric(inThresholds[[6]][3,2]),0,1))%>%
    mutate(TDS = ifelse(TDS >= as.numeric(inThresholds[[7]][3,2]),1,0))%>%
    mutate(Salinity = 0)%>%
    mutate(Chloride = ifelse(Chloride >= as.numeric(inThresholds[[8]][4,2]),1,0))%>%
    group_by(station_id)%>%
    summarise_all(.,~sum(.x,na.rm = TRUE))%>%
    left_join(Stations)%>%
    left_join(SampleCount)%>%
    mutate(`Dissolved Oxygen` = round(`Dissolved Oxygen`/`Dissolved Oxygen_Count`, 3)*100)%>%
    mutate(Conductivity = round(Conductivity/Conductivity_Count, 3)*100)%>%
    mutate(`Water Temperature` = round(`Water Temperature`/`Water Temperature_Count`, 3)*100)%>%
    mutate(pH = round(pH/pH_Count, 3)*100)%>%
    mutate(TDS = round(TDS/TDS_Count, 3)*100)%>%
    mutate(Salinity = round(Salinity/Salinity_Count, 3)*100)%>%
    mutate(Chloride = round(Chloride/Chloride_Count, 3)*100)%>%
    select(-c(`Dissolved Oxygen_Count`,Conductivity_Count,`Water Temperature_Count`, pH_Count, TDS_Count, Salinity_Count, Chloride_Count))%>%
    relocate(c(station_id,count,latitude,longitude))
  

  Maps <- list()
  
  for(c in 5:ncol(MapData))
  {
  MapData <- MapData %>%
             mutate(Values = .[[c]])

  ParamName <- colnames(MapData[c])
  
  pal <- colorNumeric(
    palette = "RdYlGn",
    domain = c(0,100),
    reverse = TRUE)
  
  Map <- leaflet(data = MapData, options = leafletOptions(zoomControl = FALSE)) %>%
       addProviderTiles(providers$CartoDB.Voyager)%>%
       addCircleMarkers(group = "stations", label = ~paste0("<b>", Values, "<br>","<br>", station_id)  %>% lapply(htmltools::HTML),
                        labelOptions = labelOptions(noHide = ifelse(isTRUE(Labels), T, F), 
                                                    direction = "top",
                                                    textOnly = TRUE,
                                                    offset = c(0,40),
                                                    style = list("font-size" = "12px", "font-style" = "bold", "text-align" = "center")), 
                        radius = 10, fillColor = ~pal(Values), fillOpacity = .50, color = "black", weight = 1, stroke = 1)%>%
                        addControl(paste(inName, "-", ParamName, "- % Exceeding Threshold"), position = "topleft")%>%
                        addLegend(pal = pal, values = c(0,100), position = "topleft")

     Maps[[c]] <- Map

  }
  
  return(Maps)
}

#Maps <- MapMaker(AllGroupData %>% filter(Basin == "Huron"),"Test",ThresholdList, T)

#### GENERATING STATION LEVEL CHARTS #### 
## Looping over stations ## 
for (row in 1:nrow(GroupData))
{
Datasource <- as.data.frame(GroupData$GroupDatasource[row])

colnames(Datasource) <- gsub('\\.',' ',colnames(Datasource))

Name <- GroupData$GroupName[row]

Stations <-  Datasource %>%
             select(station_name,station_id)%>%
             distinct(station_name, .keep_all = TRUE)

for (row in 1:nrow(Stations))
 {
  
print(Stations$station_name[row])
  
## Selecting to the chart data we need 
ChartData <- Datasource %>%
             filter(station_name == Stations$station_name[row])%>%
             filter(collection_date > ymd("2022-03-01"))%>%
             filter(collection_date < ymd("2022-11-01"))

### Warm vs. Cold Thresholds Logic ##   
StreamTemp <- ChartData %>%
              slice(1)%>%
              pull(Temp)

if(StreamTemp == "Warm")
{
  TempThreshold <- TempWarm
  DOThreshold <- DO_Warm
}
else
{
  TempThreshold <- TempCold
  DOThreshold <- DO_Cold
}

## Making filepath and removing special characters 
Filepath <- str_replace_all(string=Stations$station_name[row], pattern=" ", repl="")%>%
            str_replace_all(., "[[:punct:]]", "")

Filepath <- paste0(getwd(),"/Charts/Groups/",str_replace_all(Name, " ", "") ,"/",Filepath, "_", str_replace_all(Stations$station_id[row], " ", ""))

#Making folder
dir.create(file.path(Filepath), recursive = TRUE)

  ### Summary Table 
   print("Printing Summary Table")
   TableMaker(ChartData,Stations$station_name[row], ThresholdList, Name)%>%
   gtsave(filename = paste0(Filepath, "/", "Summary_Table.png"), expand = 5)

  ## Boxplot
  print("Printing BoxPlots")
  BoxPlot <- BoxPlotMaker(ChartData,Stations$station_name[row], Name)
  ggsave(paste0(Filepath, "/", "BoxPlot.png"), plot = BoxPlot, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## Temperature Chart
  ## Logic for Warm vs Cold !!
  print("Printing Temperature Charts")
  Temp_Chart <- TempChartMaker(ChartData, Stations$station_name[row], TempThreshold, Name)
   ggsave(paste0(Filepath, "/", "Temp_Chart.png"), plot = Temp_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## DO Chart
  ## Logic for handling Warm vs Cold !!
  print("Printing DO Charts")
  DO_Chart <- DOChartMaker(ChartData, Stations$station_name[row], DOThreshold, Name)
  ggsave(paste0(Filepath, "/", "DO_Chart.png"), plot = DO_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## Temp vs DO Chart
  ## No data breaks this chart, adding check
  if(all(!is.na(ChartData$`Water Temperature`)) | all(!is.na(ChartData$`Dissolved Oxygen`)))
     {
  print("Printing Temp_DO_Chart")
  Temp_DO_Chart <- Temp_DO_ChartMaker(ChartData, Stations$station_name[row], Name)
  ggsave(paste0(Filepath, "/", "Temp_DO_Chart.png"), plot = Temp_DO_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)
    }

  # ## Temp vs pH Chart
  ## No data breaks this chart, adding check
  if(all(!is.na(ChartData$`Water Temperature`)) & all(!is.na(ChartData$`pH`)))
  {
  print("Printing Temp_pH_Chart")
  Temp_pH_Chart <- Temp_pH_ChartMaker(ChartData, Stations$station_name[row], Name)
  ggsave(paste0(Filepath, "/", "Temp_pH_Chart.png"), plot = Temp_pH_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)
  }

  # ## Conductivity Timeseries
  print("Printing Conductivity Timeseries")
  Cond_Chart <- CondChartMaker(ChartData, Stations$station_name[row], CondThreshold, Name)
  ggsave(paste0(Filepath, "/", "Cond_Chart.png"), plot = Cond_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  print("Printing Conductivity BoxPlot ")
  Cond_BoxPlot <- CondBoxplotMaker(ChartData, Stations$station_name[row],CondReference, Name)
  ggsave(paste0(Filepath, "/", "Cond_BoxPlot.png"), plot = Cond_BoxPlot, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## pH Timeseries
  print("Printing pH_Chart")
  pH_Chart <- pHChartMaker(ChartData, Stations$station_name[row], pHThreshold, Name)
  ggsave(paste0(Filepath, "/", "pH_Chart.png"), plot = pH_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## TDS Timeseries
  print("Printing TDS_Chart")
  TDS_Chart <- TDS_ChartMaker(ChartData, Stations$station_name[row], TDSThreshold, Name)
  ggsave(paste0(Filepath, "/", "TDS_Chart.png"), plot = TDS_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## Chloride Timeseries
  print("Printing Chloride_Chart")
  Chloride_Chart <- Chloride_ChartMaker(ChartData, Stations$station_name[row], ChlorideThreshold, Name)
  ggsave(paste0(Filepath, "/", "Chloride_Chart.png"), plot = Chloride_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  # ## Salinity Timeseries
  print("Printing Salinity_Chart")
  Salinity_Chart <- Salinity_ChartMaker(ChartData, Stations$station_name[row], Name)
  ggsave(paste0(Filepath, "/", "Salinity_Chart.png"), plot = Salinity_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

 }
}


#### GENERATING RIVER LEVEL CHARTS #### 
for(i in 1:nrow(Basins))
{
  Filepath <- str_replace_all(string=Basins$Basin[i], pattern=" ", repl="")%>%
    str_replace_all(., "[[:punct:]]", "")  
  
  Filepath <- paste0(getwd(),"/Charts/Rivers/",Filepath)
  
  dir.create(file.path(Filepath), recursive = TRUE)
  
  BasinData <- AllGroupData %>%
               filter(Basin == Basins$Basin[i])
  

  
  ### Maps ### 
  Maps <- MapMaker(BasinData, Basins$Basin[i] ,ThresholdList, T)
  
  ## Calculating monthly averages by station ##  
  MonthlyAveragesStations <- BasinData %>%
    select(-c("latitude","longitude"))%>%
    group_by(collection_date,station_name)%>%
    summarize_if(is.numeric,mean, na.rm = TRUE)%>%
    data.frame()
  
  ## Loops through every column EXCEPT the first two, collection_date and station_id, additional text columns will break this loop!
  for(c in 3:ncol(MonthlyAveragesStations))
  {
    
  ParameterName <- str_replace(colnames(MonthlyAveragesStations)[c],"[.]", " ") 
  Basin_Chart <- Basin_ChartMaker(MonthlyAveragesStations,Basins$Basin[i],c)
  ggsave(paste0(Filepath, "/", str_replace(ParameterName," ",""), "_Chart.png"), plot = Basin_Chart, width = 25, height = 25, units = c("cm"), dpi = 300)

  
  ## Printing Maps 
  Map <- Maps[[c + 2]]
  mapshot(Map, file = paste0(Filepath, "/", str_replace(ParameterName," ",""), "_Map.png"),  vwidth = 1080, vheight = 1080)
  }
  
  print("Printing Summary Table")
  TableMaker(BasinData,Basins$Basin[i], ThresholdList, "")%>%
  gtsave(filename = paste0(Filepath, "/", "Summary_Table.png"), expand = 5)
}

#### GENERATING FULL DATA CHARTS #### 
Filepath <- paste0(getwd(),"/Charts/LakeErieBasin/")

dir.create(file.path(Filepath), recursive = TRUE)
    AllGroupDataCold <- AllGroupData %>%
                        filter(Temp == "Cold")
  
    AllGroupDataWarm<- AllGroupData %>%
                        filter(Temp == "Warm")
    
    print("Printing Cold Summary Table")
    TableMaker(AllGroupDataCold, "Lake Erie Basin Cold Water",ThresholdList, "")%>%
    gtsave(filename = paste0(Filepath, "/", "Cold_Summary_Table.png"), expand = 5)

    print("Printing Warm Summary Table")
    TableMaker(AllGroupDataWarm, "Lake Erie Basin Warm Water",ThresholdList, "")%>%
    gtsave(filename = paste0(Filepath, "/", "Warm_Summary_Table.png"), expand = 5)
    
    MapsWarm <- MapMaker(AllGroupDataWarm,"Lake Erie Basin Warm Water",ThresholdList, F)
    MapsCold <- MapMaker(AllGroupDataCold %>% filter(station_id != "PC1"),"Lake Erie Basin Cold Water",ThresholdList, T)
    
    for(c in 3:ncol(MonthlyAveragesStations))
    {
    MapWarm <- MapsWarm[[c + 2]]
    MapCold <- MapsCold[[c + 2]]
    ParameterName <- str_replace(colnames(MonthlyAveragesStations)[c],"[.]", " ") 
    
    mapshot(MapWarm, file = paste0(Filepath, "/", str_replace(ParameterName," ",""), "_Warm_Map.png"),  vwidth = 1080, vheight = 1080)
    mapshot(MapCold, file = paste0(Filepath, "/", str_replace(ParameterName," ",""), "_Cold_Map.png"),  vwidth = 1080, vheight = 1080)
    }
    
    
    

### !!!! ### 





