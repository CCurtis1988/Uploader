
library(dplyr)
library(data.table)
library(lubridate)
library(sf)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(igraph)
library(tidygraph)
library(sfnetworks)
library(geos)
library(DBI)
library(shiny)
library(httr)

library("RSQLite")
# Set working directory
setwd("~/R_trace2/R_trace2/R_Trace")
source("~/R_trace2/R_trace2/R_Trace/appDep/Load_Functions.r")   
source('~/R_trace2/R_trace2/R_Trace/appDep/ggplot_theme_Publication-2.R')


drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "~/R_trace2/R_trace2/R_Trace/appDep/AMI_Data.db")
MeterDat_db <- tbl(con, "ElectricMeterReadings")
dbExecute(con, "CREATE TABLE IF NOT EXISTS bugs (id INTEGER PRIMARY KEY, description TEXT, status TEXT, date DATE)")

MeterDateRange <- dbGetQuery(con, sql_q_dateRange , extended_types = TRUE)
mindate <- as.POSIXct(MeterDateRange$StartDateTime)
maxdate <- as.POSIXct(MeterDateRange$EndDateTime)


print("Define Custom Colors...")
# Define custom colors
c25 <- c("dodgerblue2","#E31A1C", # red
         "green4",
         "#6A3D9A", # purple
         "#FF7F00", # orange
         "black","gold1",
         "skyblue2","#FB9A99", # lt pink
         "palegreen2",
         "#CAB2D6", # lt purple
         "#FDBF6F", # lt orange
         "gray70", "khaki2",
         "maroon","orchid1","deeppink1","blue1","steelblue4",
         "darkturquoise","green1","yellow4","yellow3",
         "darkorange4","brown")

c25=c25[c(1:6,15,16,18:25,7,8,9,10,11,12,13,14,17)] #Sort colors (for looks).


setProgress(0.1, detail = "Finished loading dependancies ...")

conductor_import$Type = "Primary"
conductorsec_import$Type = "Secondary"




colnames(meters_import)[colnames(meters_import) == "Feeder"] <- "MeterFeeder"
colnames(conductor_import)[colnames(conductor_import) == "Feeder"] <- "FeederID"
colnames(conductorsec_import)[colnames(conductorsec_import) == "Feeder"] <- "FeederID"
colnames(startingPts_import)[colnames(startingPts_import) == "Feeder"] <- "FeederID_Start"

#meters_import$LoadkVA<-1

#PrimaryWire <- st_as_sf(rbindlist(list(PriOH_locations,PriUG_locations), fill = TRUE))
#PrimaryWire <- PrimaryWire[!st_is_empty(PrimaryWire),]
PrimaryWire_WGS84 <- st_transform(conductor_import,crs = 4326)

#SecondaryWire <- st_as_sf(rbindlist(list(SecOH_locations,SecUG_locations), fill = TRUE))
#SecondaryWire <- SecondaryWire[!st_is_empty(SecondaryWire),]
SecondaryWire_WGS84 <- st_transform(conductorsec_import,crs = 4326)

setProgress(0.12, detail = "Combining Conductor Linework ...")

Network_lines <- st_as_sf(rbindlist(list(conductor_import,conductorsec_import), fill = TRUE)) #|>
multi_lines <- Network_lines[!st_is_empty(Network_lines),] |>
  sf::st_cast(to = "LINESTRING")
buf_all <- st_buffer(opens_import, 1) #Puts 1 ft buffer around all Opens so physical gap is created when lines are selected out by location. 
parts <- st_collection_extract(lwgeom::st_split(multi_lines$geom, buf_all), "LINESTRING")
parts<-st_as_sf(parts)
parts$geom<-st_geometry(parts)


Network_Conductor_Naty <- parts |>
  sf::st_cast(to = "LINESTRING")
#st_transform(crs = 4326) |>
#data.table() |>
#mutate(geo = as_geos_geometry((geom)))

# Opens <- Opens |>
#			st_transform(crs = 4326)

setProgress(0.35, detail = "Creating sfnetwork (igraph) ...")
Network_framework <- as_sfnetwork(st_as_sf(Network_Conductor_Naty))
setProgress(0.37, detail = "Snapping starting points and meters to linework ...")
#Snapping Starting points to nearest nodes because geometry is slightly off.
nearest_nodes = st_nearest_feature(startingPts_import, Network_framework)
snapped_pois = startingPts_import %>%
  st_set_geometry(st_geometry(Network_framework)[nearest_nodes])

###Adding this section from the ArcPro traceNetwork. Should make this a script since its used in two locations.
#################################
#Converting all conductor lines to points and returned only the first and last vertex
#################################
###############SecOH
nearest_nodes_AMI = st_nearest_feature(meters_import, Network_framework)
snapped_pois_AMI = meters_import %>%
  st_set_geometry(st_geometry(Network_framework)[nearest_nodes_AMI])
AMI_locations = snapped_pois_AMI

colnames(AMI_locations)[colnames(AMI_locations) == "ID"] <- "AcctNo"
############################
##AMI removing duplicate locations & adding duplicated to list
##############################

setProgress(0.38, detail = "Processing meter data ...")
#Clean data by removing NA values
if(length(!is.na(AMI_locations$AcctNo))>0) {
  AMI_locations_clean<-AMI_locations[!is.na(AMI_locations$AcctNo),]
  } else {
    AMI_locations_clean<-AMI_locations
  }
AMI_locations_clean$PeakUsagekW_GIS <- 5 #AMI_locations_clean$PeakUsagekW


#Adding dummy data for Winter AMI usage
#AMI_locations_clean$PeakUsageWinterkW_source<-5 # Removing because time

#Adding new columns that conditionally assign usage and production for Winter and summer seasons.
#Using current peak usage for summer peak usage. Will update with real summer numbers once received.
AMI_locations_clean$RevkW<-NA
AMI_locations_clean$ProdkW<-NA
AMI_locations_clean$RevMeter<-NA
AMI_locations_clean$ProdMeter<-NA

AMI_locations_clean[AMI_locations_clean$Type!="Prod",]$RevkW<-AMI_locations_clean[AMI_locations_clean$Type!="Prod",]$PeakUsagekW
AMI_locations_clean[AMI_locations_clean$Type=="Prod",]$ProdkW<-AMI_locations_clean[AMI_locations_clean$Type=="Prod",]$PeakUsagekW
AMI_locations_clean[AMI_locations_clean$Type!="Prod",]$RevMeter<-1
AMI_locations_clean[AMI_locations_clean$Type=="Prod",]$ProdMeter<-1


#Create group column to group by
AMI_locations_clean$groupcol<-paste0(unlist(st_geometry(AMI_locations_clean))[c(TRUE,FALSE)],unlist(st_geometry(AMI_locations_clean))[c(FALSE,TRUE)])

#Convert to type data.table to group quickly
AMI_dt<-as.data.table(AMI_locations_clean)

#Group in two parts then combine at the end
group1<-AMI_dt[, lapply(.SD, first), by=groupcol]
group2<-AMI_dt[, .(Accounts=paste(list(AcctNo),collapse = ", "),
                   MeterCount=.N, 
                   RevMeterCount=sum(RevMeter,na.rm = TRUE),
                   ProdMeterCount=sum(ProdMeter,na.rm = TRUE), 
                   TotalPeakRevkW=sum(RevkW, na.rm = TRUE),
                   MeanPeakRevkW=mean(RevkW, na.rm = TRUE),
                   RevNACount=sum(is.na(RevkW))-sum(is.na(RevMeter)),
                   TotalPeakProdkW=sum(ProdkW, na.rm = TRUE),
                   MeanPeakProdkW=mean(ProdkW, na.rm = TRUE),
                   ProdNACount=sum(is.na(ProdkW))-sum(is.na(ProdMeter))), 
               by=groupcol]
AMI_locations_final<-left_join(group1,group2)

AMI_locations_final[, `:=` (TotalRevSyn  = MeanPeakRevkW*RevMeterCount,
                            TotalProdSyn  = MeanPeakProdkW*ProdMeterCount)]

#return to sf object
AMI_locations_final <- st_as_sf(AMI_locations_final)

#AMI_locations_final <- AMI_locations_final[,c("AcctNo",
##                                              "Accounts", 
#                                              "LocationID", 
#                                              "BillCode", 
#                                              "TotalPeakProdkW",
#                                              "MeanPeakProdkW",
#                                              "TotalProdSyn",
#                                              "ProdNACount",
#                                              "TotalPeakRevkW",
#                                              "MeanPeakRevkW",
#                                              "TotalRevSyn",
#                                              "RevNACount", 
#                                              "ProdMeterCount", 
#                                              "RevMeterCount")]				

AMI_locations_final$TotalRevSyn[is.na(AMI_locations_final$TotalRevSyn)] <- 0
AMI_locations_final$TotalRevSyn[is.nan(AMI_locations_final$TotalRevSyn)] <- 0
AMI_locations_final$TotalRevSyn[is.null(AMI_locations_final$TotalRevSyn)] <- 0

setProgress(0.45, detail = "Joining transformers, meters and starting points to network ...")


TransJoin <- Trans_import |> mutate(T_AssetID = ID) |> select("T_AssetID")
OpensJoin <- opens_import |> mutate(O_AssetID = ID) |> select("O_AssetID")


Network <- Network_framework |> activate("edges") |> 
  mutate(lineN = row_number()) |>
  st_join(multi_lines, join = st_intersects, largest = TRUE) |>  
  st_join(snapped_pois) |>
  #st_join(Opens[Opens$PosA==0,]) |> 
  #filter(is.na(AssetID)) |>
  activate("nodes") |>
  st_join(snapped_pois) |>
  st_join(AMI_locations_final) |>
  st_join(TransJoin)

Breakers <- ProDev_import

Transformers <- Trans_import
colnames(Transformers)[colnames(Transformers) == "ID"] <- "AssetID"
colnames(Transformers)[colnames(Transformers) == "TranskVA"] <- "BankkVA"

Network_trans <- Network |> activate("edges") |>
  st_join(Transformers[,c("AssetID")]) |>
  filter(Type=="Secondary") |>
  activate("nodes") |> 
  st_join(Transformers[,c("BankkVA")]) 
Network_trans_edges <- Network_trans |> activate("edges") |> st_as_sf() |>
  data.table() |>
  mutate(geo = as_geos_geometry((geom)))

Network_trans_edges$group <- components(graph.adjlist(geos_touches_matrix(Network_trans_edges$geo,geos_strtree(Network_trans_edges$geo))))$membership
Trans_lookup <- Network_trans_edges[,.(Feeder = paste(unique(na.omit(AssetID)),collapse = "/"),geom = st_union(geom)), by = group]  				
Network_trans_edges$TransGroup <- Trans_lookup[match(Network_trans_edges$group,Trans_lookup$group),]$Feeder
TransGroups <- Network_trans_edges[,c("TransGroup","geom")] |>
  st_as_sf()	|>
  group_by(TransGroup) |> 
  summarise(geom = st_union(geom)) |> 
  st_cast("MULTILINESTRING")
#TransGroups <- Trans_lookup |>
#  st_as_sf(sf_column_name = "geom") |>
#  mutate(TransGroup = Feeder) |>
#  select(TransGroup)

Network_Trans_Table <- Network_trans |> 
  activate("nodes") |> 
  #mutate(group = group_components(type="strong")) |>
  st_join(TransGroups) |>
  st_as_sf() |>
  data.table()

Trans_Table <- Network_Trans_Table[,.(total_Bank_kVA = sum(BankkVA,na.rm = T), total_Rev_kW = sum(TotalRevSyn, na.rm=T), total_Rev_NAs = sum(RevNACount,na.rm=T), total_Rev_meters = sum(RevMeterCount,na.rm=T), total_prod_kW = sum(TotalProdSyn, na.rm=T), total_Prod_NAs = sum(ProdNACount,na.rm=T), total_Prod_meters = sum(ProdMeterCount,na.rm=T)),by=.(TransGroup)]    ### returns data.table with tot.ET and group variables 

AMI_XY_WGS84<-st_transform(AMI_locations,crs = 4326)
AMI_XY_WGS84$CustType <- if_else(AMI_XY_WGS84$Type == "Prod","Production","Usage")

AMI_XY = AMI_XY_WGS84 |>
    mutate(ID = row_number())

AMI_XY <- AMI_XY |> st_join(st_transform(TransGroups,crs = 4326)) |>
  mutate(Transformer = TransGroup)

FeederCalcList <- startingPts_clean$FeederID

Opens <- opens_import |> rename(AssetID = ID)

setProgress(0.55, detail = "Calculating network conductivity ...")

Opensfunction <- function() {
  
  Network_active <<- Network |> activate("edges") |> 
    st_join(Opens[Opens$Pos==0,]) |> 
    #st_join(x[x$PosA==0,], join = st_is_within_distance, dist = 0.5) |> 
    filter(is.na(AssetID)) |>
    activate(nodes) |>
    st_join(OpensJoin)
  print("Separate out the linework for grouping...")	
  #Separate out the linework for grouping.
  Network_active_edges <- Network_active |> activate("edges") |> st_as_sf() |>
    data.table() |>
    mutate(geo = as_geos_geometry((geom)))
  
  print("Find the connected groups...")	
  # Find the connected groups. 
  Network_active_edges$group <- components(graph.adjlist(geos_touches_matrix(Network_active_edges$geo,geos_strtree(Network_active_edges$geo))))$membership
  
  print("Build lookup table to identify feeders based on feeder starting points...")  
  # Build lookup table to identify feeders based on feeder starting points 
  Cond_lookup <- Network_active_edges[,.(Feeder = paste(unique(na.omit(FeederID_Start)),collapse = "/"), FeederkV = paste(unique(na.omit(FeederkV)),collapse = "/")), by = group]
  
  print("Label feeders using lookup table...")
  # Label feeders using lookup table
  Network_active_edges$Feeder <- Cond_lookup[match(Network_active_edges$group,Cond_lookup$group),]$Feeder
  Network_active_edges$FeederkV <- Cond_lookup[match(Network_active_edges$group,Cond_lookup$group),]$FeederkV
  print("Make temp object to transfer feeder...")
  # Make temp object to transfer feeder
  FeederLines <<- Network_active_edges[,c("Feeder", "FeederkV","geom")] |>
    st_as_sf()
  Network_active <<- Network_active |> 
    activate("nodes") |> 
    st_join(FeederLines)
  FeederLinesR <<- reactiveVal(value = FeederLines, label = "FeederLines")
  
  listNodeDist <- lapply(FeederCalcList,getNodedistance)
  #listNodeDist <- listNodeDist[-which(sapply(listNodeDist, is.null))]
  NodesDist <<- do.call(rbind, listNodeDist)
  
  Network_active <<- Network_active |> 
    activate("nodes") |> 
    st_join(NodesDist) |> 
    mutate(NodeID = row_number())
  
  Network_activetemp <<- Network_active |> activate(nodes) |> as_tibble() |>
    mutate(name = NodeID, FeederID2 = paste0("F",FeederID_Start))
  
  Network_activetemp$name[!is.na(Network_activetemp$AcctNo)] <<- paste0("AcctNo ",Network_activetemp$AcctNo[!is.na(Network_activetemp$AcctNo)])
  Network_activetemp$name[!is.na(Network_activetemp$FeederID_Start)] <<- Network_activetemp$FeederID2[!is.na(Network_activetemp$FeederID_Start)]
  Network_activetemp$name[!is.na(Network_activetemp$O_AssetID)] <<- Network_activetemp$O_AssetID[!is.na(Network_activetemp$O_AssetID)]
  Network_activetemp$name[!is.na(Network_activetemp$T_AssetID)] <<- Network_activetemp$T_AssetID[!is.na(Network_activetemp$T_AssetID)]
  
  
  Network_active <<- Network_active |> activate(nodes) |>
    left_join(Network_activetemp |> as_tibble() |> select(name, NodeID) , by = "NodeID")
  netUNI <<- to_undirected(Network_active)
  
  length(Network_activetemp$name[!is.na(Network_activetemp$AcctNo)])
  length(paste0("AcctNo ",Network_activetemp$AcctNo[!is.na(Network_activetemp$AcctNo)]))
  length(Network_activetemp$name)
  
  print("Separate linework for map ploting...")
  # Separate linework for map ploting.
  Network_Lines <<- Network_active_edges |> st_as_sf() |> st_transform(crs = 4326)
  
  print("Convert opens object for mapping...")
  # Convert opens object for mapping
  x <- Opens |> st_transform(crs = 4326)
  
  print("Separate nodes to get use totals...")
  # Separate nodes to get use totals
  Network_active_nodes <- Network_active |> 
    activate("nodes") |> 
    #mutate(group = group_components(type="strong")) |>
    #st_join(FeederLines)  |>
    st_as_sf() 
  #Network_active_nodes <- Network_active_nodes |>
  #  data.table()
  

  AMI_filt <- Network_active_nodes[,c("AcctNo","Accounts",grep("Col", colnames(Network_active_nodes), value = TRUE),"Feeder","TotalProdSyn","ProdNACount","ProdMeterCount","TotalRevSyn","RevNACount","RevMeterCount")]
  AMI_filt <- AMI_filt |> data.table()
  AMI_sum <- AMI_filt[,.(total_Rev_kW = sum(TotalRevSyn, na.rm=T), total_Rev_NAs = sum(RevNACount,na.rm=T), total_Rev_meters = sum(RevMeterCount,na.rm=T), total_prod_kW = sum(TotalProdSyn, na.rm=T), total_Prod_NAs = sum(ProdNACount,na.rm=T), total_Prod_meters = sum(ProdMeterCount,na.rm=T)),by=.(Feeder)]    ### returns data.table with tot.ET and group variables 
  AMI_table <<- reactiveVal(value = AMI_sum, label = "AMI_table")
  #AMI_table  <- AMI_sum           
  
  print("Define feeder table...")
  #Define feeder table 
  print("Create leaflet map datasets...")
  # Create leaflet map dataset
  Network_Lines$FeederFact <- factor(substr(Network_Lines$Feeder , start = 2, stop = nchar(Network_Lines$Feeder)))
  conductorpal <<- colorFactor("plasma", levels = levels(Network_Lines$FeederFact))
  x$cat<-factor(x$Pos)
  factpal_opens <<- colorFactor(c("green", "red", "orange"), x$cat)
  Maplines <<- reactiveVal(value = Network_Lines,label = "MaplinesR")
  #Maplines(Network_Lines)
  Mapopens <<- reactiveVal(value = x,label = "MapOpensR")
  #Mapopens(x)
  TransformersMap <<- Transformers |> st_transform(crs = 4326)#|> select(AssetID, Location, SubtypeCode, PhasingCode, Voltage, PriProt, SecProt, LabelText,PlatNo,BankkVA,CentralTap,EquivIMP_ETAP,PrikV_ETAP,SeckV_ETAP,TxInBank,EquivKVA_ETAP,PosXoverR_ETAP,ZeroXoverR_ETAP) 
  # Define basemap for Opens map.
  
  basemap <<- leaflet() %>%
    addProviderTiles(providers$Stadia.AlidadeSmoothDark,
                     options = providerTileOptions(minZoom = 10, maxZoom = 24)) %>%
    setView(lng = -120.616647, lat = 37.359450, zoom = 12) %>%
    addPolylines(data = Network_Lines, group = "addon",
                 color = conductorpal(Network_Lines$FeederFact),
                 popup = paste(sep ="",
                               "<b>",Network_Lines$Feeder,"</b>","<br/>",
                               Network_Lines$Subtype,"<br/>",
                               Network_Lines$Voltage,"<br/>",
                               Network_Lines$ConductorSize,"<br/>",
                               extra_fields_add(Network_Lines,collookupCon)
                 )) %>%
    addCircleMarkers(data = TransformersMap, group = "trans",
                     layerId = TransformersMap$AssetID,
                     popup = popupTable(TransformersMap,row.numbers = FALSE),
                     #clusterOptions = markerClusterOptions()
                     radius = 5,
                     color = "black",
                     stroke = FALSE, 
                     fillOpacity = 0.75) %>%
    addCircleMarkers(data = x, group = "addon",
                     layerId = x$AssetID,
                     popup = x$AssetID,
                     #clusterOptions = markerClusterOptions()
                     radius = 3,
                     color = ~factpal_opens(cat) ,
                     stroke = FALSE, 
                     fillOpacity = 0.5) %>%
    groupOptions("trans", zoomLevels = 14:24)
  print("Update Open Maps Proxy...") 
  print("Join AMI data to graph and remove duplicate lines. This section needs correction so that lines loading add correctly...")
  # Join AMI data to graph and remove duplicate lines. This section needs correction so that lines loading add correctly. 
  

  
  Network_active_nodesLoads <<- Network_active |> 
    activate("nodes") |> 
    #mutate(group = group_components(type="strong")) |>
    #st_join(FeederLines)  |> 
    activate("edges") |> 
    st_join(AMI_locations_final[,c("AcctNo","Accounts","TotalRevSyn","TotalProdSyn")]) |>
    filter(!(check_duplicates(geom) & check_duplicates(AcctNo)))
  listofdata <- lapply(FeederCalcList,getLineLoading)
  #listofdata <- listofdata[-which(sapply(listofdata, is.null))]
  LineLoads_All <<- do.call(rbind, listofdata)
  

  
  LineLoads_Stats <<- LineLoads_All |>
    select(Voltage,Phase,Type,Feeder,grep("Col", colnames(LineLoads_All), value = TRUE),root,child_acc)
  
  print("Calc breakers table...")
  #Calc breakers table
  
  Breakerstable <<- Breakers |> 
    st_join(LineLoads_All) |> as_tibble() |>
    select(ID,grep("Col", colnames(LineLoads_All), value = TRUE),Feeder, Phase, child_acc) |>
    rename("AssetID" = ID, "LoadkW" = child_acc) |>
    group_by(AssetID) %>%
    summarise_all(first)

  print("Calc Lines loading map datasets...")
  # Lines loading map datasets
  pal_domain <- log(LineLoads_All$child_acc)
  pal_domain[is.infinite(pal_domain)] <- NA
  pal_domain <<- pal_domain
  pal_load <<- colorNumeric(palette = "YlGnBu",domain = pal_domain)
  LineLoads_All <- LineLoads_All |> st_transform(crs = 4326)
  #LineLoads_All$Phase <- as.numeric(Subtype2Phase[match(LineLoads_All$Subtype,Subtype2Phase$Subtype),]$Phase)
  LineLoads_All$Amps <- if_else(str_count(LineLoads_All$Phase)==1,LineLoads_All$child_acc/as.numeric(LineLoads_All$Voltage),LineLoads_All$child_acc/(as.numeric(LineLoads_All$Voltage)*3))
  
  LineLoads_AllR <<- reactiveVal(LineLoads_All, label = "LineLoads_AllR")
  
  basemap2 <<- leaflet() %>%
    addProviderTiles(providers$Stadia.AlidadeSmoothDark,
                     options = providerTileOptions(minZoom = 10, maxZoom = 24)) %>%
    setView(lng = -120.616647, lat = 37.359450, zoom = 12) %>%
    addPolylines(data = LineLoads_All, group = "addon",
                 color = ~pal_load(pal_domain),
                 popup = paste(sep ="",
                               "<b>","Feeder: " , LineLoads_All$Feeder,"</b>","<br/>",
                               "Type: " ,LineLoads_All$Type,"<br/>",
                               #"Conductor Size: ", LineLoads_All$ConductorSize,"<br/>",
                               "Voltage: ", LineLoads_All$Voltage,"<br/>",
                               #"Voltage_ETAP: ", LineLoads_All$V_ETAP,"<br/>",
                               "Current Load kW: ", LineLoads_All$child_acc,"<br/>",
                               "Phase: ", if_else(LineLoads_All$Phase==1,"Single","Three"),"<br/>",
                               "Amps: ", LineLoads_All$Amps,"<br/>",
                               extra_fields_add(LineLoads_All,collookupCon)),
                 layerId = LineLoads_All$accountAdd) %>%
    addCircleMarkers(data = TransformersMap, group = "trans",
                     layerId = TransformersMap$AssetID,
                     popup = popupTable(TransformersMap,row.numbers = FALSE),
                     #clusterOptions = markerClusterOptions()
                     radius = 5,
                     color = "black",
                     stroke = FALSE, 
                     fillOpacity = 0.75) %>%
    addCircleMarkers(data = x, group = "addon2",
                     layerId = x$AssetID,
                     popup = x$AssetID,
                     #clusterOptions = markerClusterOptions()
                     radius = 3,
                     color = ~factpal_opens(cat) ,
                     stroke = FALSE, 
                     fillOpacity = 0.5) %>%
    groupOptions("trans", zoomLevels = 14:24) %>%
    addLegend("topright", 
              pal = pal_load, 
              values = pal_domain,
              title = "Load kW",
              opacity = 1,
              group = "addon",
              labels = round(exp(pal_domain),2))
  
  #p_SecUG <<- reactiveVal(value = ggplot(LineLoads_All |> subset(Type =="Secondary Underground"), aes(x = ConductorSize, y = child_acc, fill = ConductorSize)) +
  #                          geom_boxplot()+
  #                          labs(title="Selected AMI Data", x = "Date", y = "Total kW")+
  #                          #scale_fill_brewer() +
  #                          theme(plot.title = element_text(hjust = 0.5)) +
  #                          theme_dark_blue(), label = "p_SecUG")
  
  
  
  #p_SecOH <<- reactiveVal(value = ggplot(LineLoads_All |> subset(Type =="Secondary Overhead"), aes(x = ConductorSize, y = child_acc, fill = ConductorSize)) +
  #                          geom_boxplot()+
  #                          labs(title="Selected AMI Data", x = "Date", y = "Total kW")+
  #                          #scale_fill_brewer() +
  #                          theme(plot.title = element_text(hjust = 0.5)) +
  #                          theme_dark_blue(), label = "p_SecOH") #+
  
  
  #p_PriUG <<- reactiveVal(value = ggplot(LineLoads_All |> subset(Type =="Primary Underground"), aes(x = ConductorSize, y = child_acc, fill = ConductorSize)) +
  #                          geom_boxplot()+
  #                          labs(title="Selected AMI Data", x = "Date", y = "Total kW")+
  #                          #scale_fill_brewer() +
  #                          theme(plot.title = element_text(hjust = 0.5)) +
  #                          theme_dark_blue(), label = "p_PriUG")  #+
  
  
  #p_PriOH <<- reactiveVal(value = ggplot(LineLoads_All |> subset(Type =="Primary Overhead"), aes(x = ConductorSize, y = child_acc, fill = ConductorSize)) +
  #                          geom_boxplot()+
  #                          labs(title="Selected AMI Data", x = "Date", y = "Total kW")+
  #                          #scale_fill_brewer() +
  #                          theme(plot.title = element_text(hjust = 0.5)) +
  #                          theme_dark_blue(), label = "p_PriUG") #+
}

Opensfunction()

setProgress(0.65, detail = "Updating meters table ...")

AMI_XY <- AMI_XY |> 
  st_join(st_transform(FeederLines |> rename('FeederfromLines' = Feeder),crs = 4326)) |>
  mutate(Feeder = FeederfromLines) |>
  select(AcctNo , Subtype, CustType, Feeder, Transformer, 'Protective Device', LoadkVA , grep("Col", colnames(AMI_XY), value = TRUE), geom) |>
  filter(!duplicated(AcctNo) & !is.na(AcctNo)) |>
  mutate(ID = row_number())

AMI_XY <<- AMI_XY %>%
  filter(!grepl("\\.", rownames(.)))

setProgress(0.67, detail = "Calculating protective devices ...")

updateProDevs()  # Need to start with ProDevCounts fucntion

ProtectiveDevice_Stats <- AMI_XY |> as_tibble() |>
  separate_rows(`Protective Device`, sep = " & ") |>
  filter(CustType == "Usage") |>
  group_by(`Protective Device`) |>
  summarize(N_Customer = n(), .groups = 'drop')

setProgress(0.85, detail = "Updating Maps ...")

AMI_mapfunction <- function (){
  AMI_XY$FeederIDFact<-factor(AMI_XY$Feeder)
  factpal <- colorFactor(c25[1:length(levels(AMI_XY$FeederIDFact))], AMI_XY$FeederIDFact)
  # Create the map object
  return(leaflet() %>%
           # Set the initial map view
           # Add a tile layer (OpenStreetMap)
           addTiles() %>%
           #addPolygons(data = TransXY,
           #           popup =TransXY$AssetID) %>%
           # Add a marker for San Francisco (just as an example)
           addPolylines(data = PrimaryWire_WGS84,
                        col="blue",
                        group = "Primary",
                        popup = paste(sep ="",
                                      "Phase: ", PrimaryWire_WGS84$Phase,"<br/>",
                                      "Voltage: ", PrimaryWire_WGS84$Voltage,"<br/>",
                                      "Feeder ID: ", PrimaryWire_WGS84$FeederID,"<br/>",
                                      extra_fields_add(PrimaryWire_WGS84,collookupCon))) %>%
           addPolylines(data = SecondaryWire_WGS84,
                        col = "red",
                        group = "Secondary",
                        popup = paste(paste(sep ="",
                                      "Conductor: ", SecondaryWire_WGS84$Phase,"<br/>",
                                      "Voltage: ", SecondaryWire_WGS84$Voltage,"<br/>",
                                      "Feeder ID: ", SecondaryWire_WGS84$FeederID,"<br/>",
                                      extra_fields_add(PrimaryWire_WGS84,collookupCon)),sep = "<br/>")) %>%
           groupOptions("Secondary", zoomLevels = 16:24) %>%
           addDrawToolbar(
             targetGroup = "draw",
             polylineOptions = FALSE,
             circleOptions = FALSE,
             rectangleOptions = FALSE,
             markerOptions = FALSE,
             circleMarkerOptions = FALSE,
             singleFeature = TRUE,
             editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
           addLayersControl(overlayGroups = c("draw"), options =
                              layersControlOptions(collapsed = FALSE)) %>%
           addMeasurePathToolbar(options = measurePathOptions(imperial = TRUE,
                                                              minPixelDistance = 100,
                                                              showDistances = FALSE,
                                                              showOnHover = TRUE)) %>%
           addMeasure(primaryLengthUnit="miles", secondaryLengthUnit="feet") %>%
           addCircleMarkers(data = AMI_XY, layerId = ~ID, #in the future I should probably change this to a generated rowID field
                            popup = paste0("Account: ",AMI_XY$AcctNo,"<br/>",
                                           "Feeder ID: ", AMI_XY$Feeder,"<br/>"),
                            #clusterOptions = markerClusterOptions(),
                            radius = 5,
                            color = ~factpal(FeederIDFact),
                            stroke = FALSE, 
                            fillOpacity = 0.5))
  
}

##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##Left off here
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################

linePhasing <- function(){
  listofdata = lapply(FeederCalcList,getLinePhasing)
  #listofdata = listofdata[-which(sapply(listofdata, is.null))]
  LineLoads_All = do.call(rbind, listofdata)
  return(LineLoads_All)
}

setProgress(0.87, detail = "Calculating Line Phasing ...")

CalcPhasing <- linePhasing()


source("~/R_trace2/R_trace2/R_Trace/appDep/AssetStats.R")  # Will come back to this later if I want to calc stats

setProgress(0.95, detail = "Updating Additional Maps ...")

AMI_mapDI <- function (){
  numpal <- colorNumeric(palette = "viridis", domain = AMI_stats$TotalErrors)
  mapData <- AMI_stats |> filter(TotalErrors > 0) |> st_transform(crs = 4326)
  # Create the map object
  return(leaflet() %>%
           # Set the initial map view
           # Add a tile layer (OpenStreetMap)
           addTiles() %>%
           #addPolygons(data = TransXY,
           #           popup =TransXY$AssetID) %>%
           # Add a marker for San Francisco (just as an example)
           addPolylines(data = PrimaryWire_WGS84,
                        col="grey",
                        weight = 2,
                        popup = paste(sep ="",
                                      "Conductor: ", PrimaryWire_WGS84$Phase,"<br/>",
                                      "Voltage: ", PrimaryWire_WGS84$Voltage,"<br/>",
                                      "Feeder ID: ", PrimaryWire_WGS84$FeederID,"<br/>",
                                      extra_fields_add(PrimaryWire_WGS84,collookupCon))) %>%
           addCircleMarkers(data = mapData, layerId = ~ID,
                            popup = paste0("Account: ",mapData$AcctNo,"<br/>",
                                           "Total Errors: ", mapData$TotalErrors,"<br/>",
                                           "Account Duplicated: ", mapData$AcctDu,"<br/>",
                                           "Trans Incorrect: ", mapData$TransML,"<br/>",
                                           "Feeder Incorrect: ", mapData$FeederML,"<br/>",
                                           "Protective Device Incorrect: ", mapData$ProDevML,"<br/>",
                                           "Missing Lat/Lon: ", mapData$MissingLL,"<br/>"),
                            #clusterOptions = markerClusterOptions(),
                            radius = 5,
                            color = ~numpal(mapData$TotalErrors),
                            stroke = FALSE, 
                            fillOpacity = 0.5) %>%
           addLegend(
             "bottomright",
             pal = numpal,
             values = mapData$TotalErrors,
             title = "Total Number of Errors",
             opacity = 1
           )
  )
}

Trans_mapDI <- function (){
  numpal <- colorNumeric(palette = "viridis", domain = Trans_Stats$TotalErrors)
  mapData <- Trans_Stats |> filter(TotalErrors > 0) |> st_transform(crs = 4326)
  # Create the map object
  return(leaflet() %>%
           # Set the initial map view
           # Add a tile layer (OpenStreetMap)
           addTiles() %>%
           #addPolygons(data = TransXY,
           #           popup =TransXY$AssetID) %>%
           # Add a marker for San Francisco (just as an example)
           addPolylines(data = PrimaryWire_WGS84,
                        col="grey",
                        weight = 2,
                        group = "Primary",
                        popup = paste(sep ="",
                                      "Conductor: ", PrimaryWire_WGS84$ConductorSize_ETAP,"<br/>",
                                      "Voltage: ", PrimaryWire_WGS84$PrikV_ETAP,"<br/>",
                                      "Feeder ID: ", PrimaryWire_WGS84$FeederID,"<br/>",
                                      extra_fields_add(PrimaryWire_WGS84,collookupCon))) %>%
           addCircleMarkers(data = mapData, layerId = ~AssetID,
                            popup = paste0("Transformer: ",mapData$AssetID,"<br/>",
                                           "Total Errors: ", mapData$TotalErrors,"<br/>",
                                           "Phase incorrect: ", mapData$PhaseML,"<br/>",
                                           "Trans Incorrect: ", mapData$PrikVML,"<br/>",
                                           "Feeder Incorrect: ", mapData$FeederML),
                            #clusterOptions = markerClusterOptions(),
                            radius = 5,
                            color = ~numpal(mapData$TotalErrors),
                            stroke = FALSE, 
                            fillOpacity = 0.5) %>%
           addLegend(
             "bottomright",
             pal = numpal,
             values = mapData$TotalErrors,
             title = "Total Number of Errors",
             opacity = 1
           )
  )
}

PriCon_mapDI  <- function (){
  numpal <- colorNumeric(palette = "viridis", domain = PriCon_Stats$TotalErrors)
  mapData <- PriCon_Stats |> filter(TotalErrors > 0) |> st_transform(crs = 4326)
  # Create the map object
  return(leaflet() %>%
           # Set the initial map view
           # Add a tile layer (OpenStreetMap)
           addTiles() %>%
           #addPolygons(data = TransXY,
           #           popup =TransXY$AssetID) %>%
           # Add a marker for San Francisco (just as an example)
           addPolylines(data = mapData,
                        color =  ~numpal(mapData$TotalErrors),
                        #weight = 2,
                        group = "Primary",
                        popup = paste(sep ="",
                                      "Conductor: ", mapData$TotalErrors,"<br/>",
                                      "Conductor: ", mapData$FeederML,"<br/>",
                                      "Voltage: ", mapData$PhaseMissing,"<br/>",
                                      "Feeder ID: ", mapData$NeutTypeMiss,"<br/>",
                                      "Voltage: ", mapData$NeutSizeMiss,"<br/>",
                                      "Voltage: ", mapData$LocationMiss,"<br/>",
                                      "Voltage: ", mapData$AssemblyCodeMiss,"<br/>",
                                      "Voltage: ", mapData$LabelTextMiss,"<br/>",
                                      "Voltage: ", mapData$ConductorMaterial_ETAPMiss,"<br/>",
                                      "Voltage: ", mapData$ConductorSize_ETAPMiss,"<br/>"
                        )) %>%
           addLegend(
             "bottomright",
             pal = numpal,
             values = mapData$TotalErrors,
             title = "Total Number of Errors",
             opacity = 1
           )
  )
}

SecCon_mapDI  <- function (){
  numpal <- colorNumeric(palette = "viridis", domain = SecCon_stats$TotalErrors)
  mapData <- SecCon_stats |> filter(TotalErrors > 0) |> st_transform(crs = 4326)
  # Create the map object
  return(leaflet() %>%
           # Set the initial map view
           # Add a tile layer (OpenStreetMap)
           addTiles() %>%
           #addPolygons(data = TransXY,
           #           popup =TransXY$AssetID) %>%
           # Add a marker for San Francisco (just as an example)
           addPolylines(data = mapData,
                        color =  ~numpal(mapData$TotalErrors),
                        weight = 2,
                        group = "Primary",
                        popup = paste(sep ="",
                                      "Total Errors: ", mapData$TotalErrors,"<br/>",
                                      "Incorrect Phase: ", mapData$PhaseML,"<br/>",
                                      "Incorrect Voltage: ", mapData$VoltML,"<br/>",
                                      "Incorrect Feeder: ", mapData$FeederML,"<br/>"
                        )) %>%
           addLegend(
             "bottomright",
             pal = numpal,
             values = mapData$TotalErrors,
             title = "Total Number of Errors",
             opacity = 1
           )
  )
}


Phasing_mapDI  <- function (){
  factpal <- colorFactor(palette = "viridis", domain = CalcPhasing$child_acc)
  mapData <- CalcPhasing |> st_transform(crs = 4326)
  # Create the map object
  return(leaflet() %>%
           # Set the initial map view
           # Add a tile layer (OpenStreetMap)   
           addTiles() %>%
           #addPolygons(data = TransXY,
           #           popup =TransXY$AssetID) %>%
           # Add a marker for San Francisco (just as an example)
           addPolylines(data = mapData,
                        color =  ~factpal(mapData$child_acc),
                        weight = 2,
                        group = "Primary",
                        popup = paste(sep ="",
                                      "Phasing Error: ", mapData$child_acc,"<br/>",
                                      "Subtype: ", mapData$Subtype,"<br/>",
                                      "Voltage: ", mapData$Voltage,"<br/>",
                                      "Size: ", mapData$ConductorSize,"<br/>",
                                      "Phase: ", mapData$Phase,"<br/>",
                                      "Feeder: ", mapData$Feeder,"<br/>"
                        )) %>%
           addLegend(
             "bottomright",
             pal = factpal,
             values = mapData$child_acc,
             title = "Phasing Error: ",
             opacity = 1
           )
  )
}


AMI_basemap <- AMI_mapfunction() # Disabling now because I don't need to calc map stats yet

AMI_Errormap <- AMI_mapDI() # Disabling now because I don't need to calc map stats yet

Trans_Errormap <- Trans_mapDI() # Disabling now because I don't need to calc map stats yet

PriCon_Errormap <- PriCon_mapDI() # Disabling now because I don't need to calc map stats yet

SecCon_Errormap <- SecCon_mapDI() # Disabling now because I don't need to calc map stats yet

Phasing_Errormap <- Phasing_mapDI() # Disabling now because I don't need to calc map stats yet

 source("appDep/BackgroundJobFunctions.R") # Call to run background jobs.

setProgress(0.98, detail = "Updating Updating Summary reports ...")

FeederSummaryTableR <- reactiveVal(value = FeederSummaryTable(as.numeric(mindate),as.numeric(maxdate)))
ProDevSummaryTableR <- reactiveVal(value = ProDevSummaryTable(as.numeric(mindate),as.numeric(maxdate)))
TransSummaryTableR <- reactiveVal(value = TransSummaryTable(as.numeric(mindate),as.numeric(maxdate)))


LastUpdateDate <- Sys.Date()  
save.image("~/R_trace2/R_trace2/R_Trace/appDep/Local_Data.RData")  
