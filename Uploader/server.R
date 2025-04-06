#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
reactlog::reactlog_enable() #enable react log for debugging hit ctrl + f3 to bring up.
options(shiny.maxRequestSize = 2048*1024^2)  # This sets the limit to 2GB



library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library("RSQLite")

#Function for loading AMI data
AMI_Loader <- function(amidatpath) {
  require(readr)
  require(stringr)
  AMIdata <- read_csv(amidatpath)
  return(AMIdata)
} 

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "AMI_Data.db")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  removeNone <- function(x) {unlist(x[x!="None"])}
  
  createTextInputs <- function(inputIds, labels) {
    inputs <- lapply(seq_along(inputIds), function(i) {
      textInput(inputIds[i], labels[i], value = labels[i])
    })
    ui_elements <- do.call(tagList, inputs)
    list(ui = ui_elements, inputIds = inputIds)
  }
  
  
  output$mapAddData <- renderLeaflet({
    #    req(isTruthy(input$Condshapefile) | isTruthy(input$StPtsshapefile) | isTruthy(input$Opensshapefile) | isTruthy(input$ProDevshapefile) | isTruthy(input$Metersshapefile))
    leaflet() %>%
      addTiles()
  })
  
  Rbbox <- reactive(label = "BBox Reactive",{
    
    bboxfinal <- st_bbox(st_sf(geometry = st_sfc(), crs = 4326))
    if(!is.null(input$Condshapefile) & input$OverWcond == TRUE ) {condR <- st_bbox(conductor_sf |> st_transform(crs = 4326))
    } else {
      condR <- st_bbox(st_sf(geometry = st_sfc(), crs = 4326))
    }
    if(!is.null(input$Condsecshapefile) & input$OverWseccond == TRUE ) {condsecR <- st_bbox(conductorsec_sf |> st_transform(crs = 4326))
    } else {
      condsecR <- st_bbox(st_sf(geometry = st_sfc(), crs = 4326))
    }
    if(!is.null(input$StPtsshapefile) & input$OverWStPts == TRUE ) {WStPtsR <- st_bbox(startingPts_sf |> st_transform(crs = 4326))
    } else {
      WStPtsR <- st_bbox(st_sf(geometry = st_sfc(), crs = 4326))
    }
    if(!is.null(input$Opensshapefile) & input$OverWOpens == TRUE ) {OpensR <- st_bbox(opens_sf |> st_transform(crs = 4326))
    } else {
      OpensR <- st_bbox(st_sf(geometry = st_sfc(), crs = 4326))
    }
    if(!is.null(input$ProDevshapefile) & input$OverWProDev == TRUE ) {ProDevR <- st_bbox(ProDev_sf |> st_transform(crs = 4326))
    } else {
      ProDevR <- st_bbox(st_sf(geometry = st_sfc(), crs = 4326))
    }
    if(!is.null(input$Transshapefile) & input$OverWTrans == TRUE ) {TransR <- st_bbox(Trans_sf |> st_transform(crs = 4326))
    } else {
      TransR <- st_bbox(st_sf(geometry = st_sfc(), crs = 4326))
    }
    if(!is.null(input$Metersshapefile) & input$OverWmeters == TRUE ) {metersR <- st_bbox(meters_sf |> st_transform(crs = 4326))
    } else {
      metersR <- st_bbox(st_sf(geometry = st_sfc(), crs = 4326))
    }
    
    bboxfinal$xmin <- min(c(condR$xmin,condsecR$xmin,WStPtsR$xmin,OpensR$xmin,ProDevR$xmin,TransR$xmin,metersR$xmin), na.rm=TRUE)
    bboxfinal$ymin <- min(c(condR$ymin,condsecR$ymin,WStPtsR$ymin,OpensR$ymin,ProDevR$ymin,TransR$ymin,metersR$ymin), na.rm=TRUE)
    bboxfinal$xmax <- max(c(condR$xmax,condsecR$xmax,WStPtsR$xmax,OpensR$xmax,ProDevR$xmax,TransR$xmax,metersR$xmax), na.rm=TRUE)
    bboxfinal$ymax <- max(c(condR$ymax,condsecR$ymax,WStPtsR$ymax,OpensR$ymax,ProDevR$ymax,TransR$ymax,metersR$ymax), na.rm=TRUE)
    
    bboxfinal
  })
  
  
  observe(label = "Primary Conductor Load",{
    req(input$Condshapefile)
    Condfiles <- input$Condshapefile
    NfilesCond <- length(Condfiles$datapath)
    data_listCond <- lapply(1:NfilesCond, function(i) {return(st_read(paste0("/vsizip/", file.path(Condfiles$datapath[i]))) |> st_cast(to = "LINESTRING"))})    
    conductor_sf <<- bind_rows(data_listCond)
    conductor_clean <<- conductor_sf |> st_drop_geometry() 
    
    print("shapefile ready")
  }) %>%
    bindEvent(input$Condshapefile)
  
  observe(label = "Primary Conductor Map Load",{
    req(input$Condshapefile)
    if(input$OverWcond == TRUE) {
      leafletProxy("mapAddData") %>% clearGroup(group = "conductor") %>%
        addPolylines(data = conductor_sf |> st_transform(crs = 4326), group = "conductor", color = "blue", weight = 1) %>%
        flyToBounds(lng1 = Rbbox()$xmin, lat1 = Rbbox()$ymin, lng2 = Rbbox()$xmax, lat2 = Rbbox()$ymax)
    } else if(input$OverWcond == FALSE){
      leafletProxy("mapAddData") %>% clearGroup(group = "conductor") 
    }
  })
  
  observe(label = "Secondary Conductor Load",{
    req(input$Condsecshapefile)
    Condfiles <- input$Condsecshapefile
    NfilesCond <- length(Condfiles$datapath)
    data_listCond <- lapply(1:NfilesCond, function(i) {return(st_read(paste0("/vsizip/", file.path(Condfiles$datapath[i]))) |> st_cast(to = "LINESTRING"))})    
    conductorsec_sf <<- bind_rows(data_listCond)
    conductorsec_clean <<- conductorsec_sf |> st_drop_geometry() #|> as_tibble()
    #mutate(across(where(is.Date), ~ format(.x, "%Y-%m-%d")))#|> 
    print("shapefile ready")
  }) %>%
    bindEvent(input$Condsecshapefile)
  
  observe(label = "Secondary Conductor Map Load",{
    req(input$Condsecshapefile)
    if(input$OverWseccond == TRUE) {
      leafletProxy("mapAddData") %>% clearGroup(group = "conductor_sec") %>%
        addPolylines(data = conductorsec_sf |> st_transform(crs = 4326), group = "conductor_sec", color = "lightblue", weight = 1) %>%
        flyToBounds(lng1 = Rbbox()$xmin, lat1 = Rbbox()$ymin, lng2 = Rbbox()$xmax, lat2 = Rbbox()$ymax)
    } else if(input$OverWseccond == FALSE){
      leafletProxy("mapAddData") %>% clearGroup(group = "conductor_sec") 
    }
  })
  
  
  observe(label = "Start Points Load",{
    req(input$StPtsshapefile)
    StPtsfiles <- input$StPtsshapefile
    NfilesStPts <- length(StPtsfiles$datapath)
    data_listStPts <- lapply(1:NfilesStPts, function(i) {return(st_read(paste0("/vsizip/", file.path(StPtsfiles$datapath[i]))))})    
    # Combine all data frames into one
    startingPts_sf <<- bind_rows(data_listStPts)
    startingPts_clean <<- startingPts_sf |> st_drop_geometry() |> as_tibble()
    
    print("Checking data for errors...")
    u = st_equals(startingPts_sf, retain_unique = TRUE)
    if(length(unique(unlist(u)))!=0) print(paste0("Found ", length(unique(unlist(u))), "points with duplicated geometry"))
    
    print("shapefile ready")
  }) %>%
    bindEvent(input$StPtsshapefile)
  
  observe(label = "Start Points Map Load",{
    req(input$StPtsshapefile)
    if(input$OverWStPts == TRUE) {
      leafletProxy("mapAddData") %>% clearGroup(group = "StartPts") %>%
        addCircleMarkers(data = startingPts_sf |> st_transform(crs = 4326), group = "StartPts", color = "green", weight = 1) %>%
        flyToBounds(lng1 = Rbbox()$xmin, lat1 = Rbbox()$ymin, lng2 = Rbbox()$xmax, lat2 = Rbbox()$ymax)
    } else if(input$OverWStPts == FALSE){
      leafletProxy("mapAddData") %>% clearGroup(group = "StartPts") 
    }
  })
  
  observe(label = "Opens Load",{
    req(input$Opensshapefile)
    Opensfiles <- input$Opensshapefile
    NfilesOpens <- length(Opensfiles$datapath)
    data_listOpens <- lapply(1:NfilesOpens, function(i) {return(st_read(paste0("/vsizip/", file.path(Opensfiles$datapath[i]))))})    
    # Combine all data frames into one
    opens_sf <<- bind_rows(data_listOpens)
    opens_clean <<- opens_sf |> st_drop_geometry() |> as_tibble()
    
    print("Checking data for errors...")
    u = st_equals(opens_sf, retain_unique = TRUE)
    if(length(unique(unlist(u)))!=0) print(paste0("Found ", length(unique(unlist(u))), "points with duplicated geometry"))
    print("shapefile ready")
  }) %>%
    bindEvent(input$Opensshapefile)
  
  observe(label = "Opens Map Load",{
    req(input$Opensshapefile)
    if(input$OverWOpens == TRUE) {
      leafletProxy("mapAddData") %>% clearGroup(group = "opens") %>%
        addCircleMarkers(data = opens_sf |> st_transform(crs = 4326), group = "opens", color = "orange", weight = 1) %>%
        flyToBounds(lng1 = Rbbox()$xmin, lat1 = Rbbox()$ymin, lng2 = Rbbox()$xmax, lat2 = Rbbox()$ymax)
    } else if(input$OverWOpens == FALSE){
      leafletProxy("mapAddData") %>% clearGroup(group = "opens") 
    }
  })
  
  observe(label = "ProDev Load",{
    req(input$ProDevshapefile)
    ProDevfiles <- input$ProDevshapefile
    NfilesProDev <- length(ProDevfiles$datapath)
    data_listProDev <- lapply(1:NfilesProDev, function(i) {return(st_read(paste0("/vsizip/", file.path(ProDevfiles$datapath[i]))))})    
    # Combine all data frames into one
    ProDev_sf <<- bind_rows(data_listProDev)
    ProDev_clean <<- ProDev_sf |> st_drop_geometry() |> as_tibble()
    
    print("Checking data for errors...")
    u = st_equals(ProDev_sf, retain_unique = TRUE)
    if(length(unique(unlist(u)))!=0) print(paste0("Found ", length(unique(unlist(u))), "points with duplicated geometry"))
    
    print("shapefile ready")
  }) %>%
    bindEvent(input$ProDevshapefile)
  
  observe(label = "ProDev Map Load",{
    req(input$ProDevshapefile)
    if(input$OverWProDev == TRUE) {
      leafletProxy("mapAddData") %>% clearGroup(group = "prodev") %>%
        addCircleMarkers(data = ProDev_sf |> st_transform(crs = 4326), group = "prodev", color = "blue", weight = 1) %>%
        flyToBounds(lng1 = Rbbox()$xmin, lat1 = Rbbox()$ymin, lng2 = Rbbox()$xmax, lat2 = Rbbox()$ymax)
    } else if(input$OverWProDev == FALSE){
      leafletProxy("mapAddData") %>% clearGroup(group = "prodev") 
    }
  })
  
  observe(label = "Trans Load",{
    req(input$Transshapefile)
    Metersfiles <- input$Transshapefile
    NfilesMeters <- length(Metersfiles$datapath)
    data_listMeters <- lapply(1:NfilesMeters, function(i) {return(st_read(paste0("/vsizip/", file.path(Metersfiles$datapath[i]))))})    
    # Combine all data frames into one
    Trans_sf <<- bind_rows(data_listMeters)
    Trans_clean <<- Trans_sf |> st_drop_geometry() |> as_tibble()
    
    print("Checking data for errors...")
    u = st_equals(Trans_sf, retain_unique = TRUE)
    if(length(unique(unlist(u)))!=0) print(paste0("Found ", length(unique(unlist(u))), "points with duplicated geometry"))
    
    
    print("shapefile ready")
  }) %>%
    bindEvent(input$Transshapefile)
  
  
  
  observe(label = "Trans Map Load",{
    req(input$Transshapefile)
    if(input$OverWTrans == TRUE) {
      leafletProxy("mapAddData") %>% clearGroup(group = "trans") %>%
        addCircleMarkers(data = Trans_sf |> st_transform(crs = 4326), group = "trans", color = "black", weight = 2) %>%
        flyToBounds(lng1 = Rbbox()$xmin, lat1 = Rbbox()$ymin, lng2 = Rbbox()$xmax, lat2 = Rbbox()$ymax)
    } else if(input$OverWTrans == FALSE){
      leafletProxy("mapAddData") %>% clearGroup(group = "trans") 
    }
  })  
  
  observe(label = "Meters Load",{
    req(input$Metersshapefile)
    Metersfiles <- input$Metersshapefile
    NfilesMeters <- length(Metersfiles$datapath)
    data_listMeters <- lapply(1:NfilesMeters, function(i) {return(st_read(paste0("/vsizip/", file.path(Metersfiles$datapath[i]))))})    
    # Combine all data frames into one
    meters_sf <<- bind_rows(data_listMeters)
    meters_clean <<- meters_sf |> st_drop_geometry() |> as_tibble()
    
    print("Checking data for errors...")
    u = st_equals(meters_sf, retain_unique = TRUE)
    if(length(unique(unlist(u)))!=0) print(paste0("Found ", length(unique(unlist(u))), "points with duplicated geometry"))
    
    
    print("shapefile ready")
  }) %>%
    bindEvent(input$Metersshapefile)
  
  
  
  observe(label = "Meters Map Load",{
    req(input$Metersshapefile)
    if(input$OverWmeters == TRUE) {
      leafletProxy("mapAddData") %>% clearGroup(group = "meters") %>%
        addCircleMarkers(data = meters_sf |> st_transform(crs = 4326), group = "meters", color = "lightgreen", weight = 1) %>%
        flyToBounds(lng1 = Rbbox()$xmin, lat1 = Rbbox()$ymin, lng2 = Rbbox()$xmax, lat2 = Rbbox()$ymax)
    } else if(input$OverWmeters == FALSE){
      leafletProxy("mapAddData") %>% clearGroup(group = "meters") 
    }
  })
  
  output$t_cond <- renderTable({
    req(input$Condshapefile)
    class_info = sapply(conductor_clean, class)
    conductor_clean = conductor_clean |> mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    colnames(conductor_clean) = paste0(colnames(conductor_clean), " (",class_info,")")
    head(conductor_clean) #[, input$columns, drop = FALSE]
    })
  output$t_cond_sel <- renderTable({
    req(input$Condshapefile)
    dcols = removeNone(c(input$selConVoltage,input$selConPhase,input$selConFeeder,input$selConOther))
    if(length(dcols)!=0) {select_dat = conductor_sf[, dcols, drop = FALSE] |>
      mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    
    
    Con_Voltage = if(input$selConVoltage!="None") "Voltage" else "None"
    Con_Phase = if(input$selConPhase!="None") "Phase" else "None"
    Con_Feeder = if(input$selConFeeder!="None") "Feeder" else "None"
    #print(length(removeNone(input$selConOther)))
    Con_Other = if(length(removeNone(input$selConOther))==0)  {"None"
      } else if(removeNone(input$selConOther)[1]=="") {"None"
        } else paste0("Cond_Col_Extra_",1:length(removeNone(c(input$selConOther))))
    colnames(select_dat) <- removeNone(c(Con_Voltage,Con_Phase,Con_Feeder,Con_Other, "geom"))
    st_geometry(select_dat) <- "geom"
    
    conductor_import <<- select_dat
    
    select_dat <- select_dat |> head() |> st_drop_geometry()
    collookupCon <<- data.frame(oldcol = colnames(select_dat),newcol = colnames(select_dat))
    print(paste0("Cond",input$Renamecon))
    if(input$Renamecon == TRUE) {
    input_values <- sapply(removeNone(input$selConOther), function(id) input[[id]])
    print(input_values)
    paste0("Cond_Col_Extra_",1:length(removeNone(c(input$selConOther))))
    paste0(input_values)
    collookupCon <<- data.frame(oldcol = paste0("Cond_Col_Extra_",1:length(removeNone(c(input$selConOther)))),newcol = input_values)
    colnames(select_dat)[colnames(select_dat) %in% collookupCon$oldcol] <- collookupCon$newcol[collookupCon$oldcol %in% colnames(select_dat)]
    }
    select_dat
    }
  })
  
  output$t_condsec <- renderTable({
    req(input$Condsecshapefile)
    class_info = sapply(conductorsec_clean, class)
    conductor_clean = conductorsec_clean |> mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    colnames(conductor_clean) = paste0(colnames(conductor_clean), " (",class_info,")")
    head(conductor_clean) #[, input$columns, drop = FALSE]
  })
  output$t_condsec_sel <- renderTable({
    req(input$Condsecshapefile)
    dcols = removeNone(c(input$selConsecVoltage,input$selConsecPhase,input$selConsecFeeder,input$selConsecOther))
    if(length(dcols)!=0) {select_dat = conductorsec_sf[, dcols, drop = FALSE] |>
      mutate_if(is.Date,~format(.,"%Y-%m-%d"))
  print(colnames(select_dat))
    ConSec_Voltage = if(input$selConsecVoltage!="None") "Voltage" else "None"
    ConSec_Phase = if(input$selConsecPhase!="None") "Phase" else "None"
    ConSec_Feeder = if(input$selConsecFeeder!="None") "Feeder" else "None"
    ConSec_Other = if(length(removeNone(input$selConsecOther))==0)  {"None"
      } else if(removeNone(input$selConsecOther)[1]=="") {"None"
        } else paste0("Cond_Col_Extra_",1:length(removeNone(c(input$selConsecOther))))
    #ConSec_Other = if(!is.null(input$selConsecOther)|removeNone(input$selConsecOther)[1]!="None") paste0("Col_Extra_",1:length(removeNone(c(input$selConsecOther)))) else "None"
    print(removeNone(c(ConSec_Voltage,ConSec_Phase,ConSec_Feeder,ConSec_Other)))
    colnames(select_dat) <- removeNone(c(ConSec_Voltage,ConSec_Phase,ConSec_Feeder,ConSec_Other, "geom"))
    st_geometry(select_dat) <- "geom"
    
    conductorsec_import <<- select_dat
    
    select_dat <- select_dat |> head() |> st_drop_geometry()
    collookupConsec <<- data.frame(oldcol = colnames(select_dat),newcol = colnames(select_dat))
    print(paste0("CondSec",input$Renameconsec))
    if(input$Renameconsec == 1) {
      input_values <- sapply(removeNone(input$selConsecOther), function(id) input[[id]])
      
      collookupConsec <<- data.frame(oldcol = paste0("Cond_Col_Extra_",1:length(removeNone(c(input$selConsecOther)))),newcol = input_values)
      colnames(select_dat)[colnames(select_dat) %in% collookupConsec$oldcol] <- collookupConsec$newcol[collookupConsec$oldcol %in% colnames(select_dat)]
    }
    select_dat    
    }
  })
  
  
  output$t_FS <- renderTable({
    req(input$StPtsshapefile)
    class_info = sapply(startingPts_clean, class)
    startingPts_clean = startingPts_clean |> mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    colnames(startingPts_clean) = paste0(colnames(startingPts_clean), " (",class_info,")") 
    head(startingPts_clean) #[, input$columns, drop = FALSE]
  })
  output$t_FS_sel <- renderTable({
    req(input$StPtsshapefile)
    dcols = removeNone(c(input$selFSID,input$selFSVoltage,input$selFSOther))
    if(length(dcols)!=0) {select_dat = startingPts_sf[, dcols, drop = FALSE] |>
      mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    FS_Feeder = if(input$selFSID!="None") "Feeder" else "None"
    FS_FeederkV = if(input$selFSVoltage!="None") "FeederkV" else "None"
    #FS_Other = if(!is.null(input$selFSOther)|removeNone(input$selFSOther)[1]!="None") paste0("Col_Extra_",1:length(removeNone(c(input$selFSOther)))) else "None"
    FS_Other = if(length(removeNone(input$selFSOther))==0)  {"None"
    } else if(removeNone(input$selFSOther)[1]=="") {"None"
    } else paste0("SP_Col_Extra_",1:length(removeNone(c(input$selFSOther))))
    
    print(colnames(select_dat))
    print(removeNone(c(FS_Feeder,FS_FeederkV,FS_Other, "geom")))
    colnames(select_dat) <- removeNone(c(FS_Feeder,FS_FeederkV,FS_Other, "geom"))
    st_geometry(select_dat) <- "geom"
    startingPts_import <<- select_dat
    
    select_dat <- select_dat |> head() |> st_drop_geometry()
    collookupFS <<- data.frame(oldcol = colnames(select_dat),newcol = colnames(select_dat))
    print(paste0("FS",input$Renamestpt))
    if(input$Renamestpt == 1) {
      input_values <- sapply(removeNone(input$selFSOther), function(id) input[[id]])
      
      print(paste0("SP_Col_Extra_",1:length(removeNone(c(input$selFSOther)))))
      print(input_values)
      
      
      collookupFS <<- data.frame(oldcol = paste0("SP_Col_Extra_",1:length(removeNone(c(input$selFSOther)))),newcol = input_values)
      colnames(select_dat)[colnames(select_dat) %in% collookupFS$oldcol] <- collookupFS$newcol[collookupFS$oldcol %in% colnames(select_dat)]
    }
    select_dat
    }
    #print(paste0("There are ", duplicated(ProDev_clean[[input$selFSID]]), " feeder starting points with duplicated ID numbers."))
})
  
  output$t_Ope <- renderTable({
    req(input$Opensshapefile)
    class_info = sapply(opens_clean, class)
    opens_clean = opens_clean |> mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    colnames(opens_clean) = paste0(colnames(opens_clean), " (",class_info,")") 
    head(opens_clean) #[, input$columns, drop = FALSE]
  })
  output$t_Ope_sel <- renderTable({
    req(input$Opensshapefile)
    dcols = removeNone(c(input$selOpeID,input$selOpePos,input$selOpeOther))
    if(length(dcols)!=0) {select_dat = opens_sf[, dcols, drop = FALSE] |>
      mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    
    Open_ID = if(input$selOpeID!="None") "ID" else "None"
    Open_Pos = if(input$selOpePos!="None") "Pos" else "None"
    Open_Other = if(length(removeNone(input$selOpeOther))==0)  {"None"
    } else if(removeNone(input$selOpeOther)[1]=="") {"None"
    } else paste0("Open_Col_Extra_",1:length(removeNone(c(input$selOpeOther))))
    #ConSec_Other = if(!is.null(input$selConsecOther)|removeNone(input$selConsecOther)[1]!="None") paste0("Col_Extra_",1:length(removeNone(c(input$selConsecOther)))) else "None"
    
    colnames(select_dat) <- removeNone(c(Open_ID,Open_Pos,Open_Other,"geom"))
    st_geometry(select_dat) <- "geom"
    opens_import <<- select_dat
    
    select_dat <- select_dat |> head() |> st_drop_geometry()
    collookupOpen <<- data.frame(oldcol = colnames(select_dat),newcol = colnames(select_dat))
    print(paste0("Opens",input$RenameOpe))
    if(input$RenameOpe == 1) {
      input_values <- sapply(removeNone(input$selOpeOther), function(id) input[[id]])
      
      collookupOpen <<- data.frame(oldcol = paste0("Open_Col_Extra_",1:length(removeNone(c(input$selOpeOther)))),newcol = input_values)
      colnames(select_dat)[colnames(select_dat) %in% collookupOpen$oldcol] <- collookupOpen$newcol[collookupOpen$oldcol %in% colnames(select_dat)]
    }
    select_dat
    }
    
    #print(paste0("There are ", duplicated(ProDev_clean[[input$selOpeID]]), " open point devices with duplicated ID numbers."))
  })
  
  output$t_PD <- renderTable({
    req(input$ProDevshapefile)
    class_info = sapply(ProDev_clean, class)
    ProDev_clean = ProDev_clean |> mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    colnames(ProDev_clean) = paste0(colnames(ProDev_clean), " (",class_info,")") 
    head(ProDev_clean) #[, input$columns, drop = FALSE]
  })
  output$t_PD_sel <- renderTable({
    req(input$ProDevshapefile)
    dcols = removeNone(c(input$selPDID,input$selPDPosition,input$selPDOther))
    if(length(dcols)!=0) {select_dat = ProDev_sf[, dcols, drop = FALSE] |>
      mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    
    ProDev_ID = if(input$selPDID!="None") "ID" else "None"
    #ProDev_Pos = if(input$selPDPosition!="None") "Pos" else "None"
    ProDev_Other = if(length(removeNone(input$selPDOther))==0)  {"None"
    } else if(removeNone(input$selPDOther)[1]=="") {"None"
    } else paste0("MP_Col_Extra_",1:length(removeNone(c(input$selPDOther))))
    #ConSec_Other = if(!is.null(input$selConsecOther)|removeNone(input$selConsecOther)[1]!="None") paste0("Col_Extra_",1:length(removeNone(c(input$selConsecOther)))) else "None"
    
    colnames(select_dat) <- removeNone(c(ProDev_ID,ProDev_Other, "geom"))
    st_geometry(select_dat) <- "geom"
    ProDev_import <<- select_dat
    
    select_dat <- select_dat |> head() |> st_drop_geometry()
    collookupPD <<- data.frame(oldcol = colnames(select_dat),newcol = colnames(select_dat))
    print(paste0("PD",input$RenamePD))
    if(input$RenamePD == 1) {
      input_values <- sapply(removeNone(input$selPDOther), function(id) input[[id]])
      
      collookupPD <<- data.frame(oldcol = paste0("MP_Col_Extra_",1:length(removeNone(c(input$selPDOther)))),newcol = input_values)
      colnames(select_dat)[colnames(select_dat) %in% collookupPD$oldcol] <- collookupPD$newcol[collookupPD$oldcol %in% colnames(select_dat)]
    }
    select_dat
    }
    
    #print(paste0("There are ", duplicated(ProDev_clean[[input$selPDID]]), " monitoring devices with duplicated ID numbers."))
  })
  
  output$t_trans <- renderTable({
    req(input$Transshapefile)
    class_info = sapply(Trans_clean, class)
    Trans_clean = Trans_clean |> mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    colnames(Trans_clean) = paste0(colnames(Trans_clean), " (",class_info,")") 
    head(Trans_clean) #[, input$columns, drop = FALSE]
  })
  output$t_trans_sel <- renderTable({
    req(input$Transshapefile)
    dcols = removeNone(c(input$selTraID,input$selTraKVA,input$selTraPhase,input$selTraPriVolt,input$selTraSecVolt,input$selTraFeeder,input$selTraProDev,input$selTraOther))
    if(length(dcols)!=0) {select_dat = Trans_sf[, dcols, drop = FALSE] |>
    mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    
    Trans_ID = if(input$selTraID!="None") "ID" else "None"
    Trans_kVA = if(input$selTraKVA!="None") "TranskVA" else "None"
    Trans_Phase = if(input$selTraPhase!="None") "TransPhase" else "None"
    Trans_PriVolt = if(input$selTraPriVolt!="None") "TransPriVolt" else "None"
    Trans_SecVolt = if(input$selTraSecVolt!="None") "TransSecVolt" else "None"
    Trans_Feeder = if(input$selTraFeeder!="None") "TransFeeder" else "None"
    #Trans_ProDev = if(input$selTraProDev!="None") "TransProDev" else "None"
    Trans_Other = if(length(removeNone(input$selTraOther))==0)  {"None"
    } else if(removeNone(input$selTraOther)[1]=="") {"None"
    } else paste0("Trans_Col_Extra_",1:length(removeNone(c(input$selTraOther))))
    #ConSec_Other = if(!is.null(input$selConsecOther)|removeNone(input$selConsecOther)[1]!="None") paste0("Col_Extra_",1:length(removeNone(c(input$selConsecOther)))) else "None"
    
    colnames(select_dat) <- removeNone(c(Trans_ID,Trans_kVA,Trans_Phase,Trans_PriVolt,Trans_SecVolt,Trans_Feeder,Trans_Other, "geom"))
    st_geometry(select_dat) <- "geom"
    Trans_import <<- select_dat
    
    select_dat <- select_dat |> head() |> st_drop_geometry()
    collookupTrans <<- data.frame(oldcol = colnames(select_dat),newcol = colnames(select_dat))
    print(input$RenameTrans)
    if(input$RenameTrans == 1) {
      input_values <- sapply(removeNone(input$selTraOther), function(id) input[[id]])
      
      collookupTrans <<- data.frame(oldcol = paste0("Trans_Col_Extra_",1:length(removeNone(c(input$selTraOther)))),newcol = input_values)
      colnames(select_dat)[colnames(select_dat) %in% collookupTrans$oldcol] <- collookupTrans$newcol[collookupTrans$oldcol %in% colnames(select_dat)]
    }
    select_dat
    }
    
    #print(paste0("There are ", duplicated(Trans_clean[[input$selTraID]]), " transformers with duplicated ID numbers."))
  })


output$t_meter <- renderTable({
  req(input$Metersshapefile)
  class_info = sapply(meters_clean, class)
  meters_clean = meters_clean |> mutate_if(is.Date,~format(.,"%Y-%m-%d"))
  colnames(meters_clean) = paste0(colnames(meters_clean), " (",class_info,")") 
  head(meters_clean) #[, input$columns, drop = FALSE]
})


output$t_meter_sel <- renderTable({
  req(input$Metersshapefile)
  req(input$selMetSubtype)
  #req(input$RenameMeter)
  #print(input$selMetSubtype)
  #print(removeNone(input$selMetSubtype))
  dcols = removeNone(c(input$selMetID,input$selMetSubtype,input$selMetFeeder,input$selMetTrans,input$selMetProDev,input$selMetLoad,input$selMetOther))
  if(length(removeNone(input$selMetSubtype))>=1) {
      if(input$r_metertype_method==2) {
      subtypeCol = meters_sf[, removeNone(input$selMetSubtype), drop = FALSE] |> st_drop_geometry()
      subtypeCol[subtypeCol[[1]] %in% input$selMetTypeUseCols,] <- "Usage"
      subtypeCol[subtypeCol[[1]] %in% input$selMetTypeSolCols,] <- "Prod"
      colnames(subtypeCol) <- c("Type")
    } else {
      subtypeCol = meters_sf[, removeNone(input$selMetTypeCol), drop = FALSE] |> st_drop_geometry()
      if(length(colnames(subtypeCol))==1) colnames(subtypeCol) = c("Type")
    }
  } else {
    subtypeCol = NULL
  }
  
  if(length(dcols)!=0) {
    dat_temp = meters_sf[, dcols, drop = FALSE]
    if(is.null(subtypeCol)) {
      df_final = dat_temp
    } else {
      df_final = cbind(subtypeCol, dat_temp)
    }
    select_dat = df_final |>
      mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    
    Meter_Type = if(input$selMetTypeUseCols[1]!="None" | input$selMetTypeSolCols[1]!="None") "Type" else "None"
    Meter_ID = if(input$selMetID!="None") "ID" else "None"
    Meter_Sub = if(input$selMetSubtype!="None") "Subtype" else "None"
    Meter_Feeder = if(input$selMetFeeder!="None") "Feeder" else "None"
    Meter_Trans = if(input$selMetTrans!="None") "Transformer" else "None"
    Meter_ProDev = if(input$selMetProDev!="None") "Protective Device" else "None"
    Meter_Load = if(input$selMetLoad!="None") "LoadkVA" else "None"
    Meter_Other = if(length(removeNone(input$selMetOther))==0)  {"None"
    } else if(removeNone(input$selMetOther)[1]=="") {"None"
    } else paste0("Meter_Col_Extra_",1:length(removeNone(c(input$selMetOther))))
    #ConSec_Other = if(!is.null(input$selConsecOther)|removeNone(input$selConsecOther)[1]!="None") paste0("Col_Extra_",1:length(removeNone(c(input$selConsecOther)))) else "None"
    
    
    #print(removeNone(c(Meter_Type,Meter_ID,Meter_Sub,Meter_Feeder,Meter_Trans,Meter_ProDev,Meter_Load,Meter_Other, "geom")))
    #print(colnames(select_dat))
    colnames(select_dat) <- removeNone(c(Meter_Type,Meter_ID,Meter_Sub,Meter_Feeder,Meter_Trans,Meter_ProDev,Meter_Load,Meter_Other, "geom"))
    st_geometry(select_dat) <- "geom"
    meters_import <<- select_dat
    
    
    select_dat <- select_dat |> head() |> st_drop_geometry()
    collookupMeter <<- data.frame(oldcol = colnames(select_dat),newcol = colnames(select_dat))
    print(input$Renamemeter)                                    
    #if(is.null(input$Renamemeter)) {print("NULL because of startup")
    #} else 
    #if(input$Renamemeter == 1) {
      input_values <- sapply(removeNone(input$selMetOther), function(id) input[[id]])
      if(!is.null(input_values[[1]])) {
      print(input_values)
      print(colnames(input_values))
      print(paste0("Meter_Col_Extra_",1:length(removeNone(c(input$selMetOther)))))
      collookupMeter <<- data.frame(oldcol = paste0("Meter_Col_Extra_",1:length(removeNone(c(input$selMetOther)))),newcol = input_values)
      colnames(select_dat)[colnames(select_dat) %in% collookupMeter$oldcol] <- collookupMeter$newcol[collookupMeter$oldcol %in% colnames(select_dat)]
    }
    select_dat
  }
    
    #print(paste0("There are ", duplicated(df_final[[input$selMetID]]), " meters with duplicated meter ID numbers."))
})

#input_values_meters <- reactive(sapply(removeNone(input$selMetOther), function(id) input[[id]]))

#observe({
#  
#  conductor_import <<- conductor_sf[, removeNone(c(input$selConVoltage,input$selConPhase,input$selConFeeder,input$selConOther)), drop = FALSE]
#  conductorsec_import <<- conductorsec_sf[, removeNone(c(input$selConsecVoltage,input$selConsecPhase,input$selConsecFeeder,input$selConsecOther)), drop = FALSE]
#  startingPts_import <<- startingPts_sf[, removeNone(c(input$selFSID,input$selFSVoltage,input$selFSOther)), drop = FALSE]
#  opens_import <<- opens_sf[, removeNone(c(input$selOpeID,input$selOpePos,input$selOpeOther)), drop = FALSE]
#  ProDev_import <<- ProDev_sf[, removeNone(c(input$selPDID,input$selPDPosition,input$selPDOther)), drop = FALSE]
#  Trans_import <<- Trans_sf[, removeNone(c(input$selTraID,input$selTraKVA,input$selTraPhase,input$selTraPriVolt,input$selTraSecVolt,input$selTraFeeder,input$selTraProDev,input$selTraOther)), drop = FALSE]
#  
#  if(input$r_metertype_method==2) {
#    subtypeCol = meters_clean[, removeNone(input$selMetSubtype), drop = FALSE]
#    subtypeCol[subtypeCol[[1]] %in% input$selMetTypeUseCols,] <- "Usage"
#    subtypeCol[subtypeCol[[1]] %in% input$selMetTypeSolCols,] <- "Prod"
#    colnames(subtypeCol) <- c("Type")
#  } else {
#    subtypeCol = meters_clean[, removeNone(input$selMetTypeCol), drop = FALSE]
#    colnames(subtypeCol) = c("Type")
#  }
#  
#  dat_temp = meters_sf[, removeNone(c(input$selMetID,input$selMetSubtype,input$selMetFeeder,input$selMetTrans,input$selMetProDev,input$selMetLoad,input$selMetOther)), drop = FALSE]
#  meters_import <<- cbind(subtypeCol, dat_temp) |> st_as_sf()
#  
#  Voltage
#}) |> 
#  bindEvent(input$collectDat)




input_ids <- list("selConVoltage","selConPhase","selConFeeder","selConOther",
                  "selConsecVoltage","selConsecPhase","selConsecFeeder","selConsecOther",
                  "selFSID","selFSVoltage","selFSOther","selOpeID",
                  "selOpePos","selOpeOther","selPDID","selPDPosition",
                  "selPDOther","selTraID","selTraKVA","selTraPhase",
                  "selTraPriVolt","selTraSecVolt","selTraFeeder","selTraProDev",
                  "selTraOther","selMetID","selMetSubtype","r_metertype_method",
                  "selMetFeeder","selMetTypeCol","selMetTypeUseCols","selMetTypeSolCols",
                  "selMetTrans","selMetProDev","selMetLoad","selMetLoad","selMetOther")

#observeEvent(input$importData, {
#  input_values <- lapply(input_ids, function(id) input[[id]])
#  names(input_values) <- input_ids
#  data_inputs <<- input_values
#})


output$downloadData <- downloadHandler(
  filename = "config.json",
  content = function(file) {
    input_values <- lapply(input_ids, function(id) input[[id]])
    names(input_values) <- input_ids
    data_inputs <<- input_values
    write_json(data_inputs, file)
  }
)

observeEvent(input$change, {
  print(input$fileinputs)
  
  config_RAW <- input$fileinputs
  replacements <<- fromJSON(file.path(config_RAW$datapath))
  
  #updateTextInput(inputId = "selConVoltage", value = replacements$selConVoltage)
  
})



output$UI_ConsselVolt <- renderUI({
  req(input$Condshapefile)
  selectInput("selConVoltage", "Choose Voltage Column:", 
              choices = c("None",names(conductor_sf)), 
              selected = ifelse(exists("replacements"), replacements$selConVoltage ,"None"),
              multiple = FALSE)
})
output$UI_ConsselPhase <- renderUI({
  req(input$Condshapefile)
  selectInput("selConPhase", "Choose Phase Column:", 
              choices = c("None",names(conductor_sf)),
              selected = ifelse(exists("replacements"), replacements$selConPhase ,"None"),
              multiple = FALSE)
})
output$UI_ConsselFeeder <- renderUI({
  req(input$Condshapefile)
  selectInput("selConFeeder", "Choose Feeder Column:", 
              choices = c("None",names(conductor_sf)),
              selected = ifelse(exists("replacements"), replacements$selConFeeder ,"None"),
              multiple = FALSE)
})
output$UI_ConsselOther <- renderUI({
  req(input$Condshapefile)
  selectInput("selConOther", "Choose Other Columns:", 
              choices = c("None",names(conductor_sf)),
              selected = if(exists("replacements")) {
                replacements$selConOther
              } else {
                "None"
              },
              multiple = TRUE)
})
output$UI_ConsselRNBox <- renderUI({
  req(input$Condshapefile)
  checkboxInput("Renamecon", "Rename Extra Columns", FALSE)
})
output$UI_ConSelNames <- renderUI({
  req(input$Condshapefile)
  req(input$Renamecon)
  conditionalPanel(
    condition = "input.Renamecon == '1'",
    #req(input$selMetSubtype)
    createTextInputs(removeNone(input$selConOther), paste0("Cond_Col_Extra_",1:length(removeNone(c(input$selConOther)))))$ui
  )
})


output$UI_ConSecVolt <- renderUI({
  req(input$Condsecshapefile)
    selectInput("selConsecVoltage", "Choose Voltage Column:", 
                choices = c("None",names(conductorsec_sf)),
                selected = ifelse(exists("replacements"), replacements$selConsecVoltage ,"None"),
                multiple = FALSE)
})
    
output$UI_ConSecPhase <- renderUI({
  req(input$Condsecshapefile)
    selectInput("selConsecPhase", "Choose Phase Column:", 
                choices = c("None",names(conductorsec_sf)),
                selected = ifelse(exists("replacements"), replacements$selConsecPhase ,"None"),
                multiple = FALSE)
})

output$UI_ConSecFeeder <- renderUI({
  req(input$Condsecshapefile)
    selectInput("selConsecFeeder", "Choose Feeder Column:", 
                choices = c("None",names(conductorsec_sf)),
                selected = ifelse(exists("replacements"), replacements$selConsecFeeder ,"None"),
                multiple = FALSE)
})

output$UI_ConSecOther <- renderUI({
  req(input$Condsecshapefile)
    selectInput("selConsecOther", "Choose Other Columns:", 
                choices = c("None",names(conductorsec_sf)),
                selected = if(exists("replacements")) {
                  replacements$selConsecOther
                } else {
                  "None"
                },
                multiple = TRUE)
    
})
output$UI_ConsecselRNBox <- renderUI({
  req(input$Condshapefile)
  checkboxInput("Renameconsec", "Rename Extra Columns", FALSE)
})

output$UI_ConsecSelNames <- renderUI({
  req(input$Condshapefile)
  req(input$Renameconsec)
  conditionalPanel(
    condition = "input.Renameconsec == '1'",
    #req(input$selMetSubtype)
    createTextInputs(removeNone(input$selConsecOther), paste0("Cond_Col_Extra_",1:length(removeNone(c(input$selConsecOther)))))$ui
  )
})



output$UI_StPtsID <- renderUI({
  req(input$StPtsshapefile)
    selectInput("selFSID", "Choose ID Column*:", 
                choices = c("None",names(startingPts_sf)),
                selected = ifelse(exists("replacements"), replacements$selFSID ,"None"),
                multiple = FALSE)
})
    
output$UI_StPtsVolt <- renderUI({
  req(input$StPtsshapefile)  
    selectInput("selFSVoltage", "Choose Voltage Column*:", 
                choices = c("None",names(startingPts_sf)),
                selected = ifelse(exists("replacements"), replacements$selFSVoltage ,"None"),
                multiple = FALSE)
    
})
  
output$UI_StPtsOther <- renderUI({
  req(input$StPtsshapefile)
  selectInput("selFSOther", "Choose Other Columns:", 
                choices = c("None",names(startingPts_sf)),
                selected = if(exists("replacements")) {
                  replacements$selFSOther
                } else {
                  "None"
                },
                multiple = TRUE)
  })
output$UI_StPtselRNBox <- renderUI({
  req(input$StPtsshapefile)
  checkboxInput("Renamestpt", "Rename Extra Columns", FALSE)
})
output$UI_StPtSelNames <- renderUI({
  req(input$StPtsshapefile)
  req(input$Renameconsec)
  conditionalPanel(
    condition = "input.Renamestpt == '1'",
    #req(input$selMetSubtype)
    createTextInputs(removeNone(input$selFSOther), paste0("SP_Col_Extra_",1:length(removeNone(c(input$selFSOther)))))$ui
  )
})



output$UI_OpensID <- renderUI({
  req(input$Opensshapefile)
    selectInput("selOpeID", "Choose ID Column:", 
                choices = c("None",names(opens_sf)),
                selected = ifelse(exists("replacements"), replacements$selOpeID ,"None"),
                multiple = FALSE)
})
output$UI_OpensPos <- renderUI({
      req(input$Opensshapefile)
    selectInput("selOpePos", "Choose Position Column:", 
                choices = c("None",names(opens_sf)),
                selected = ifelse(exists("replacements"), replacements$selOpePos ,"None"),
                multiple = FALSE)
})
output$UI_OpensOther <- renderUI({
      req(input$Opensshapefile)
    selectInput("selOpeOther", "Choose Other Columns:", 
                choices = c("None",names(opens_sf)),
                selected = if(exists("replacements")) {
                  replacements$selOpeOther
                } else {
                  "None"
                },
                multiple = TRUE)
})
output$UI_OpeselRNBox <- renderUI({
  req(input$Opensshapefile)
  checkboxInput("RenameOpe", "Rename Extra Columns", FALSE)
})
output$UI_OpeSelNames <- renderUI({
  req(input$Opensshapefile)
  req(input$RenameOpe)
  conditionalPanel(
    condition = "input.RenameOpe == '1'",
    #req(input$selMetSubtype)
    createTextInputs(removeNone(input$selOpeOther), paste0("Open_Col_Extra_",1:length(removeNone(c(input$selOpeOther)))))$ui
  )
})
output$UI_ProDevID <- renderUI({
  req(input$ProDevshapefile)
    selectInput("selPDID", "Choose ID Column:", 
                choices = c("None",names(ProDev_sf)),
                selected = ifelse(exists("replacements"), replacements$selPDID ,"None"),
                multiple = FALSE)
})
    #selectInput("selPDPosition", "Choose Position Column:", 
    #            choices = c("None",names(ProDev_sf)),
    #            selected = ifelse(exists("replacements"), replacements$selPDPosition ,"None"),
    #            multiple = FALSE),
output$UI_ProDevOther <- renderUI({
      req(input$ProDevshapefile)
    selectInput("selPDOther", "Choose Other Columns:", 
                choices = c("None",names(ProDev_sf)),
                selected = if(exists("replacements")) {
                  replacements$selPDOther
                } else {
                  "None"
                },
                multiple = TRUE)
})
output$UI_PDselRNBox <- renderUI({
  req(input$Opensshapefile)
  checkboxInput("RenamePD", "Rename Extra Columns", FALSE)
})
output$UI_PDSelNames <- renderUI({
  req(input$Opensshapefile)
  req(input$RenamePD)
  conditionalPanel(
    condition = "input.RenamePD == '1'",
    #req(input$selMetSubtype)
    createTextInputs(removeNone(input$selPDOther), paste0("MP_Col_Extra_",1:length(removeNone(c(input$selPDOther)))))$ui
  )
})





output$UI_TransID <- renderUI({
  req(input$Transshapefile)
    selectInput("selTraID", "Choose ID Column:", 
                choices = c("None",names(Trans_sf)),
                selected = ifelse(exists("replacements"), replacements$selTraID ,"None"),
                multiple = FALSE)
})
output$UI_TransKVA <- renderUI({
  req(input$Transshapefile)
    selectInput("selTraKVA", "Choose KVA Column:", 
                choices = c("None",names(Trans_sf)),
                selected = ifelse(exists("replacements"), replacements$selTraKVA ,"None"),
                multiple = FALSE)
})
output$UI_TransPhase <- renderUI({
  req(input$Transshapefile)    
selectInput("selTraPhase", "Choose Phase Column:", 
                choices = c("None",names(Trans_sf)),
                selected = ifelse(exists("replacements"), replacements$selTraPhase ,"None"),
                multiple = FALSE)
    })
output$UI_TransVolt <- renderUI({
  req(input$Transshapefile)    
selectInput("selTraPriVolt", "Choose Primary Voltage ID Column:", 
                choices = c("None",names(Trans_sf)),
                selected = ifelse(exists("replacements"), replacements$selTraPriVolt ,"None"),
                multiple = FALSE)
    })
output$UI_TransSec <- renderUI({
  req(input$Transshapefile)    
selectInput("selTraSecVolt", "Choose Secondary Voltage Column:", 
                choices = c("None",names(Trans_sf)),
                selected = ifelse(exists("replacements"), replacements$selTraSecVolt ,"None"),
                multiple = FALSE)
    })
output$UI_TransFeeder <- renderUI({
  req(input$Transshapefile)    
selectInput("selTraFeeder", "Choose Feeder ID Column:", 
                choices = c("None",names(Trans_sf)),
                selected = ifelse(exists("replacements"), replacements$selTraFeeder ,"None"),
                multiple = FALSE)
    })
    #selectInput("selTraProDev", "Choose Protective ID Column:", 
    #            choices = c("None",names(Trans_sf)),
    #            selected = ifelse(exists("replacements"), replacements$selTraProDev ,"None"),
    #            multiple = FALSE),
output$UI_TransOther <- renderUI({
  req(input$Transshapefile)    
selectInput("selTraOther", "Choose Other Columns:", 
                choices = c("None",names(Trans_sf)),
                selected = if(exists("replacements")) {
                  replacements$selTraOther
                } else {
                  "None"
                },
                multiple = TRUE)
})
output$UI_TransselRNBox <- renderUI({
  req(input$Transshapefile)    
  checkboxInput("RenameTrans", "Rename Extra Columns", FALSE)
})
output$UI_TransSelNames <- renderUI({
  req(input$Transshapefile)    
  req(input$RenameTrans)
  conditionalPanel(
    condition = "input.RenameTrans == '1'",
    #req(input$selMetSubtype)
    createTextInputs(removeNone(input$selTraOther), paste0("Trans_Col_Extra_",1:length(removeNone(c(input$selTraOther)))))$ui
  )
})




output$UI_MetersselID <- renderUI({
  req(input$Metersshapefile)
  selectInput("selMetID", "Choose ID Column:", 
              choices = c("None",names(meters_sf)),
              selected = ifelse(exists("replacements"), replacements$selMetID ,"None"),
              multiple = FALSE)
})
output$UI_MetersselST <- renderUI({
  req(input$Metersshapefile)
  selectInput("selMetSubtype", "Choose Subtype Column:", 
              choices = c("None",names(meters_sf)),
              selected = ifelse(exists("replacements"), replacements$selMetSubtype ,"None"),
              multiple = FALSE)
})
output$UI_MetersselTM <- renderUI({
  req(input$Metersshapefile)
  radioButtons( 
    inputId = "r_metertype_method", 
    label = "Meter Type Selection Method", 
    choices = list( 
      "Select Using Pre-formated column (Solar/Usage)" = 1, 
      "Define based on Subtype Column" = 2
    ),
    selected = ifelse(exists("replacements"), replacements$r_metertype_method ,1)
  )
})
output$UI_MetersselTC <- renderUI({
  req(input$Metersshapefile)
  req(input$selMetSubtype)
  conditionalPanel(
    condition = "input.r_metertype_method == '1'",
    #req(input$selMetSubtype)
    selectInput("selMetTypeCol", "Choose Type Column:", 
                choices = c("None",names(meters_sf)),
                selected = ifelse(exists("replacements"), replacements$selMetTypeCol ,"None"),
                multiple = FALSE)
  )
})
output$UI_MetersselUC <- renderUI({
  req(input$Metersshapefile)
  req(input$selMetSubtype)
  conditionalPanel(
    condition = "input.r_metertype_method == '2'",
    selectInput("selMetTypeUseCols", "Choose Usage Subtypes:", 
                choices = c("None",unique(meters_clean[[input$selMetSubtype]])),
                selected = ifelse(exists("replacements"), replacements$selMetTypeUseCols ,"None"),
                multiple = TRUE)
  )
})
output$UI_MetersselSC <- renderUI({
  req(input$Metersshapefile)
  req(input$selMetSubtype)
  conditionalPanel(
    condition = "input.r_metertype_method == '2'",
    selectInput("selMetTypeSolCols", "Choose Solar/Production Subtypes:", 
                choices = c("None",unique(meters_clean[[input$selMetSubtype]])),
                selected = ifelse(exists("replacements"), replacements$selMetTypeSolCols ,"None"),
                multiple = TRUE)
  )
})
observe({
  req(input$Metersshapefile)
  req(input$selMetSubtype)
  
  choices <- unique(meters_clean[[input$selMetSubtype]])
  
  selected1 <- input$selMetTypeUseCols
  selected2 <- input$selMetTypeSolCols
  
  updateSelectInput(session, "selMetTypeUseCols", choices = setdiff(choices, selected2), selected = selected1)
  updateSelectInput(session, "selMetTypeSolCols", choices = setdiff(choices, selected1), selected = selected2)
})
output$UI_MetersselFe <- renderUI({
  req(input$Metersshapefile)
  selectInput("selMetFeeder", "Choose Feeder ID Column:", 
              choices = c("None",names(meters_sf)),
              selected = ifelse(exists("replacements"), replacements$selMetFeeder ,"None"),
              multiple = FALSE)
})
output$UI_MetersselTr <- renderUI({
  req(input$Metersshapefile)
  selectInput("selMetTrans", "Choose Transformer ID Column:", 
              choices = c("None",names(meters_sf)),
              selected = ifelse(exists("replacements"), replacements$selMetTrans ,"None"),
              multiple = FALSE)
})
output$UI_MetersselPD <- renderUI({
  req(input$Metersshapefile)
  selectInput("selMetProDev", "Choose Protective ID Column:", 
              choices = c("None",names(meters_sf)),
              selected = ifelse(exists("replacements"), replacements$selMetProDev ,"None"),
              multiple = FALSE)
})
output$UI_MetersselPU <- renderUI({
  req(input$Metersshapefile)
  selectInput("selMetLoad", "Choose Load (kW) Column:", 
              choices = c("None",names(meters_sf)),
              selected = ifelse(exists("replacements"), replacements$selMetProDev ,"None"),
              multiple = FALSE)
})
output$UI_MetersselOth <- renderUI({
  req(input$Metersshapefile)
  selectInput("selMetOther", "Choose Other Columns:", 
              choices = c("None",names(meters_sf)),
              selected = if(exists("replacements")) {
                replacements$selMetOther
              } else {
                "None"
              },
              multiple = TRUE)
})  
output$UI_MetersselRNBox <- renderUI({
  req(input$Metersshapefile)
  #selectInput("RenameMeter", "Choose test true/false:", 
  #            choices = c(TRUE,FALSE),
  #            selected = FALSE,
  #            multiple = FALSE)
  checkboxInput("RenameMeter", "Rename Extra Columns", FALSE)
})
output$UI_MetersSelNames <- renderUI({
  req(input$Metersshapefile)
  req(input$RenameMeter)
  conditionalPanel(
    condition = "input.RenameMeter == '1'",
    #req(input$selMetSubtype),
    createTextInputs(removeNone(input$selMetOther), paste0("Meter_Col_Extra_",1:length(removeNone(c(input$selMetOther)))))$ui
  )
})
output$testtextoutput <- renderText({input$RenameMeter})


observe(label = "Meters Data Load",{
  req(input$meterdataload)
  withProgress(message = 'Upload Data to Database', value = 0, {
    setProgress(0, detail = "Starting processing data...")
  setProgress(0.2, detail = "Uploading data ...")  
  AMI_Data_RAW <- input$meterdataload
  print(paste0("/vsicsv/", file.path(AMI_Data_RAW$datapath)))
  AMI_Data <- AMI_Loader(file.path(AMI_Data_RAW$datapath))
  print(head(AMI_Data))
  #db_path  <- "~/R_trace2/R_trace2/R_Trace/appDep/AMI_Data.db"
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = "~/R_trace2/R_trace2/R_Trace/appDep/AMI_Data.db")
  print(con)
  setProgress(0.4, detail = "Writing data to DMS ...")
  dbWriteTable(con, "ElectricMeterReadings", AMI_Data, row.names = FALSE, append = TRUE)
  indexs <- dbGetQuery(con, "PRAGMA index_list('ElectricMeterReadings')")
  if(!length(indexs$seq)>0) dbExecute(con, "CREATE INDEX idx_acct_timestamp ON ElectricMeterReadings (AcctNo, Timestamp)")
  
  setProgress(0.75, detail = "Removing duplicates ...")
  #Removing duplicates
  #Identify dups
  duplicates_query <- "
  SELECT AcctNo, Timestamp, COUNT(*) as count
  FROM ElectricMeterReadings
  GROUP BY AcctNo, Timestamp
  HAVING count > 1
"
  duplicates <- dbGetQuery(con, duplicates_query)
  print(paste0(duplicates, ": duplicates removed"))
  
  
  remove_duplicates_query <- "DELETE FROM ElectricMeterReadings
  WHERE ROWID NOT IN (
    SELECT MIN(ROWID)
    FROM ElectricMeterReadings
    GROUP BY AcctNo, Timestamp
    )
  "
  setProgress(0.8, detail = "Removing duplicates ...")
  dbExecute(con, remove_duplicates_query)
  setProgress(1, detail = "Finished uploading data ...")
  })
}) %>%
  bindEvent(input$meterdataload)


observe(label = "Meters Clear Data",{
  req(input$clearmeterdata)
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = "~/R_trace2/R_trace2/R_Trace/appDep/AMI_Data.db")
  #Delete all rows from ElectricMetersReadings table
  dbExecute(con, "DELETE FROM ElectricMeterReadings")
  #Verify table is empty
  #dbGetQuery(con, "SELECT * FROM ElectricMeterReadings")  
}) %>%
  bindEvent(input$clearmeterdata)

observe(label = "Process Data",{
  req(input$proccessdata)
  withProgress(message = 'Update Opens', value = 0, {
    setProgress(0, detail = "Starting processing data...")
  source("~/R_trace2/R_trace2/Uploader/Compiler.R")
    setProgress(1, detail = "Finished processing data ...")
  })
}) %>%
  bindEvent(input$proccessdata)



}