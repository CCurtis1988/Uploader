#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(dplyr)
library(lubridate)
library(DT)
library(jsonlite)

ui <- navbarPage(theme = shinytheme("slate"), "Electric Dataset Processor", # Setting nice theme
                 tags$head(
                   tags$link(rel = "icon", type = "image/x-icon", href = "favicon.png")
                 ),
           tabPanel("Upload",sidebarLayout(
             sidebarPanel(h5("Supply shapefiles within zipfiles"),
                          fileInput("Condshapefile", "All Primary Conductor",
                                    multiple = TRUE,
                                    accept = c('.zip')),
                          checkboxInput("OverWcond", "Overwrite Existing", TRUE),
                          fileInput("Condsecshapefile", "All Secondary Conductor",
                                    multiple = TRUE,
                                    accept = c('.zip')),
                          #tags$hr(),
                          
                          checkboxInput("OverWseccond", "Overwrite Existing", TRUE),
                          fileInput("StPtsshapefile", "Feeder Starting Points",
                                    multiple = TRUE,
                                    accept = c('.zip')),
                          #tags$hr(),
                          column(4,checkboxInput("OverWStPts", "Overwrite Existing", TRUE)),
                          column(4,checkboxInput("SnapStPts", "Snap to conductor", TRUE)),
                          column(4,numericInput("SnapDStPts","Max Snaping Distance",min = 0, value = 5,step = 5)),
                          fileInput("Opensshapefile", "Switches and/or Other Open Points",
                                    multiple = TRUE,
                                    accept = c('.zip')),
                          #tags$hr(),
                          column(4,checkboxInput("OverWOpens", "Overwrite Existing", TRUE)),
                          column(4,checkboxInput("SnapOpens", "Snap to conductor", TRUE)),
                          column(4,numericInput("SnapDOpens","Max Snaping Distance",min = 0, value = 5,step = 5)),
                          fileInput("ProDevshapefile", "Monitoring Points (Protective Devices, breakers and Fuses)",
                                    multiple = TRUE,
                                    accept = c('.zip')),
                          #tags$hr(),
                          column(4,checkboxInput("OverWProDev", "Overwrite Existing", TRUE)),
                          column(4,checkboxInput("SnapProDev", "Snap to conductor", TRUE)),
                          column(4,numericInput("SnapDProDev","Max Snaping Distance",min = 0, value = 5,step = 5)),
                          fileInput("Transshapefile", "Transformers",
                                    multiple = TRUE,
                                    accept = c('.zip')),
                          column(4,checkboxInput("OverWTrans", "Overwrite Existing", TRUE)),
                          column(4,checkboxInput("SnapTrans", "Snap to conductor", TRUE)),
                          column(4,numericInput("SnapDTrans","Max Snaping Distance",min = 0, value = 5,step = 5)),
                          fileInput("Metersshapefile", "Production and Usage Meters",
                                    multiple = TRUE,
                                    accept = c('.zip')),
                          #tags$hr(),
                          column(4,checkboxInput("OverWmeters", "Overwrite Existing", TRUE)),
                          column(4,checkboxInput("Snapmeters", "Snap to conductor", TRUE)),
                          column(4,numericInput("SnapDmeters","Max Snaping Distance",min = 0, value = 5,step = 5)),
                          actionButton("B_UpdateNetwork", "Update network"),
                          actionButton("B_SaveState", "Save Network State")
             ),
             mainPanel(
               leafletOutput("mapAddData", height = 825)
             ))),
           tabPanel("Config",
                    tabsetPanel(
                      tabPanel("Feeder Starting Points",sidebarLayout(
                        sidebarPanel(
                        uiOutput("UI_StPtsID"),
                        uiOutput("UI_StPtsVolt"),
                        uiOutput("UI_StPtsOther"),
                        uiOutput("UI_StPtselRNBox"),
                        uiOutput("UI_StPtSelNames")
                        ),
                        mainPanel(column(width = 12,tableOutput("t_FS"),style = "overflow-x: scroll;"),
                                  tableOutput("t_FS_sel")))),
                      tabPanel("Primary Conductors",sidebarLayout(
                        sidebarPanel(
                        uiOutput("UI_ConsselVolt"),
                        uiOutput("UI_ConsselPhase"),
                        uiOutput("UI_ConsselFeeder"),
                        uiOutput("UI_ConsselOther"),
                        uiOutput("UI_ConsselRNBox"),
                        uiOutput("UI_ConSelNames")
                  ),
                  mainPanel(column(width = 12,tableOutput("t_cond"),style = "overflow-x: scroll;"),
                    tableOutput("t_cond_sel")))),
                  tabPanel("Network Open Points",sidebarLayout(
                    sidebarPanel(
                    uiOutput("UI_OpensID"),
                    uiOutput("UI_OpensPos"),
                    uiOutput("UI_OpensOther"),
                    uiOutput("UI_OpeselRNBox"),
                    uiOutput("UI_OpeSelNames")
                    ),
                    mainPanel(column(width = 12,tableOutput("t_Ope"),style = "overflow-x: scroll;"),
                              tableOutput("t_Ope_sel")))),
                  tabPanel("Protective Devices",sidebarLayout(
                    sidebarPanel(
                    uiOutput("UI_ProDevID"),
                    uiOutput("UI_ProDevOther"),
                    uiOutput("UI_PDselRNBox"),
                    uiOutput("UI_PDSelNames")
                    ),
                    mainPanel(column(width = 12,tableOutput("t_PD"),style = "overflow-x: scroll;"),
                              tableOutput("t_PD_sel")))),
                  tabPanel("Transformers",sidebarLayout(
                    sidebarPanel(
                    uiOutput("UI_TransID"),
                    uiOutput("UI_TransKVA"),
                    uiOutput("UI_TransPhase"),
                    uiOutput("UI_TransVolt"),
                    uiOutput("UI_TransSec"),
                    uiOutput("UI_TransFeeder"),
                    uiOutput("UI_TransOther"),
                    uiOutput("UI_TransselRNBox"),
                    uiOutput("UI_TransSelNames")
                    ),
                    mainPanel(column(width = 12,tableOutput("t_trans"),style = "overflow-x: scroll;"),
                              tableOutput("t_trans_sel")))),
                  tabPanel("Secondary Conductors",sidebarLayout(
                    sidebarPanel(
                    uiOutput("UI_ConSecVolt"),
                    uiOutput("UI_ConSecPhase"),
                    uiOutput("UI_ConSecFeeder"),
                    uiOutput("UI_ConSecOther"),
                    uiOutput("UI_ConsecselRNBox"),
                    uiOutput("UI_ConsecSelNames")
                    ),
                    mainPanel(column(width = 12,tableOutput("t_condsec"),style = "overflow-x: scroll;"),
                              tableOutput("t_condsec_sel")))),
                  tabPanel("Meters",sidebarLayout(
                    sidebarPanel(
                    uiOutput("UI_MetersselID"),
                    uiOutput("UI_MetersselST"),
                    uiOutput("UI_MetersselTM"),
                    uiOutput("UI_MetersselTC"),
                    uiOutput("UI_MetersselUC"),
                    uiOutput("UI_MetersselSC"),
                    uiOutput("UI_MetersselFe"),
                    uiOutput("UI_MetersselTr"),
                    uiOutput("UI_MetersselPD"),
                    uiOutput("UI_MetersselPU"),
                    uiOutput("UI_MetersselOth"),
                    uiOutput("UI_MetersselRNBox"),
                    uiOutput("UI_MetersSelNames")
                    ),
                    mainPanel(column(width = 12,tableOutput("t_meter"),style = "overflow-x: scroll;"),
                              tableOutput("t_meter_sel"))))
                    )),
           tabPanel("Finalization",
                    h4("Data Config file"),
                    downloadButton("downloadData", "Download Config File"),
                    fileInput("fileinputs", "Upload Config File"),
                    actionButton("change", "Apply Config"),
                    #actionButton("importData", "Import Data"),
                    h4("Database Controls"),
                    fileInput("meterdataload", "Load Meter Use Data"),
                    actionButton("clearmeterdata", "Clear Meter Database"),
                    h4("Data Processing"),
                    actionButton("proccessdata", "Proccess Network Data"))
         
         

)
