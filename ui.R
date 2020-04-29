library(shiny)
library(shinymeta)
library(shinythemes)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(datasets)
library(haven)
library(reactable)

options(shiny.usecairo = F)

shinyUI(navbarPage(title = "tidyTouch",
                 
                 theme = shinytheme("sandstone"),
                 
                 #--------------------------------------------Info/Demos
                 
                 # tabPanel(
                 #   "About"
                 # ),
                 
                 #--------------------------------------------Data reading and transformation
                 tabPanel(
                     "Data",
                     sidebarLayout(
                         sidebarPanel(
                           
                           #tags$style(type="text/css",
                           #           ".shiny-output-error { visibility: hidden; }",
                           #           ".shiny-output-error:before { visibility: hidden; }"
                           #),
                           
                             selectInput("data.source", "Data",
                                         choices = c("Choose Source", "Import Dataset", "mtcars", "iris", "attitude", "airquality", "freeny", "USArrests")
                                         ),
                             uiOutput("file.type"),
                             uiOutput("file.select"),
                             #fileInput("table.data", "Choose CSV File",
                             #          multiple = FALSE,
                             #          accept = c("text/csv",
                             #                     "text/comma-separated-values,text/plain",
                             #                     ".csv")),
                             
                             checkboxInput("header", "Header", TRUE),
                             
                             #radioButtons("sep", "Separator",
                             #             choices = c(Comma = ",",
                             #                         Semicolon = ";",
                             #                         Tab = "\t"),
                             #             selected = ","),
                             #
                             #radioButtons("quote", "Quote",
                             #             choices = c(None = "",
                             #                         "Double Quote" = '"',
                             #                         "Single Quote" = "'"),
                             #             selected = '"'),
                             
                             tags$hr(),
                             
                             radioButtons("disp", "Display",
                                          choices = c(Head = "head",
                                                      All = "all"),
                                          selected = "head"),
                             
                             verbatimTextOutput("str.info")
                             
                         ), #close sidebarPanel
                         
                         mainPanel(
                           
                           #tags$style(type="text/css",
                           #           ".shiny-output-error { visibility: hidden; }",
                           #           ".shiny-output-error:before { visibility: hidden; }"
                           #),
                           
                             div(style = 'overflow-y: scroll;
                                           height: 542px',
                                #tableOutput("data.view")
                                reactableOutput("reac_table")
                             ),
                             
                             tags$hr(),
                             uiOutput("tr.fun_select"),
                             tags$hr(),
                             uiOutput("tr.var_type"),
                             uiOutput("tr.filter"),
                             uiOutput("tr.group")
                             
                             #data manipulation
                             #dplyr
                             
                             #condensing table
                             #select
                             #contains (character string)
                             #matches (matched regular expression)
                             
                             #filter
                             #give rows that pass a 'test'
                             
                             #mutate
                              #create variables with given data
                               #returns data frame that has new variables added
                               #should assign to object (original_data.frame)
                              #use for centering, but separate function
                             #tidyr
                              #spread and gather 'retired'
                              #pivot_wider() & pivot_longer() 
                             
                         ) #close mainPanel
                     ) #close sidebarLayout
                 ), #close tabPanel for 'Data'
                 
                 #--------------------------------------------plot creation / feature adjustment  
                 tabPanel(
                     "Plot",
                     sidebarLayout(
                       
                       sidebarPanel(
                          uiOutput("single_var_check"),
                          uiOutput("geom_select"),
                          uiOutput("y_select"),
                          uiOutput("x_select"),
                          uiOutput("position_adj"),
                          uiOutput("theme_select"),
                          uiOutput("facet_check"),
                          uiOutput("facet_select"),
                          uiOutput("facet_var1"),
                          uiOutput("facet_var2")
                          #uiOutput("coord_check"),
                          #uiOutput("coord_select"),
                          #tags$hr(),
                          #uiOutput("update_plot")
                      ),#close sidebarPanel
                       
                      mainPanel(
                         plotOutput("plot"),
                         tabsetPanel(
                           tabPanel("Labels",
                              uiOutput("lab_check"),
                              #title and axis labels
                            fluidRow(
                              column(uiOutput("plot_title"), width = 6)
                            ),
                            fluidRow(
                              column(uiOutput("y_label"), width = 6),
                              column(uiOutput("x_label"), width = 6)
                            ),
                              #font
                            #uiOutput("font_select")
                           ),
                           
                           tabPanel("Aesthetic Specifications",
                              uiOutput("aes_check"),
                              tags$hr(),
                              #color
                              uiOutput("color_select"),
                              #alpha
                              uiOutput("alpha_type"),
                              uiOutput("alpha_adjust"),
                              uiOutput("alpha_scale"),
                              #size
                              uiOutput("size_type"),
                              uiOutput("size_adjust"),
                              #fill
                              uiOutput("fill_select"),
                              #linetype
                              uiOutput("linetype_select"),
                              #shape
                              uiOutput("shape_select"),
                           )
                           
                            #axis ranges
                            
                            #shape

                            #aesthetic specifications
                            #color and fill
                            #lines
                            #size, linetype
                            #polygons
                            #color, linetype, size
                           
                        )#close tabset Panel
                      )#close mainPanel
                    )#close sidebarLayout     
                 ),#,#close tabPanel for 'Plot'
                 
                 #------------------------------------------------- code section
                 tabPanel(
                 "Code",
                 uiOutput("code")
                 )#close tabPanel for 'Code'
                 
    ) #close navbarPage
)#close shinyUI
