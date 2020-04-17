  
shinyServer(function(input, output) {
    
    #--------------------------------------------------------data
    
    #-----------------------read/view
    
    #selecting file
    output$file.type <- renderUI({
      req(input$data.source == "Import Dataset")
      selectInput("file.type", "File Type",
                  choices = c("csv", "tsv","Excel(xls/xlsx)", "SAS", "SPSS(sav)", "Stata(dta)"))
    })
    
    output$file.select <- renderUI({
      req(input$file.type)
      fileInput("table.data", "",
            multiple = FALSE#,
            #accept = c("text/csv",
            #           "text/comma-separated-values",
            #           ".csv",
            #           "text/tab-separated-values",
            #           ".tsv",
            #           "text/plain")
      )
    })
    #reactive dataframe (manipulation, analysis/plot prep)
    initial.data <- reactive({
      #req(input$data.source != "Choose Source")
      req(input$data.source != "Choose Source")
      if(input$data.source == "mtcars"){
        data("mtcars")
        mtcars
      }
      else if(input$data.source == "iris"){
        data("iris")
        iris
      }
      else if(input$data.source == "attitude"){
        data("attitude")
        attitude
      }
      else if(input$data.source == "airquality"){
        data("airquality")
        airquality
      }
      else if(input$data.source == "freeny"){
        data("freeny")
        freeny
      }
      else if(input$data.source == "USArrests"){
        data("USArrests")
        USArrests
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "csv"){
        read.csv(input$table.data$datapath,
                 header = input$header,
                 #sep = input$sep,
                 #quote = input$quote,
                 stringsAsFactors = F)
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "tsv"){
        read_tsv(input$table.data$datapath,
                 header = input$header)
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "Excel(xls/xlsx)"){
        as.data.frame(read_excel(input$table.data$datapath,
                  col_names = input$header))
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "SAS"){
        read_sas(input$table.data$datapath)
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "SPSS(sav)"){
        read_sav(input$table.data$datapath)
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "Stata(dta)"){
        read_dta(input$table.data$datapath)
      }
    })
    
    
    #----------------------manipulation
    
    output$tr.fun_select <- renderUI({
      req(input$data.source != "Choose Source")
        checkboxGroupInput("tr.fun",
                    "Transformation Functions",
                    choices = c("Adjust Variable Types", "Filter Rows by Value", 
                                #"New Variable",
                                "Group Data by Variable"),
                    inline = T
        )
    })
    
    #variable type adjustment
    output$tr.var_type <- renderUI({
        req(input$tr.fun == 'Adjust Variable Types')

        fluidRow(
            column(width = 6,
                selectInput("type.adj_var", "Variable",
                            choices = c(names(initial.data()))
                )
            ),

            column(width = 6,
                selectInput("type.adj_type", "Adjust to Type:",
                            choices = c("-", "factor", "numeric", "character")
                )
            )
        )
    })

    var_type_reac <- reactive({
        data.var_type <- initial.data()
      if(is.null(input$tr.fun)){
        data.var_type
      }
      else if(input$tr.fun != 'Adjust Variable Type'){
        data.var_type
      }
      else if(input$tr.fun == 'Adjust Variable Type' && input$type.adj_type == "-"){
        data.var_type
      }
      else if(input$tr.fun == 'Adjust Variable Type' && input$type.adj_type == "factor"){
        data.var_type[input$type.adj_var] <- as.factor(data.var_type[input$type.adj_var])
        data.var_type
      }
      else if(input$tr.fun == 'Adjust Variable Type' && input$type.adj_type != "numeric"){
        data.var_type[input$type.adj_var] <- as.numeric(data.var_type[input$type.adj_var])
        data.var_type
      }
      else if(input$tr.fun == 'Adjust Variable Type' && input$type.adj_type != "character"){
        data.var_type[input$type.adj_var] <- as.character(data.var_type[input$type.adj_var])
        data.var_type
      }
    })

    #filter
    output$tr.filter <- renderUI({
        req(input$tr.fun == 'Filter Rows by Value')
        fluidRow(
            column(width = 4,
                selectInput("filter.var", "Variable",
                            choices = c(names(var_type_reac()))
                )
            ),
            column(width = 4,
                selectInput("filter.operator", "Operator",
                            choices = c(">", ">=", "<", "<=") 
                )
            ),
            column(width = 4,
                numericInput("filter.value", "Value",
                            value = 0)
                )
        )
    })

    filter_reac <- reactive({
      data.filter <- var_type_reac()
        if(is.null(input$tr.fun)){
          data.filter
        }
        else if(input$tr.fun != "Filter Rows by Value"){
          data.filter
        }
        else if(input$filter.operator == ">"){
          data.filter <- data.filter %>% filter((!!as.name(input$filter.var))>input$filter.value)
        }
        else if(input$filter.operator == ">="){
          data.filter <- data.filter %>% filter((!!as.name(input$filter.var))>=input$filter.value)
        }
        else if(input$filter.operator == "<"){
          data.filter <- data.filter %>% filter((!!as.name(input$filter.var))<input$filter.value)
        }
        else if(input$filter.operator == "<="){
          data.filter <- data.filter %>% filter((!!as.name(input$filter.var))<=input$filter.value)
        }
        #else if(input$filter.operator == "is NA"){
        #  data.filter <- data.filter %>% filter(is.na(input$filter.var))
        #}
        #else if(input$filter.operator == "is not NA"){
        #  data.filter <- data.filter %>% filter(!is.na(input$filter.var))
        #}
        data.filter
    })

    # #mutate
    # output$tr.mutate <- renderUI({
    #     req(input$tr.fun == 'Create New Variable')
    # 
    #     fluidRow(
    #       column(width = 4,
    #              textInput("new_mutate.var", "New Variable Name",
    #                           value = "new_variable"
    #              )
    #       ),
    #       column(width = 4,
    #              radioButtons("mutate.1", "Term 1 Type",
    #                          choices = c("Variable", "Numeric Value")
    #              )
    #       ),
    #       column(width = 4,
    #              radioButtons("mutate.2", "Term 2 Type",
    #                           choices = c("Variable", "Numeric Value")
    #              )
    #       )
    #     )
    #     fluidRow(
    #       column(width = 4,
    #              if(input$mutate.1 == "Variable"){
    #                selectInput("mutate_var1", "Variable 1",
    #                            choices = c(names(filter_reac())))
    #              }
    #              else if(input$mutate.1 == "Numeric"){
    #                numericInput("mutate_var1", "Variable 1",
    #                             value = 1)
    #              }
    #       ),
    #       column(width = 4,
    #              selectInput("mutate_oper", "Operation",
    #                          choices = c("/", "*", "+", "-"))
    #       ),
    #       column(width = 4,
    #              if(input$mutate.2 == "Variable"){
    #                selectInput("mutate_var2", "Variable 2",
    #                            choices = c(names(filter_reac())))
    #              }
    #              else if(input$mutate.2 == "Numeric"){
    #                numericInput("mutate_var2", "Variable 2",
    #                             value = 1)
    #              }
    #       )
    #     )
    # })
    # 
    # mutate_reac <- reactive({
    #   data.mutate <- filter_reac()
    #   if(input$tr.fun != "Create New Variable"){
    #     data.mutate
    #   }
    #   else if(input$mutate_oper == "/"){
    #     data.mutate <- data.mutate %>% mutate(as.name(input$new_mutate.var) = input$mutate_var1/input$mutate_var2)
    #   }
    #   else if(input$mutate_oper == "*"){
    #     data.mutate <- data.mutate %>% mutate(as.name(input$new_mutate.var) = input$mutate_var1*input$mutate_var2)
    #   }
    #   else if(input$mutate_oper == "+"){
    #     data.mutate <- data.mutate %>% mutate(as.name(input$new_mutate.var) = input$mutate_var1+input$mutate_var2)
    #   }
    #   else if(input$mutate_oper == "-"){
    #     data.mutate <- data.mutate %>% mutate(as.name(input$new_mutate.var) = input$mutate_var1-input$mutate_var2)
    #   }
    # })
     
    #group_by
    output$tr.group <- renderUI({
        req(input$tr.fun == 'Group Data by Variable')

        fluidRow(
            column(width = 6,
                selectInput("group_by.var", "Variable for Grouping",
                            choices = c(
                              #names(mutate_reac())
                              names(filter_reac())
                              ))
            )
        )
    })

    group_by_reac <- reactive({
      data.group <- filter_reac()
        if(is.null(input$tr.fun)){
          data.group
        }
        else if(input$tr.fun != "Group Data by Variable"){
          data.group
        }
        else{
          data.group <- data.group %>% group_by(!!as.name(input$group_by.var))
        }
      data.group
    })

    #modified data object
    plot.data <- reactive({
      #group_by_reac()
      initial.data()
      })
    
    #data characteristics output
    output$str.info <- renderPrint(
      print(str(plot.data()))
    )
    
    #table view of imported data
    output$data.view <- renderTable({ 
      #req(input$data.source != "Choose Source")
      if(input$disp == "head") {
        return(head(plot.data()))
      }
      
      else {
        return(plot.data())
      }
    })
    
    output$reac_table <- renderReactable({
      if(input$disp == "head") {
        reactable(head(plot.data()), filterable = T, pagination = F, showSortable = T, showSortIcon = T, striped = T, highlight = T)
      }
      
      else {
        reactable(plot.data(),  filterable = T, pagination = F, showSortable = T, showSortIcon = T, striped = T, highlight = T)
      }
    })
    
    #----------------------------------------------------plotting  
    
    #var select
    output$single_var_check <- renderUI({
        req(input$data.source != "Choose Source")
        checkboxInput("single.var.check",
                      "Single Variable",
                      value = F)
    })
    
    output$y_select <- renderUI({
        req(input$data.source != "Choose Source")
        if(input$single.var.check == F){
            selectInput("yvar",
                        label = "Y Variable",
                        choices = c(names(plot.data()))
            )
        }
        else{
            selectInput("single.var",
                        label = "Variable",
                        choices = c(names(plot.data()))
            )
        }
    })
    
    output$x_select <- renderUI({
        req(input$single.var.check == F)
        selectInput("xvar",
                    label = "X Variable",
                    choices = c(names(plot.data()))
        )
    })
    
    #reactive x and y vars
    y_var <- reactive({
        if(input$single.var.check == F){
            input$yvar
        }
        else{
            input$single.var
        }
    })
    
    x_var <- reactive({
        input$xvar
    })
    
    #geom select
    
    geom_list <- reactive({
      if(input$single.var.check == T){
        c("Choose Geometry", "Density", "Dotplot", "Histogram", "Bar", "Boxplot")
      }else{
        c("Choose Geometry", "Point", "Rug", "Smooth", "Violin", "Area", "Line", "Step")
      }
    })
    
    output$geom_select <- renderUI({
        req(input$data.source != "Choose Source")
        selectInput("geom",
                    label = "Geometry Type",
                    choices = geom_list(),
                    multiple = F
        )
    })
    
    
    #theme
    output$theme_select <- renderUI({
        req(input$data.source != "Choose Source")
        selectInput("theme",
                    label = "Theme",
                    choices = c("default", "theme_bw", "theme_classic", "theme_grey", "theme_minimal", "theme_light", "theme_dark")
        )
    })
    
    theme_reac <- reactive({
      if(input$theme == "default"){
        NULL
      }
      else if(input$theme == "theme_bw"){
        theme_bw()
      }
      else if(input$theme == "theme_classic"){
        theme_classic()
      }
      else if(input$theme == "theme_grey"){
        theme_grey()
      }
      else if(input$theme == "theme_minimal"){
        theme_minimal()
      }
      else if(input$theme == "theme_light"){
        theme_light()
      }
      else if(input$theme == "theme_dark"){
        theme_dark()
      }
    })
    
    #fonts
    output$font_select <- renderUI({
      req(input$lab.check == "Change Font")
      fluidPage(    
        fluidRow(
          column(width = 6,
                 selectInput("font.type",
                             "Font Type",
                             choices = c("Default", "AvanteGarde", "Bookman", "Courier", "Helvetica", "Palatino", "Times"),
                             selected = "Default")
          ),
          column(width = 6,
                 numericInput("font.size",
                              "Font Size",
                              value = 12,
                              min = 2, max = 42,
                              step = 1)
          )
        ),
        if(input$theme == "default"){
          fluidRow(
            column(width = 6,
                   selectInput("font.face",
                               "Font Face",
                               choices = c("plain", "bold", "italic", "bold.italic"))
            )
          )
        }
      )
    })
    
    font_type_reac <- reactive({
      if(is.null(input$lab.check)){
        NULL
      }
      else if(input$lab.check != "Change Font"){
        NULL
      }
      else if(input$font.type == "Default"){
        NULL
      }
      else{
        input$font.type
      }
    })
    
    font_size_reac <- reactive({
      if(is.null(input$lab.check)){
        NULL
      }
      else if(input$lab.check != "Change Font"){
        NULL
      }
      else{
        input$font.size
      }
    })
    
    font_face_reac <- reactive({
      if(is.null(input$lab.check)){
        NULL
      }
      else if(input$lab.check != "Change Font"){
        NULL
      }
      else if(input$font.face == "plain"){
        NULL
      }
      else{
        input$font.face
      }
    })
    
    theme_font_args <- reactive({
      if(is.null(input$lab.check)){
        NULL
      }
      else if(input$lab.check != "Change Font"){
        NULL
      }
      else if(input$lab.check == "Change Font"){
        font_args <- list(input$font.type, input$font.size, input$font.face) 
      }
    })
    
    font_reac <- reactive({
      if(input$theme == "default" && is.null(input$lab.check)){
        NULL
      }
      else if(input$theme == "default" && input$lab.check != "Change Font")
        NULL
     else if(input$theme == "default" && input$lab.check == "Change Font"){
        theme_fun <- function(family = NULL, size = NULL, face = NULL){
          theme(axis.title = element_text(family = family, size = size, face = face))
        }
        do.call(theme_fun, theme_font_args())
      }
      else if(input$theme != "default" && input$lab.check == "Change Font"){
        theme_fun <- function(family = NULL, size = NULL){
          do.call(theme_type(), base_family = family, base_size = size)
        }
      }
    })
    
    #faceting
    output$facet_check <- renderUI({
        req(input$data.source != "Choose Source")
        checkboxInput("facet.check",
                      "Use Faceting",
                      value = F
        )
    })
    
    output$facet_select <- renderUI({
        req(input$facet.check == T)
        selectInput("facet",
                    label = "Faceting Type",
                    choices = c("None", "Wrap", "Columns", "Rows", "Grid")
        )
    })
    
    output$facet_var1 <- renderUI({
        req(input$facet.check == T)
        req(input$facet == "Rows" || input$facet == "Grid")
        selectInput("facet.var1",
                    label = "Row Facet Variable",
                    choices = c("None", names(plot.data()))
        )
    })
    
    output$facet_var2 <- renderUI({
        req(input$facet.check == T)
        req(input$facet == "Columns" || input$facet == "Wrap" || input$facet == "Grid")
        selectInput("facet.var2",
                    label =  
                        if(input$facet == "Columns" || input$facet == "Grid"){
                            "Column Facet Variable"
                        }
                        else if(input$facet == "Wrap"){
                            "Wrapping Facet Variable"
                        }
                    ,
                    choices = c("None", names(plot.data()))
        )
    })
    
    #reactive faceting
    facet_reac <- reactive({
        if(input$facet.check == F){
          NULL
        }
        else if(input$facet == "Wrap"){
            req(input$facet.var2 != "None")
            facet_wrap(as.formula(paste("~", input$facet.var2)))
        }
        else if(input$facet == "Columns"){
            req(input$facet.var2 != "None")
            facet_grid(as.formula(paste("~", input$facet.var2)))
        }
        else if(input$facet == "Rows"){
            req(input$facet.var1 != "None")
            facet_grid(as.formula(paste(input$facet.var1, "~", ".")))
        }
        else if(input$facet == "Grid"){
            req(input$facet.var1 != "None" && input$facet.var2 != "None")
            facet_grid(as.formula(paste(input$facet.var1, "~", input$facet.var2)))
        }
    })
    
    ##coordinate system select
    #output$coord_check <- renderUI({
    #    req(input$data.source != "Choose Source")
    #    checkboxInput("coord.adj",
    #        "Adjust Coordinate System",
    #        value = F
    #    )
    #})
    #
    #output$coord_select <- renderUI({
    #    req(input$coord.adj == T)
    #    selectInput("coord",
    #                label = "Coordinate System",
    #                choices = c("Cartesian", "Fixed", "Flipped", "Polar")
    #    )
    #})
    #
    ##reactive coords
    #coord_reac <- reactive({
    #  if(input$coord_adj == F){
    #    NULL
    #  }
    #  else if(input$coord == "Cartesian"){
    #    coord_cartesian()
    #  }
    #  else if(input$coord == "Fixed"){
    #    coord_fixed()
    #  }
    #  else if(input$coord == "Flipped"){
    #    coord_flip()
    #  }
    #  else if(input$coord == "Polar"){
    #    coord_polar()
    #  }
    #})
    
    
    #submit button
    output$update_plot <- renderUI({
        req(input$data.source != "Choose Source")
        actionButton("plot_update", "Plot/Update", icon("pencil-ruler", lib = "font-awesome"), width = 230)
        #icons: "laptop-code", 
    })
    
    
    
    #--------------------------labels
    #title
    output$lab_check <- renderUI({
        req(input$data.source != "Choose Source")
        checkboxGroupInput("lab.check", "",
                      choices = c("Add Title", "Change Axis Labels"
                                  #, "Change Font"
                                  ),
                      inline = T
        )
    })
    
    output$plot_title <- renderUI({
        req(input$lab.check == "Add Title")
        textInput("title",
            "Plot Title",
            "Title"
        )
    })
    
    
    #xlab
    output$x_label <- renderUI({
      req(input$lab.check == "Change Axis Labels")
        if(input$single.var.check == F){
          textInput("x_lab",
                    "X-axis Label",
                    value = input$xvar
          )
        }
    })
    
    #ylab
    output$y_label <- renderUI({
      req(input$lab.check == "Change Axis Labels")
        if(input$single.var.check == F){
            textInput("y_lab",
                      "Y-axis Label",
                      value = input$yvar
            )
        }
        else if(input$single.var.check == T){
            textInput("var_lab",
                      "Variable Label",
                      value = input$single.var
            )
        }
    })
    
    #reactive labs
    labs_reac <- reactive({
      if(input$single.var.check == T){
        lab_fun <- function(title = NULL, y = NULL){
          labs(title = title, y = y)
        }
        lab_args <- list(title = input$title, y = input$var_lab)
      }
      else if(input$single.var.check == F){
        lab_fun <- function(title = NULL, y = NULL, x = NULL){
          labs(title = title, y = y, x = x)
        }
        lab_args <- list(input$title, input$y_lab, input$x_lab)
      }
      do.call(lab_fun, lab_args)
    })
    
    #-----------------------aes and geom parameters
    
    #aes check choice string
    aes_choice <- reactive({
      ac <- c("Colors", "Alpha")
      if(input$geom != "Rug" && input$geom != "Line" && input$geom != "Step"){
        ac <- c(ac, "Fill")
      }else{ac}
      if(input$geom != "Dotplot"){
        ac <- c(ac, "Size")
      }else{ac}
      if(input$geom != "Dotplot" && input$geom != "Point"){
        ac <- c(ac, "Linetype")
      }else{ac}
      if(input$geom == "Bar" || input$geom == "Point"){
        ac <- c(ac, "Position")
      }else{ac}
    })
    
    #color
    output$aes_check <- renderUI({
        req(input$data.source != "Choose Source")
        checkboxGroupInput("aes.check", "",
                      #choices = c("Colors", "Fill", "Alpha", "Size", "Linetype"),
                      choices = aes_choice(),
                      inline = T
        )
    })
    
    #color
    #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    output$color_select <- renderUI({
        req(input$aes.check == "Colors")
        fluidRow(    
            column(width = 6,
                selectInput("color",
                            "Select Color",
                            choices = c("None","Black", "Orange", "Sky Blue", "Green", "Yellow", "Blue", "Red", "Pink"))
            ),
            column(width = 6,
                selectInput("color.group",
                            "Variable to Group or Scale by Color",
                            choices = c("None", names(plot.data())))
            )
        )
    })
    
    color_group_reac <- reactive({
        if(is.null(input$aes.check)){
           NULL
        }
        else if(input$aes.check != "Colors"){
            NULL
        }
        else if(input$color.group == "None"){
            NULL
        }
        else{
            input$color.group
        }
    })
    
    color_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Colors"){
        NULL
      }
      else if(input$color == "None"){
        NULL
      }
      else if(input$color == "Black"){
        "#000000"
      }
      else if(input$color == "Orange"){
        "#E69F00"
      }
      else if(input$color == "Sky Blue"){
        "#56B4E9"
      }
      else if(input$color == "Green"){
        "#009E73"
      }
      else if(input$color == "Yellow"){
        "#F0E442"
      }
      else if(input$color == "Blue"){
        "#0072B2"
      }
      else if(input$color == "Red"){
        "#D55E00"
      }
      else if(input$color == "Pink"){
        "#CC79A7"
      }
    })
    
    
    #fill
    output$fill_select <- renderUI({
        req(input$aes.check == "Fill" && input$geom != "Rug", input$geom != "Line", input$geom != "Step")
        selectInput("fill.select",
                    "Select Fill Color",
                    choices = c("None","Black", "Orange", "Sky Blue", "Green", "Yellow", "Blue", "Red", "Pink")
        )
    })
    
    fill_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Fill"){
        NULL
      }
      else if(input$fill.select == "None"){
        NULL
      }
      else if(input$fill.select == "Black"){
        "#000000"
      }
      else if(input$fill.select == "Orange"){
        "E69F00"
      }
      else if(input$fill.select == "Sky Blue"){
        "56B4E9"
      }
      else if(input$fill.select == "Green"){
        "009E73"
      }
      else if(input$fill.select == "Yellow"){
        "F0E442"
      }
      else if(input$fill.select == "Blue"){
        "0072B2"
      }
      else if(input$fill.select == "Red"){
        "D55E00"
      }
      else if(input$fill.select == "Pink"){
        "#CC79A7"
      }
    })
    
    #alpha
    output$alpha_type <- renderUI({
        req(input$aes.check == "Alpha")
        radioButtons("alpha.type", label = NULL,
                     choices = c("Consistent Alpha", "Scaled Alpha"),
                     selected = "Consistent Alpha")
    })
    
    output$alpha_adjust <- renderUI({
        req(input$aes.check == "Alpha")
        if(input$alpha.type == "Consistent Alpha"){
            sliderInput("alpha.adj",
                        "Adjust Alpha",
                        min = 0.01, max = 1,
                        value = 0.7,
                        round = -2)  
        }
        else if(input$alpha.type == "Scaled Alpha"){
          radioButtons("alpha.scale.type", label = NULL,
                       choices = c("Alpha by Variable", "Select Range"),
                       selected = "Select Range")
        }
    })
    
    output$alpha_scale <- renderUI({      
        req(input$alpha.type == "Scaled Alpha")    
        if(input$alpha.scale.type == "Alpha by Variable"){
          fluidRow(
            column(width = 6,
                   selectInput("alpha.scale.var",
                               "Variable for Scaling Alpha",
                               choices = c("None", names(plot.data())))
            )
          )
        }
        else if(input$alpha.scale.type == "Select Range"){
          fluidRow(
            column(width = 6,
                   sliderInput("alpha.scale.range",
                               "Alpha Scale Range",
                               min = 0.01, max = 1,
                               value = c(0.3, 0.7),
                               round = -2,
                               step = 0.05)
            )
          )
        }
    })
    
    alpha_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Alpha"){
        NULL
      }
      else if(input$alpha.type == "Consistent Alpha"){
        input$alpha.adj
      }
      else if(input$alpha.scale.type == "Alpha by Variable"){
        input$alpha.scale.var
      }
      else if(input$alpha.scale.type == "Select Range"){
        input$alpha.scale.range
      }
    })
    
    #size
    output$size_type <- renderUI({
        req(input$aes.check == "Size")
        radioButtons("size.type", label = NULL,
                     choices = c("Consistent Size", "Scale Size"),
                     selected = "Consistent Size")
    })
    
    output$size_adjust <- renderUI({
        req(input$aes.check == "Size")
        if(input$size.type == "Consistent Size"){
            sliderInput("size.adj",
                         "Adjust Size",
                         min = 0.1, max = 20,
                         value = 1,
                         step = 0.5) 
        }
        else if(input$size.type == "Scale Size"){
            selectInput("size.scale",
                        "Variable to Scale Size",
                        choices = c("None", names(plot.data())))
        }
    })
    
    size_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Size"){
        NULL
      }
      else if(input$size.type == "Consistent Size"){
        input$size.adj
      }
      else if(input$size.type == "Scale Size"){
        input$size.scale
      }
    })
    
    
    #linetype
    output$linetype_select <- renderUI({
        req(input$aes.check == "Linetype")
        selectInput("linetype",
                    "Select Linetype",
                    choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))
    })
    
    linetype_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Linetype"){
        NULL
      } 
      else if(input$linetype == "solid"){
        NULL
      }
      else if(input$linetype == "dashed"){
        "dashed"
      }
      else if(input$linetype == "dotted"){
        "dotted"
      }
      else if(input$linetype == "dotdash"){
        "dotdash"
      }
      else if(input$linetype == "longdash"){
        "longdash"
      }
      else if(input$linetype == "twodash"){
        "twodash"
      }
    })
    
    #shape
    output$shape_select <- renderUI({
        req(input$aes.check == "Shape")
        selectInput("shape.select",
                    "Select Shape",
                    choices = c("default", "circle", "square", "diamond", "triangle", "plus", "cross", "asterisk"))
    })
    
    shape_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      } 
      else if(input$aes.check != "Shape"){
        NULL
      } 
      else if(input$shape.select == "default"){
        NULL
      }
      else if(input$shape.select == "circle"){
        1
      }
      else if(input$shape.select == "square"){
        15
      }
      else if(input$shape.select == "diamond"){
        18
      }
      else if(input$shape.select == "triangle"){
        17
      }
      else if(input$shape.select == "plus"){
        3
      }
      else if(input$shape.select == "cross"){
        4
      }
      else if(input$shape.select == "asterisk"){
        8
      }
    })
    
    
    #position adjustment
    position_string <- reactive({
      if(input$geom == "Bar"){
        c("None", "Dodge", "Fill", "Stack")
      }
      else if(input$geom == "Point"){
        c("None", "Jitter")
      }
    }) 
    
    output$position_adj <- renderUI({
      req(input$aes.check == "Position")
      selectInput("pos_adj",
                  label = "Position Adjustment",
                  choices = position_string()
      )
    })
    
    position_reac <- reactive({
      if(is.null(input$pos_adj)){
        NULL
      }
      else if(input$pos_adj == "None"){
        NULL
      }
      else if(input$pos_adj == "Jitter"){
        "jitter"
      }
      else if(input$pos_adj == "Dodge"){
        "dodge"
      }
      else if(input$pos_adj == "Fill"){
        "fill"
      }
      else if(input$pos_adj == "Stack"){
        "stack"
      }
    })
    
    #-----------------------plot code division
    
    #primary aes
    p_aes_add_args <- reactive({
        c(color = color_group_reac())
    })
    
    p_aes_simple <- reactive({
        if(input$single.var.check == T){
            p_aes_fun <- function(y){
                aes_string(y)
            }
            p_aes_args <- list(y_var())
        }
        else{
            p_aes_fun <- function(x, y){
                aes_string(x, y)
            }
            p_aes_args <- list(x_var(), y_var())
        }
        do.call(p_aes_fun, p_aes_args)
    })
    
    p_aes_full <- reactive({
        if(input$single.var.check == T){
            p_aes_fun <- function(y, color = NULL){
                aes_string(y, color = color)
            }
            p_aes_args <- list(y_var(), p_aes_add_args())
        }
        else{
            p_aes_fun <- function(x, y, color = NULL){
                aes_string(x, y, color = color)
            }
            p_aes_args <- list(x_var(), y_var(), p_aes_add_args())
        }
        do.call(p_aes_fun, p_aes_args)
    })
    
    
    #geom args
    geom_args <- reactive({
      if(!is.null(color_reac())){
        al <- c(color = color_reac())
      }else{al <- NULL}
      if(!is.null(alpha_reac())){
        al <- c(al, alpha = alpha_reac())
      }else{al <- al}
      if(!is.null(fill_reac())){
        al <- c(al, fill = fill_reac())
      }else{al <- al}
      if(!is.null(size_reac())){
        al <- c(al, size = size_reac())
      }else{al <- al}
      if(!is.null(linetype_reac())){
        al <- c(al, linetype = linetype_reac())
      }else{al <- al}
      if(!is.null(position_reac())){
        al <- c(al, position = position_reac())
      }else{al <- al}
    })
    
    geom_fun_str <- reactive({
      #c(color = color, alpha = alpha, fill = fill, size = size, linetype = linetype, position = position)
      c(color = color_reac(), alpha = alpha_reac(), fill = fill_reac(), size = size_reac(), linetype = linetype_reac(), position = position_reac())
    })
    
    geom_reac <- reactive({
      geom_fun <- function(color = NULL, alpha = NULL, fill = NULL, size = NULL, linetype = NULL, position = NULL){
        #list(geom_args()) %>% geom_type() 
        
          if(input$geom == "Choose Geometry"){
            NULL
          }
          else if(input$geom == "Density"){
            geom_density(mapping = (geom_fun_str()))
          }
          else if(input$geom == "Dotplot"){
            geom_dotplot(geom_fun_str())
          }
          else if(input$geom == "Histogram"){
            geom_histogram(geom_fun_str())
          }
          else if(input$geom == "Bar"){
            geom_bar(geom_fun_str())
          }
          else if(input$geom == "Point"){
            geom_point(geom_fun_str())
          }
          else if(input$geom == "Rug"){
            geom_rug(geom_fun_str())
          }
          else if(input$geom == "Smooth"){
            geom_smooth(geom_fun_str())
          }
          else if(input$geom == "Boxplot"){
            geom_boxplot(geom_fun_str())
          }
          else if(input$geom == "Violin"){
            geom_violin(geom_fun_str())
          }
          else if(input$geom == "Area"){
            geom_area(geom_fun_str())
          }
          else if(input$geom == "Line"){
            geom_line(geom_fun_str())
          }
          else if(input$geom == "Step"){
            geom_step(geom_fun_str())
          }
      }
      geom_fun(geom_args())
      #do.call(geom_fun, list(geom_args()))
      #if(!is.null(geom_args())){
      #  list(geom_args()) %>% geom_type()
      #}else{
      #  geom_type()
      #}
    })
    
    
    #plot component layering
    p <- reactive({
      p <- plot.data() %>% 
            ggplot(p_aes_full())# + theme_reac() # + geom_reac()
      #p
      if(!is.null(geom_reac())){
        pg <- p + geom_reac()
      }else{pg <- p}
      if(!is.null(theme_reac())){
        pt <- pg + theme_reac()
      }else{pt <- pg}
      #if(!is.null(font_reac())){
      #  pf <- pt + font_reac()
      #}else{pf <- pt}
      if(!is.null(facet_reac())){
        pa <- pt + facet_reac()
      }else{pa <- pt}
      #if(!is.null(coord_reac())){
      #  pc <- pa + coord_reac()
      #}else{pc <- pa}
      if(!is.null(labs_reac())){
        pl <- pa + labs_reac()
      }else{pl <- pa}
      pl
    })

    #plot layering
    #if(!is.null(geom_reac()) && length(geom_reac() > 0)){
    #  p() <- reactive({p() + geom_point()})
    #}
#    geom, theme, font, facet, coord, labs, 
    
    
    #----------------------plot code combination
    output$plot <- renderPlot({
        p() #+ geom_reac()#+ geom_point() + theme_reac()#labs_reac() #theme_reac() + facet_reac() + #lims_reac
    })
    
   
    
    #----------------------------code display
    #check parse/str2expression/str2lang
    #quote() operator
    
    #shinymeta usage
        #!! - precede reactive value expressions to replace with their values
        
        #reactive replace    
            #metaReactive()
            #metaReactive2()
        #observe replace
            #metaObserve()
            #metaObserve2()
        #render replace
            #metaRender()
            #metaRender2()
        #expandChain()
            #upward-growing variable chain
        #outputCodeButton() & displayCodeModal()
        #buildRmdBundle()
    
    #---------------------------------------------------tables
   
})