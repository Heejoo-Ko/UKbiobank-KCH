library(shiny);library(shinycustomloader);library(ggpubr);library(survival);library(jsmodule);library(DT)
#source("global.R")
library(data.table);library(magrittr)
zz <- readRDS("data.RDS")
out <- zz$data[, .SD, .SDcols = -names(zz$data)[sapply(zz$data, function(x){"Date" %in% class(x)})]]
out.label <- jstable::mk.lev(out)


nfactor.limit <- 20

ui <- navbarPage("UK biobank",
                 tabPanel("Data", icon = icon("table"),
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("check_subset", "Subset data", F),
                              uiOutput("subset_var"),
                              uiOutput("subset_val")
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                          tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                              )
                            )
                          )
                 ),
                 tabPanel("Table 1", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )
                          
                 ),
                 navbarMenu("Regression", icon = icon("list-alt"),
                            tabPanel("Linear regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("linear")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("lineartable"), type="html", loader="loader6"),
                                         br(),
                                         uiOutput("warning_linear")
                                       )
                                     )
                            ),
                            tabPanel("Logistic regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("logistic")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Cox model",
                                     sidebarLayout(
                                       sidebarPanel(
                                         coxUI("cox")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("coxtable"), type="html", loader="loader6")
                                       )
                                     )
                            )
                            
                 ),
                 navbarMenu("Plot", icon = icon("bar-chart-o"),
                            tabPanel("Basic plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         ggpairsModuleUI1("ggpairs")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("ggpairs_plot"), type="html", loader="loader6"),
                                         ggpairsModuleUI2("ggpairs")
                                       )
                                     )
                            ),
                            tabPanel("Scatterplot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         scatterUI("scatter")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("scatter_plot"), type="html", loader="loader6"),
                                         ggplotdownUI("scatter")
                                       )
                                     )
                            ),
                            tabPanel("Kaplan-meier plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         kaplanUI("kaplan")
                                       ),
                                       mainPanel(
                                         optionUI("kaplan"),
                                         withLoader(plotOutput("kaplan_plot"), type="html", loader="loader6"),
                                         ggplotdownUI("kaplan")
                                       )
                                     )
                            )
                            
                 ),
                 navbarMenu("ROC analysis", icon = icon("check"),
                            tabPanel("ROC",
                                     sidebarLayout(
                                       sidebarPanel(
                                         rocUI("roc")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("plot_roc"), type="html", loader="loader6"),
                                         ggplotdownUI("roc"),
                                         withLoader(DTOutput("table_roc"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Time-dependent ROC",
                                     sidebarLayout(
                                       sidebarPanel(
                                         timerocUI("timeroc")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("plot_timeroc"), type="html", loader="loader6"),
                                         ggplotdownUI("timeroc"),
                                         withLoader(DTOutput("table_timeroc"), type="html", loader="loader6")
                                       )
                                     )
                            )
                 )
)

server <- function(input, output, session) {
  

  

  
  observeEvent(input$check_subset, {
    output$subset_var <- renderUI({
      req(input$check_subset == T)
      #factor_subset <- c(data.list$factor_original, input$factor_vname)
      
      #validate(
      #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
      #)
      
      tagList(
        selectInput("var_subset", "Subset variables",
                    choices = names(out), multiple = T,
                    selected = "Age_60")
      )
    })
    
    output$subset_val <- renderUI({
      req(input$check_subset == T)
      req(length(input$var_subset) > 0)
      vars.factor <- factor_vars
      
      outUI <- tagList()
      
      for (v in seq_along(input$var_subset)){
        if (input$var_subset[[v]] %in% vars.factor){
          varlevel <- levels(as.factor(out[[input$var_subset[[v]]]]))
          outUI[[v]] <- selectInput(paste0("val_subset", v), paste0("Subset value: ", input$var_subset[[v]]),
                                    choices = varlevel, multiple = T,
                                    selected = varlevel[length(varlevel) - 1])
        } else{
          val <- stats::quantile(out[[input$var_subset[[v]]]], na.rm = T)
          outUI[[v]] <- sliderInput(paste0("val_subset", v), paste0("Subset range: ", input$var_subset[[v]]),
                                    min = val[1], max = val[5],
                                    value = c(val[2], val[4]))
        }
        
      }
      outUI
    })
  })
  
  
  data.info <- reactive({
    out <- copy(out)
    out.label <- copy(out.label)
    #out.label[, var_label := ref[out.label$variable, name.old]]
    
    if (!is.null(input$check_subset)){
      if (input$check_subset){
        validate(
          need(length(input$var_subset) > 0 , "No variables for subsetting"),
          need(all(sapply(1:length(input$var_subset), function(x){length(input[[paste0("val_subset", x)]])})), "No value for subsetting")
        )
        vars.factor <- factor_vars
        #var.conti <- setdiff(data()$conti_original, input$factor_vname)
        
        for (v in seq_along(input$var_subset)){
          if (input$var_subset[[v]] %in% vars.factor){
            out <- out[get(input$var_subset[[v]]) %in% input[[paste0("val_subset", v)]]]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out[, (vars.factor) := lapply(.SD, factor), .SDcols = vars.factor]
            out.label2 <- mk.lev(out)[, c("variable", "level")]
            data.table::setkey(out.label, "variable", "level")
            data.table::setkey(out.label2, "variable", "level")
            out.label <- out.label[out.label2]
          } else{
            out <- out[get(input$var_subset[[v]]) >= input[[paste0("val_subset", v)]][1] & get(input$var_subset[[v]]) <= input[[paste0("val_subset", v)]][2]]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out[, (vars.factor) := lapply(.SD, factor), .SDcols = vars.factor]
            out.label2 <- mk.lev(out)[, c("variable", "level")]
            data.table::setkey(out.label, "variable", "level")
            data.table::setkey(out.label2, "variable", "level")
            out.label <- out.label[out.label2]
          }
        }
        
      }
    }
    
    return(list(data = out, label = out.label))
  })
  
  data <- reactive(data.info()$data)
  data.label <- reactive(data.info()$label)
  
  
  output$data <- renderDT({
    datatable(data(), rownames=F, editable = F, extensions= "Buttons", caption = "Data",
              options = c(jstable::opt.data("data"), list(scrollX = TRUE))
    )
  })
  
  
  output$data_label <- renderDT({
    datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
              options = c(jstable::opt.data("label"), list(scrollX = TRUE))
    )
  })
  
  
  
  
  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, showAllLevels = T)
  
  output$table1 <- renderDT({
    tb <- out_tb1()$table
    cap <- out_tb1()$caption
    out.tb1 <- datatable(tb, rownames = T, extensions = "Buttons", caption = cap,
                         options = c(jstable::opt.tb1("tb1"),
                                     list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                     ),
                                     list(scrollX = TRUE)
                         )
    )
    if ("sig" %in% colnames(tb)){
      out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    }
    return(out.tb1)
  })
  
  out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
  
  output$lineartable <- renderDT({
    hide = which(colnames(out_linear()$table) == "sig")
    datatable(out_linear()$table, rownames=T, extensions = "Buttons", caption = out_linear()$caption,
              options = c(jstable::opt.tbreg(out_linear()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  output$warning_linear <- renderText({
    paste("<b>", out_linear()$warning, "</b>")
  })
  
  out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
  
  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == "sig")
    datatable(out_logistic()$table, rownames=T, extensions = "Buttons", caption = out_logistic()$caption,
              options = c(jstable::opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = T, nfactor.limit = nfactor.limit)
  
  output$coxtable <- renderDT({
    hide = which(colnames(out_cox()$table) == c("sig"))
    datatable(out_cox()$table, rownames=T, extensions= "Buttons", caption = out_cox()$caption,
              options = c(opt.tbreg(out_cox()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets= hide))
                          )
              )
    )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  
  out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
  
  output$ggpairs_plot <- renderPlot({
    print(out_ggpairs())
  })
  
  out_scatter <- scatterServer("scatter", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
  
  output$scatter_plot <- renderPlot({
    print(out_scatter())
  })
  
  out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
  
  output$kaplan_plot <- renderPlot({
    print(out_kaplan())
  })
  
  out_roc <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
  
  output$plot_roc <- renderPlot({
    print(out_roc()$plot)
  })
  
  output$table_roc <- renderDT({
    datatable(out_roc()$tb, rownames=F, editable = F, extensions= "Buttons",
              caption = "ROC results",
              options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
  })
  
  
  out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
  
  output$plot_timeroc <- renderPlot({
    print(out_timeroc()$plot)
  })
  
  output$table_timeroc <- renderDT({
    datatable(out_timeroc()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
              options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
  })
  
  session$onSessionEnded(function() {
    session$close()
  })
  
}





shinyApp(ui, server)

