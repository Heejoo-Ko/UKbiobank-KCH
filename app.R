options(shiny.sanitize.errors=F)
library(shiny);library(shinycustomloader);library(ggpubr);library(survival);library(jsmodule);library(DT)
#source("global.R")
library(data.table);library(magrittr);library(jstable);library(forestplot);library(shinyWidgets);library(officer)
nfactor.limit <- 20  ## For module

### Load info
info <- readRDS("info.RDS")

## Load fst data: Except MRI
varlist <- info$varlist[names(info$varlist)[c(1:4,6,7)]]
out <- fst::read_fst("data.fst", as.data.table = T, columns = unlist(varlist))

factor_vars <- info$factor_vars
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]

out.label <- info$label

out<-out[!(prev_dementia==1 | prev_parkinson==1 | prev_motor_neuron_disease==1 | prev_IHD==1 | prev_stroke==1),.SD,.SDcols=!c("prev_dementia","prev_parkinson","prev_motor_neuron_disease","prev_IHD","prev_stroke")]
out <- out[dementia_all_day > 0 & parkinson_PD_day > 0 & motor_neuron_disease_day > 0 & MI_all_day > 0 & stroke_all_day > 0]
out.label<-out.label[!grep("prev_",variable),,]


vars.surv <- sapply(strsplit(varlist$Event, "_"), `[[`, 1)

ui <- navbarPage("UK biobank",
                 tabPanel("Data", icon = icon("table"),
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("check_binary", "Make binary variables"),
                              uiOutput("binary_var"),
                              uiOutput("binary_val"),
                              checkboxInput("check_ref", "Change reference of categorical variables"),
                              uiOutput("ref_var"),
                              uiOutput("ref_val"),
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
                 ),
                 tabPanel("Subgroup analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("dep_tbsub", "Outcome", choices = varlist$Event, selected = varlist$Event[1]),
                              selectInput("group_tbsub", "Main variable", varlist$MetS, varlist$MetS[1]),
                              selectInput("subgroup_tbsub", "Subgroup to analyze", intersect(c(varlist$Base, varlist$Medication, varlist$Cognition), factor_vars), "sex", multiple = T),
                              selectInput("cov_tbsub", "Additional covariates", varlist[4:6], selected = NULL, multiple = T),
                              actionBttn("action_tbsub", "Run subgroup analysis")
                            ),
                            mainPanel(
                              withLoader(DTOutput("tablesub"), type="html", loader="loader6"),
                              h3("Download options"),
                              wellPanel(
                                tagList(
                                  column(6,
                                         sliderInput("fig_width_forest", "Width (in):", min = 5, max = 15, value = 12)
                                  ),
                                  column(6,
                                         sliderInput("fig_height_forest", "Height (in):", min = 3, max = 20, value = 5)
                                  )
                                ),
                                downloadButton("forest", "Download forest plot")
                                
                              )
                            )
                          )
                          
                 ),
)

server <- function(input, output, session) {
  
  observeEvent(input$check_binary, {
    var.conti <- setdiff(names(out), factor_vars)
    output$binary_var <- renderUI({
      req(input$check_binary == T)
      selectInput("var_binary", "Variables to dichotomize",
                  choices = var.conti, multiple = T,
                  selected = var.conti[1])
    })
    
    output$binary_val <- renderUI({
      req(input$check_binary == T)
      req(length(input$var_binary) > 0)
      outUI <- tagList()
      for (v in seq_along(input$var_binary)){
        med <- stats::quantile(out[[input$var_binary[[v]]]], c(0.05, 0.5, 0.95), na.rm = T)
        outUI[[v]] <- splitLayout(cellWidths = c("25%", "75%"),
                                  selectInput(paste0("con_binary", v), paste0("Define reference:"),
                                              choices = c("\u2264", "\u2265", "\u003c", "\u003e"), selected = "\u2264"
                                  ),
                                  numericInput(paste0("cut_binary", v), input$var_binary[[v]],
                                               value = med[2], min = med[1], max = med[3]
                                  )
        )
        
      }
      outUI
      
    })
  })
  
  observeEvent(input$check_ref, {
    var.factor <- factor_vars
    output$ref_var <- renderUI({
      req(input$check_ref == T)
      selectInput("var_ref", "Variables to change reference",
                  choices = var.factor, multiple = T,
                  selected = var.factor[1])
    })
    
    output$ref_val <- renderUI({
      req(input$check_ref == T)
      req(length(input$var_ref) > 0)
      outUI <- tagList()
      for (v in seq_along(input$var_ref)){
        outUI[[v]] <- selectInput(paste0("con_ref", v), paste0("Reference: ", input$var_ref[[v]]),
                                  choices = levels(factor(out[[input$var_ref[[v]]]])), selected = levels(factor(out[[input$var_ref[[v]]]]))[2])
        
      }
      outUI
      
    })
  })
  
  
  
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
                    selected = names(out)[1])
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
    
    req(!is.null(input$check_binary))
    if (input$check_binary == T){
      validate(
        need(length(input$var_binary) > 0 , "No variables to dichotomize")
      )
      sym.ineq <- c("\u2264", "\u2265", "\u003c", "\u003e")
      names(sym.ineq) <- sym.ineq[4:1]
      sym.ineq2 <- c("le", "ge", "l", "g")
      names(sym.ineq2) <- sym.ineq
      for (v in seq_along(input$var_binary)){
        req(input[[paste0("con_binary", v)]])
        req(input[[paste0("cut_binary", v)]])
        if (input[[paste0("con_binary", v)]] == "\u2264"){
          out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) <= input[[paste0("cut_binary", v)]]))]
        } else if (input[[paste0("con_binary", v)]] == "\u2265"){
          out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) >= input[[paste0("cut_binary", v)]]))]
        } else if (input[[paste0("con_binary", v)]] == "\u003c"){
          out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) < input[[paste0("cut_binary", v)]]))]
        } else{
          out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) > input[[paste0("cut_binary", v)]]))]
        }
        
        cn.new <- paste0(input$var_binary[[v]], "_group_", sym.ineq2[input[[paste0("con_binary", v)]]], input[[paste0("cut_binary", v)]])
        data.table::setnames(out, "BinaryGroupRandom", cn.new)
        
        label.binary <- mk.lev(out[, .SD, .SDcols = cn.new])
        label.binary[, var_label := paste0(input$var_binary[[v]], " _group")]
        label.binary[, val_label := paste0(c(input[[paste0("con_binary", v)]], sym.ineq[input[[paste0("con_binary", v)]]]), " ", input[[paste0("cut_binary", v)]])]
        out.label <- rbind(out.label, label.binary)
      }
      
    }
    
    if (!is.null(input$check_ref)){
      if (input$check_ref){
        validate(
          need(length(input$var_ref) > 0 , "No variables to change reference")
        )
        for (v in seq_along(input$var_ref)){
          req(input[[paste0("con_ref", v)]])
          out[[input$var_ref[[v]]]] <- stats::relevel(out[[input$var_ref[[v]]]], ref = input[[paste0("con_ref", v)]])
          out.label[variable == input$var_ref[[v]], ':='(level = levels(out[[input$var_ref[[v]]]]), val_label = levels(out[[input$var_ref[[v]]]]))]
        }
        
      }
    }
    
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
  
  
  
  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label=data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, showAllLevels = T)
  
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
  
  out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = reactive(varlist[names(varlist)[c(2, 3, 1, 4)]]), nfactor.limit = nfactor.limit, default.unires = F)
  
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
  
  out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = reactive(varlist[names(varlist)[c(1, 4, 2, 3)]]), nfactor.limit = nfactor.limit, default.unires = F)
  
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
  
  
  data.cox <- reactive({
    return(data())
  })
  
  out_cox <- callModule(coxModule, "cox", data = data.cox, data_label = data.label, data_varStruct = reactive(varlist[names(varlist)[c(2, 3, 1, 4)]]), default.unires = F, nfactor.limit = nfactor.limit)
  
  output$coxtable <- renderDT({
    hide = which(colnames(out_cox()$table) == c("sig"))
    datatable(out_cox()$table, rownames=T, extensions= "Buttons", caption = out_cox()$caption,
              options = c(opt.tbreg(out_cox()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets= hide))
                          )
              )
    )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  
  out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = reactive(varlist[names(varlist)[c(2, 3, 1, 4)]]), nfactor.limit = nfactor.limit)
  
  output$ggpairs_plot <- renderPlot({
    print(out_ggpairs())
  })
  
  out_scatter <- scatterServer("scatter", data = data, data_label = data.label, data_varStruct = reactive(varlist[names(varlist)[c(2, 3, 1, 4)]]), nfactor.limit = nfactor.limit)
  
  output$scatter_plot <- renderPlot({
    print(out_scatter())
  })
  
  data.kap <- reactive({
    return(data())
  })
  
  out_kaplan <- callModule(kaplanModule, "kaplan", data = data.kap, data_label = data.label, data_varStruct = reactive(varlist[names(varlist)[c(2, 3, 1, 4)]]), nfactor.limit = nfactor.limit)
  
  
  output$kaplan_plot <- renderPlot({
    print(out_kaplan())
  })
  
  out_roc <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = reactive(varlist[names(varlist)[c(1, 4, 2, 3)]]), nfactor.limit = nfactor.limit)
  
  output$plot_roc <- renderPlot({
    print(out_roc()$plot)
  })
  
  output$table_roc <- renderDT({
    datatable(out_roc()$tb, rownames=F, editable = F, extensions= "Buttons",
              caption = "ROC results",
              options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
  })
  
  data.timeroc <- reactive({
    return(data())
  })
  
  out_timeroc <- callModule(timerocModule, "timeroc", data = data.timeroc, data_label = data.label, data_varStruct = reactive(varlist[names(varlist)[c(2, 3, 1, 4)]]), nfactor.limit = nfactor.limit)
  
  output$plot_timeroc <- renderPlot({
    print(out_timeroc()$plot)
  })
  
  output$table_timeroc <- renderDT({
    datatable(out_timeroc()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
              options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
  })
  
  
  
  group.tbsub <- reactive({
    input$group_tbsub
  })
  
  
  tbsub <-  eventReactive(input$action_tbsub,{
    
    data <- data()[!is.na(get(group.tbsub()))]
    label <- data.label()
    
    
    var.event <- input$dep_tbsub
    var.day <- varlist$Time[which(varlist$Event == var.event)]
    
    #data <- data[!( get(var.day) <= input$year_tbsub[1]) & !(is.na(get(group.tbsub())))]
    
    #data[[var.event]] <- ifelse(data[[var.day]] >= input$year_tbsub[2] & data[[var.event]] == "1", 0,  as.numeric(as.vector(data[[var.event]])))
    #data[[var.day]] <- ifelse(data[[var.day]] >= input$year_tbsub[2], input$year_tbsub[2], data[[var.day]])
    
    data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
    
    
    form <- as.formula(paste("Surv(", var.day, ",", var.event, ") ~ ", group.tbsub(), sep = ""))
    vs <- input$subgroup_tbsub
    
    tbsub <-  TableSubgroupMultiCox(form, var_subgroups = vs, var_cov = setdiff(input$cov_tbsub, vs),
                                    data=data, time_eventrate = 2, line = F)
    #data[[var.event]] <- ifelse(data[[var.day]] > 365 * 5 & data[[var.event]] == 1, 0,  as.numeric(as.vector(data[[var.event]])))
    
    lapply(vs, 
           function(x){
             cc <- data.table(t(c(x, NA, NA)))
             
             dd <- lapply(levels(data[[group.tbsub()]]),
                          function(y){
                            ev <- data[!is.na(get(x)) & get(group.tbsub()) == y, sum(as.numeric(as.vector(get(var.event)))), keyby = get(x)]
                            nn <- data[!is.na(get(x)) & get(group.tbsub()) == y, .N, keyby = get(x)]
                            vv <- paste0(ev[, V1], "/", nn[, N], " (", round(ev[, V1]/ nn[, N] * 100, 1), ")")
                            data.table(ev[, 1], vv)
                          })
             dd.bind <- cbind(dd[[1]], dd[[2]][, -1])
             names(cc) <- names(dd.bind)
             rbind(cc, dd.bind)
           }) %>% rbindlist -> ll
    
    
    ev.ov <- data[, sum(as.numeric(as.vector(get(var.event)))), keyby = get(group.tbsub())][, V1]
    nn.ov <- data[, .N, keyby = get(group.tbsub())][, N]
    
    ov <- data.table(t(c("OverAll", paste0(ev.ov, "/", nn.ov, " (", round(ev.ov/nn.ov * 100, 1), ")"))))
    names(ov) <- names(ll)
    cn <- rbind(ov, ll)
    
    names(cn)[-1] <- label[variable == group.tbsub(), val_label]
    tbsub <- cbind(Variable = tbsub[, 1], cn[, -1], tbsub[, c(7, 8, 4, 5, 6, 9, 10)])
    
    tbsub[-1, 1] <- unlist(lapply(vs, function(x){c(label[variable == x, var_label][1], paste0("     ", label[variable == x, val_label]))}))
    colnames(tbsub)[1:6] <- c("Subgroup", paste0("N(%): ", label[variable == group.tbsub(), val_label]), paste0(paste(input$year_tbsub, collapse = "~"), "-year KM rate(%): ", label[variable == group.tbsub(), val_label]), "HR")
    #colnames(tbsub)[c(4)] <- c("HR")
    #tbsub[27:30, Subgroup := c("EES", "ZES", "BES", "Others")]
    
    return(tbsub)
  })
  
  output$tablesub <- renderDT({
    
    datatable(tbsub()[, .SD, .SDcols = -c(4, 5)], caption = paste0(input$dep_tbsub, " subgroup analysis"), rownames = F, extensions= "Buttons",
              options = c(opt.tb1(paste0("tbsub_original")),
                          list(scrollX = TRUE, columnDefs = list(list(className = 'dt-right', targets = 0)))
              )) 
    
  })
  
  
  
  
  
  output$forest <- downloadHandler(
    filename =  function() {
      paste(input$dep_tbsub,"_forestplot.pptx", sep="")
      
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     
                     #devEMF::emf(file, width = 15, height = 5, coordDPI = 300, emfPlus = T)
                     data <- tbsub()
                     
                     tabletext <- cbind(c("Subgroup","\n",data$Subgroup),
                                        c("N(%)\n0", "\n" , data[[2]]),
                                        c("N(%)\n1", "\n", data[[3]]),
                                        c("P Value","\n",data$`P value`),
                                        c("P for interaction","\n",data$`P for interaction`))
                     
                     tabletext <- tabletext[, c(1,2,3, 4, 5)]
                     ## Save as tiff 
                     forestplot::forestplot(labeltext=tabletext, graph.pos=4, xticks = c(0.5, 1, 2), xlog= T, align = c("r", rep("c", ncol(tabletext) - 1)),                          ## graph.pos- column number
                                            mean=c(NA,NA,as.numeric(data$HR)), 
                                            lower=c(NA,NA,as.numeric(data$Lower)), upper=c(NA,NA,as.numeric(data$Upper)),
                                            title="Hazard Ratio",
                                            xlab="<---non-MetS-risky ---    ---MetS risky --->",    ## You cas modify this.
                                            hrzl_lines=list("3" = gpar(lwd=1, col="#99999922")
                                            ),
                                            
                                            txt_gp=fpTxtGp(label=gpar(cex=1.25),
                                                           ticks=gpar(cex=1.1),
                                                           xlab=gpar(cex = 1.2),
                                                           title=gpar(cex = 1.2)),
                                            col=fpColors(box="black", lines="black", zero = "gray50"),
                                            zero=1, cex=0.9, lineheight = "auto", boxsize=0.4, colgap=unit(6,"mm"),
                                            lwd.ci=2, ci.vertices=F, ci.vertices.height = 0.4) -> zz
                     #print(zz)
                     my_vec_graph <- rvg::dml(code = print(zz))
                     
                     doc <- officer::read_pptx()
                     doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                     doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width_forest, height = input$fig_height_forest))
                     print(doc, target = file)
                   })
      
    }
  )
  
  session$onSessionEnded(function() {
    session$close()
  })
  
}


shinyApp(ui, server)