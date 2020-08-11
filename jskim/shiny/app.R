source("global.R")
library(shiny)
library(magrittr)
library(DT)
library(jstable)
library(survival)
library(shinycustomloader)
library(jskm)


Makekaplan <- function(event.original = "TLF", day.original = "TLFDay", var.group = "SBintervention", data = out, data.label = out.label, timeby = 365){
  var.event <- event.original
  var.day <- day.original
  form <- as.formula(paste("Surv(", var.day, ",", var.event, ") ~ ", var.group, sep = ""))
  
  data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
  #data[[var.day]] <- data[[var.day]]
  
  res.kap <- survfit(form, data = data)
  res.kap$call$formula <- form
  p <- jskm(res.kap, xlabs = "Days", ylab = "Cumulative Incidence", cumhaz = T, ylims = c(0, 0.05), 
            ystrataname = data.label[variable == var.event, var_label][1], ystratalabs = data.label[variable == var.group][level %in% levels(data[[var.group]]), val_label],
            pval.coord = c(100, 0.01), legendposition = c(0.3, 0.8), timeby = timeby,
            surv.scale = "percent", mark = F, pval = T, table = T, data = data)
  
  
  return(p)
  
  
  
}


label.dinfo <- c("PPI 30 days vs non-PPI" = 2, "PPI 60 days vs non-PPI" = 5, "PPI 90 days vs non-PPI" = 8, "PPI 180 days vs non-PPI" = 11,
  "PPI 30 days vs PPI < 30 days" = 3, "PPI 60 days vs PPI < 30 days" = 6, "PPI 90 days vs PPI < 30 days" = 9, "PPI 180 days vs PPI < 30 days" = 12,
  "PPI 30 days vs H2RA 30 days" = 1, "PPI 60 days vs H2RA 60 days" = 4, "PPI 90 days vs H2RA 90 days" = 7, "PPI 180 days vs H2RA 180 days" = 10)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "CKD", 
                 tabPanel("Table 1",
                          sidebarLayout(
                              sidebarPanel(
                                  radioButtons(inputId="Study", label="Study", choices= label.dinfo, inline = T),
                                  radioButtons("mat_tb1", "Data type", choices = c("Original", "Matching"), selected = "Matching", inline = T)
                                  
                              ),
                              mainPanel(
                                withLoader(DTOutput("table1"), type="html", loader="loader6")
                                
                              )
                          )),
                 tabPanel("Cox model",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId="Study_cox", label="Study", choices= label.dinfo, inline = T),
                              radioButtons("mat_cox", "Data type", choices = c("Original", "Matching"), selected = "Matching", inline = T),
                              radioButtons("dep_cox", "Outcome", choices = c( "ADVCKD", "CKD", "ESRD"), selected = "AKI", inline = T),
                              selectInput("cov_cox", "Covariate", choices = c("EXPCON", "SEX", "AGE_GROUP", grep("Pre_", names(dinfo[[2]]$original), value = T)), selected = "EXPCON", multiple = T)
                            ),
                            mainPanel(
                              withLoader(DTOutput("tablecox"), type="html", loader="loader6")
                              )
                            )
                          ),
                        
                 
                 tabPanel("Kaplan-meier plot",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId="Study_kap", label="Study", choices= label.dinfo, inline = T),
                              radioButtons("mat_kap", "Data type", choices = c("Original", "Matching"), selected = "Matching", inline = T),
                              radioButtons("event_kap", "Event", choices = c( "ADVCKD", "CKD", "ESRD"), selected = "AKI", inline = T)
                            ),
                            mainPanel(
                              withLoader(plotOutput("kap"), type="html", loader="loader6"),
                              h3("Download options"),
                              wellPanel(
                                uiOutput("downloadControls_kap"),
                                downloadButton("downloadButton_kap", label = "Download the plot")
                              )
                            )
                          )
                          
                 ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  tb1 <- reactive({
    switch(input$mat_tb1, 
           "Original" = list.tb1[[as.integer(input$Study)]][[1]],
           "Matching" = list.tb1[[as.integer(input$Study)]][[2]])
  })
  
  output$table1 <- renderDT({
    out.tb1 <- tb1()
    
    colnames(out.tb1)[2:3] <- strsplit(names(which(label.dinfo == as.integer(input$Study))), " vs ")[[1]][2:1]
    datatable(out.tb1, caption = paste0("Original"), rownames = T, extensions= "Buttons",
              options = c(opt.tb1("tb1"),
                          list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb1()) %in% c("test","sig"))))
                          ),
                          list(scrollX = TRUE)
              )) %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
  })
  
  
  
  data.cox <- reactive({
    switch(input$mat_cox, 
           "Original" = dinfo[[as.integer(input$Study_cox)]][[1]],
           "Matching" = dinfo[[as.integer(input$Study_cox)]][[2]])
  })
  
  output$tablecox <- renderDT({
    validate(
      need(!is.null(input$cov_cox), "Please select at least 1 independent variable.")
    )
    data <- data.cox()
    label <- label[, .SD]
    label[variable == "EXPCON", val_label := strsplit(names(which(label.dinfo == as.integer(input$Study_cox))), " vs ")[[1]][2:1]]
    
    
    var.event <- input$dep_cox
    var.day <- paste0(var.event, "_Day")
    
    
    data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
    
    forms.cox <- as.formula(paste("Surv(", var.day,",", var.event,") ~ ", paste(input$cov_cox, collapse = "+"), sep=""))
    
    
    cc <- substitute(survival::coxph(.form, data= data, model = T), list(.form= forms.cox))
    res.cox <- eval(cc)
    tb.cox <- jstable::cox2.display(res.cox, dec = 2)
    tb.cox <- jstable::LabeljsCox(tb.cox, ref = label)
    out.cox <- rbind(tb.cox$table, tb.cox$metric)
    sig <- out.cox[, ncol(out.cox)]
    sig <- gsub("< ", "", sig)
    sig <- ifelse(as.numeric(as.vector(sig)) <= 0.05, "**", NA)
    out.cox <- cbind(out.cox, sig)
    
    cap.cox <- paste("Cox's proportional hazard model on time ('", label[variable == var.day, var_label][1] , "') to event ('", label[variable == var.event, var_label][1], "')", sep="")
    
    hide <- which(colnames(out.cox) == c("sig"))
    datatable(out.cox, rownames=T, extensions= "Buttons", caption = cap.cox,
              options = c(opt.tbreg(cap.cox),
                          list(columnDefs = list(list(visible=FALSE, targets= hide))
                          )
              )
    )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    
    
  })
  
  data.kap <- reactive({
    switch(input$mat_kap, 
           "Original" = dinfo[[as.integer(input$Study_kap)]][[1]],
           "Matching" = dinfo[[as.integer(input$Study_kap)]][[2]])
  })
  
  
  obj.km <- reactive({
    req(input$event_kap)
    
    data <- data.kap()
    label <- label[, .SD]
    label[variable == "EXPCON", val_label := strsplit(names(which(label.dinfo == as.integer(input$Study_kap))), " vs ")[[1]][2:1]]
    
    
    
    Makekaplan(event.original = input$event_kap, day.original = paste0(input$event_kap, "_Day"),  var.group = "EXPCON", data = data, data.label = label,
               timeby = 365) 
    
  })
  
  output$kap <- renderPlot({
    print(obj.km())
  })
  
  output$downloadControls_kap <- renderUI({
    fluidRow(
      column(4,
             selectizeInput("kap_file_ext", "File extension (dpi = 300)", 
                            choices = c("jpg","pdf", "tiff", "svg", "emf"), multiple = F, 
                            selected = "jpg"
             )
      ),
      column(4,
             sliderInput("fig_width_kap", "Width (in):",
                         min = 5, max = 20, value = 8
             )
      ),
      column(4,
             sliderInput("fig_height_kap", "Height (in):",
                         min = 5, max = 20, value = 6
             )
      )
    )
  })
  
  output$downloadButton_kap <- downloadHandler(
    filename =  function() {
      paste(input$event_kap, "_", input$data_kap, "_", input$group_kap , "_plot.", input$kap_file_ext ,sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     if (input$kap_file_ext == "emf"){
                       devEMF::emf(file, width = input$fig_width_kap, height =input$fig_height_kap, coordDPI = 300, emfPlus = F)
                       plot(obj.km())
                       dev.off()
                       
                     } else{
                       ggsave(file, obj.km(), dpi = 300, units = "in", width = input$fig_width_kap, height =input$fig_height_kap)
                     }
                     
                   })
      
      
    })
  
    
    
       
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
