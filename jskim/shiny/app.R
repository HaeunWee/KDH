source("global.R")
library(shiny)
library(magrittr)
library(DT)
library(jstable)
library(survival)
library(shinycustomloader)




# Define UI for application that draws a histogram
ui <- navbarPage(title = "PPIH2RA", 
                 tabPanel("Table 1",
                          sidebarLayout(
                              sidebarPanel(
                                  radioButtons(inputId="Study", label="Study", choices=list("PPI vs 나머지", "H2RA vs 나머지"), inline = T),
                                  "HTA (1, 365, 365), HTA(2, 365, 365) 기준"
                              ),
                              mainPanel(
                                withLoader(DTOutput("Table1"), type="html", loader="loader6")
                                  
                                 
                              )
                          )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
    data.tb1 <- reactive({
      switch(input$Study, 
             "PPI vs 나머지" = data.PPI,
             "H2RA vs 나머지" = data.H2RA)
    })

    output$Table1 <- renderDT({
      tb1 <- CreateTableOneJS(vars = setdiff(names(data.tb1()), "EXPCON"), strata = "EXPCON", data = data.tb1(), Labels = T, labeldata = label, smd = T)$table
      
      datatable(tb1, caption = paste0("Original"), rownames = T, extensions= "Buttons",
                options = c(opt.tb1("tb1"),
                            list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb1) %in% c("test","sig"))))
                            ),
                            list(scrollX = TRUE)
                )) %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    })
       
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
