#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd('/home/whe')
library(shiny)
library(knitr)
library(data.table)
library(fst)             ## For Fst binary format
library(lubridate)
library(magrittr)
library(stringr)
library(parallel)
library(crosstalk)
library(DT)
library(tableone)
library(jstable)
library(survival)
library(jstable)

PPI <- fread("PPI.csv")
PPI <- PPI[AGE_GROUP>4]
PPIpr <- PPI[,c(49, 50, 25:46, 8, 47, 48, 18, 16, 21, 23)]
PPIpr
aa <- lapply(5:18, function(x){
    return(as.integer(PPIpr$AGE_GROUP==x))
}) %>% Reduce(cbind,.)
colnames(aa) <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                  "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", ">85")
PPIpr <- cbind(cbind(PPIpr[, 1:2], aa),PPIpr[, 3:31])


PPIpr[, SEX := SEX-1]

PPIpr2 <- PPIpr

PPIpr2 <- lapply(1:ncol(PPIpr),function(x){
  p <- PPIpr[, x, with = FALSE]
  colnames(p) <- c("pp")
  p$pp <- as.character(p$pp)
  return(p)
}) %>% Reduce(cbind, .)
rownames(PPIpr2) <- rownames(PPIpr)
colnames(PPIpr2) <- colnames(PPIpr)
PPIpr2 <- PPIpr2[,c(1,2,17:45)]

CreateTableOneJS(vars=names(PPIpr2),strata="EXPCON",data=PPIpr2,argsNormal=list(var.equal=F),smd=T) -> PPIprCTOJ
PPIprCTOJ <- as.data.table(PPIprCTOJ)
PPIprCTOJup <- PPIprCTOJ[1:17, ]
PPIprCTOJdown <- PPIprCTOJ[18:75, ]
PPIprCTOJup[3, c(4, 6)] <- PPIprCTOJup[2, c(4, 6)]
for(x in 1:58){
  PPIprCTOJdown[2*x, c(4, 6)] <- PPIprCTOJdown[2*x-1, c(4, 6)]
}
PPIprCTOJ <- rbind(PPIprCTOJup, PPIprCTOJdown)
PPIprCTOJ <- PPIprCTOJ[table.level != 0]
PPIprCTOJ <- PPIprCTOJ[-c(45,46),]
PPIprCTOJ <- PPIprCTOJ[c(1,2,12:16,3:11,17:44),]
PPIprCTOJ[3, c(4, 6)] <- PPIprCTOJ[8, c(4, 6)]
PPIprCTOJ[8, c(4, 6)] <- PPIprCTOJ[7, c(4, 6)]



PPITABLE1 <- lapply(c(1,3:44),function(x){
    c(sum(PPIpr[EXPCON==1][, x, with = FALSE]),
      sum(PPIpr[EXPCON==2][, x, with=FALSE]))
}) %>% Reduce(rbind, .)

ppinm <- c("SEX = Female (%)", "20-24", "25-29", 
           "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
           "70-74", "75-79", "80-84", ">85", paste0(colnames(PPI[,25:46])," = Yes (%)"),
           "AKI1", "AKI2", "AKI3", "CKD", "ADVCKD", "ESRD")

rownames(PPITABLE1) <- ppinm

PPIpr <- rbind("n"=c(nrow(PPIpr[EXPCON==1]),nrow(PPIpr[EXPCON==2])),PPITABLE1)
colnames(PPIpr) <- c("Non-PPI","PPI")

PPIpv <- lapply(1:44,function(x){
    bb <- PPIpr[c(1,x),1:2]
    bb[1, ] <- bb[1, ] - bb[2, ]
    vv <- chisq.test(bb)$p.value
    return(vv)
}) %>% Reduce(rbind, .)

PPIpv <- round(PPIpv, digits = 4)
colnames(PPIpv) <- c("p")
PPIpr <- cbind(PPIpr, PPIpv)

PPIpercent <- lapply(1:44, function(x){
    c(paste0(as.integer(PPIpr[as.integer(x), 1]), " (", 100 * round(as.integer(PPIpr[as.integer(x), 1]) / as.integer(PPIpr[1, 1]), digits=4), ")"),
      paste0(as.integer(PPIpr[as.integer(x), 2]), " (", 100 * round(as.integer(PPIpr[as.integer(x), 2]) / as.integer(PPIpr[1, 2]), digits=4), ")"))
}) %>% Reduce(rbind, .)

PPIpr[, 1] <- PPIpercent[, 1]
PPIpr[, 2] <- PPIpercent[, 2]


PPIprCTOJ <- PPIprCTOJ[,-1]
names(PPIprCTOJ) <- c("NON-PPI","PPI","p","table.test","SMD","table.sig","caption")
PPIprCTOJ <- PPIprCTOJ[,.(`NON-PPI`, PPI, p, SMD)]
rownames(PPIprCTOJ) <- rownames(PPIpr)


##### PPI 완성

H2RA <- fread("H2RA.csv")
H2RA <- H2RA[AGE_GROUP>4]
H2RApr <- H2RA[,c(49, 50, 25:46, 8, 47, 48, 18, 16, 21, 23)]
H2RApr
aa <- lapply(5:18, function(x){
  return(as.integer(H2RApr$AGE_GROUP==x))
}) %>% Reduce(cbind,.)
colnames(aa) <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                  "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", ">85")
H2RApr <- cbind(cbind(H2RApr[, 1:2], aa),H2RApr[, 3:31])


H2RApr[, SEX := SEX-1]

H2RApr2 <- H2RApr

H2RApr2 <- lapply(1:ncol(H2RApr),function(x){
  p <- H2RApr[, x, with = FALSE]
  colnames(p) <- c("pp")
  p$pp <- as.character(p$pp)
  return(p)
}) %>% Reduce(cbind, .)
rownames(H2RApr2) <- rownames(H2RApr)
colnames(H2RApr2) <- colnames(H2RApr)
H2RApr2 <- H2RApr2[,c(1,2,17:45)]

CreateTableOneJS(vars=names(H2RApr2),strata="EXPCON",data=H2RApr2,argsNormal=list(var.equal=F),smd=T) -> H2RAprCTOJ
H2RAprCTOJ <- as.data.table(H2RAprCTOJ)
H2RAprCTOJup <- H2RAprCTOJ[1:17, ]
H2RAprCTOJdown <- H2RAprCTOJ[18:75, ]
H2RAprCTOJup[3, c(4, 6)] <- H2RAprCTOJup[2, c(4, 6)]
for(x in 1:58){
  H2RAprCTOJdown[2*x, c(4, 6)] <- H2RAprCTOJdown[2*x-1, c(4, 6)]
}
H2RAprCTOJ <- rbind(H2RAprCTOJup, H2RAprCTOJdown)
H2RAprCTOJ <- H2RAprCTOJ[table.level != 0]
H2RAprCTOJ <- H2RAprCTOJ[-c(45,46),]
H2RAprCTOJ <- H2RAprCTOJ[c(1,2,12:16,3:11,17:44),]
H2RAprCTOJ[3, c(4, 6)] <- H2RAprCTOJ[8, c(4, 6)]
H2RAprCTOJ[8, c(4, 6)] <- H2RAprCTOJ[7, c(4, 6)]



H2RATABLE1 <- lapply(c(1,3:44),function(x){
  c(sum(H2RApr[EXPCON==1][, x, with = FALSE]),
    sum(H2RApr[EXPCON==2][, x, with=FALSE]))
}) %>% Reduce(rbind, .)

H2RAnm <- c("SEX = Female (%)", "20-24", "25-29", 
           "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
           "70-74", "75-79", "80-84", ">85", paste0(colnames(H2RA[,25:46])," = Yes (%)"),
           "AKI1", "AKI2", "AKI3", "CKD", "ADVCKD", "ESRD")

rownames(H2RATABLE1) <- H2RAnm

H2RApr <- rbind("n"=c(nrow(H2RApr[EXPCON==1]),nrow(H2RApr[EXPCON==2])),H2RATABLE1)
colnames(H2RApr) <- c("Non-H2RA","H2RA")

H2RApv <- lapply(1:44,function(x){
  bb <- H2RApr[c(1,x),1:2]
  bb[1, ] <- bb[1, ] - bb[2, ]
  vv <- chisq.test(bb)$p.value
  return(vv)
}) %>% Reduce(rbind, .)

H2RApv <- round(H2RApv, digits = 4)
colnames(H2RApv) <- c("p")
H2RApr <- cbind(H2RApr, H2RApv)

H2RApercent <- lapply(1:44, function(x){
  c(paste0(as.integer(H2RApr[as.integer(x), 1]), " (", 100 * round(as.integer(H2RApr[as.integer(x), 1]) / as.integer(H2RApr[1, 1]), digits=4), ")"),
    paste0(as.integer(H2RApr[as.integer(x), 2]), " (", 100 * round(as.integer(H2RApr[as.integer(x), 2]) / as.integer(H2RApr[1, 2]), digits=4), ")"))
}) %>% Reduce(rbind, .)

H2RApr[, 1] <- H2RApercent[, 1]
H2RApr[, 2] <- H2RApercent[, 2]


H2RAprCTOJ <- H2RAprCTOJ[,-1]
names(H2RAprCTOJ) <- c("NON-H2RA","H2RA","p","table.test","SMD","table.sig","caption")
H2RAprCTOJ <- H2RAprCTOJ[,.(`NON-H2RA`, H2RA, p, SMD)]
rownames(H2RAprCTOJ) <- rownames(H2RApr)



# H2RA 완성


# Define UI for application that draws a histogram
ui <- navbarPage(title = "PPIH2RA", 
                 tabPanel("Table 1",
                          sidebarLayout(
                              sidebarPanel(
                                  radioButtons(inputId="Study",label="Study",choices=list("PPI vs 나머지","H2RA vs 나머지", "PPI vs 나머지 Table", "H2RA vs 나머지 Table"),inline=TRUE),
                                  "HTA (1, 365, 365), HTA(2, 365, 365) 기준"
                              ),
                              mainPanel(
                                  DT::DTOutput("Table1"),
                                  
                                 
                              )
                          )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    output$Table1 <- DT::renderDT({
        if(input$Study == "PPI vs 나머지"){
            pr <- PPIpr
        } 
        if(input$Study == "H2RA vs 나머지"){
          pr <- H2RApr 
        }
      if(input$Study == "PPI vs 나머지 Table" ){
        pr <- PPIprCTOJ
      }
      
      if(input$Study == "H2RA vs 나머지 Table"){
        pr <- H2RAprCTOJ 
      }
      

       datatable(pr, options = list(
           pageLength = 25,
           lengthMenu = c(10, 25, 50)


       )) %>% DT::formatStyle('p', target = "row", backgroundColor = styleEqual(c("<0.001"),c('yellow')))
       #%>% DT::formatStyle('p', target = "row", backgroundColor = styleInterval(0.01, c('yellow','default')))

                              
         #DT::formatStyle('p', target = "row", backgroundColor = styleInterval(0.01, c('yellow','default')))
         #DT::formatStyle('p', target = "row", backgroundColor = styleEqual(c("<0.001",0),c('yellow','yellow')))
       
        
    })
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
