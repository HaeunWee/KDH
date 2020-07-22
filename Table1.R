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
# PERSON_ID
# STND_Y : indexdate의 년도.
# o1 : 행위코드에 O7020 | O9991 존재
# period.o1 : 행위코드에 o1 이 처음 발생한 날로부터 마지막으로 발생한 날 + 처방일수까지
# start.o1 : 행위코드에 처음으로 o1이 발생한 날
# o2, period.o2, start.o2 마찬가지
# start.ppi, period.ppi, freq.ppi, start.h2ra, period.h2ra, freq.h2ra : 위암에서의 ppi, h2ra와 동일
# ADVCKD : ADVCKD 상병코드 (N183 N184 N185) 발생여부
# start.ADVCKD : ADVCKD 상병코드 발생일
# period.bm 복막투석액 처방기간, 첫 처방일로부터 마지막 처방일까지 + 처방일수
# ESRD : ESRD의 조작적 정의를 만족하는지여부 0, 1
# start.come 첫 내원일
# EXPCON : 실헙군(1), 대조군(2)
# PPI 의 경우 PPI를 처방받기 전에 적어도 1년 전의 기록이 있는 환자만 실험군이 되고
# 1년 전의 기록이 없는 환자는 삭제, PPI를 복용하지 않은 환자는 대조군이 됨.
# H2RA 는 PPI 와 동일한 구조이나 PPI가 H2RA로 바뀌는 것.
# indexdate : 실험군은 PPI/H2RA를 첫 처방받기 1년 전, 대조군은 첫 내방일.
# ~ 과거력 ~
# AKI2 투석치료를 요하는 AKI 여부
# AKI3 입원치료를 요하는 AKI 여부

#PPI[, start.ESRD := ifelse(ESRD==1,start.ADVCKD,NA)]
#PPI[, day.AKI := ymd(start.AKI) - ymd(start.come)]
#PPI[, day.CKD := ymd(start.CKD) - ymd(start.come)]
#PPI[, day.ADVCKD := ymd(start.ADVCKD) - ymd(start.come)]
#PPI[, day.ESRD := ymd(start.ESRD) - ymd(start.come)]
#PPI[, day.AKICON := as.integer(is.na(day.AKI))*(ymd(20131231)-ymd(start.come))]
#PPI[day.AKICON==0]$day.AKICON <- NA
#PPI[, day.CKDCON := as.integer(is.na(day.CKD))*(ymd(20131231)-ymd(start.come))]
#PPI[day.CKDCON==0]$day.CKDCON <- NA
#PPI[, day.ADVCKDCON := as.integer(is.na(day.ADVCKD))*(ymd(20131231)-ymd(start.come))]
#PPI[day.ADVCKDCON==0]$day.ADVCKDCON <- NA
#PPI[, day.ESRDCON := as.integer(is.na(day.ESRD))*(ymd(20131231)-ymd(start.come))]
#PPI[day.ESRDCON==0]$day.ESRDCON <- NA

#H2RA[, start.ESRD := ifelse(ESRD==1,start.ADVCKD,NA)]
#H2RA[, day.AKI := ymd(start.AKI) - ymd(start.come)]
#H2RA[, day.CKD := ymd(start.CKD) - ymd(start.come)]
#H2RA[, day.ADVCKD := ymd(start.ADVCKD) - ymd(start.come)]
#H2RA[, day.ESRD := ymd(start.ESRD) - ymd(start.come)]
#H2RA[, day.AKICON := as.integer(is.na(day.AKI))*(ymd(20131231)-ymd(start.come))]
#H2RA[day.AKICON==0]$day.AKICON <- NA
#H2RA[, day.CKDCON := as.integer(is.na(day.CKD))*(ymd(20131231)-ymd(start.come))]
#H2RA[day.CKDCON==0]$day.CKDCON <- NA
#H2RA[, day.ADVCKDCON := as.integer(is.na(day.ADVCKD))*(ymd(20131231)-ymd(start.come))]
#H2RA[day.ADVCKDCON==0]$day.ADVCKDCON <- NA
#H2RA[, day.ESRDCON := as.integer(is.na(day.ESRD))*(ymd(20131231)-ymd(start.come))]
#H2RA[day.ESRDCON==0]$day.ESRDCON <- NA


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
