library(data.table)
library(fst)
library(jstable)
library(lubridate)


varlist <- c('SEX', 'AGE_GROUP', 'Pre_Smoking', 'Pre_Alcohol', 'Pre_HTN', 'Pre_DM', 'Pre_Dyslipidemia', 'Pre_Obesity', 
             'Pre_IHD', 'Pre_Afib', 'Pre_CHF', 'Pre_Stroke', 'Pre_Cirrhosis', 'Pre_GERD', 'Pre_G_ulcer', 'Pre_D_ulcer', 'Pre_Statin', 
             'Pre_Metformin', 'Pre_Aspirin', 'Pre_NSAIDs_COX2', 'Pre_Clopidogrel', 'Pre_H2RA',
             'CKD', 'ADVCKD', 'ESRD', 'EXPCON', 'CCI')

#data.ESRD <- fread("/home/whe/ESRD.csv")

mkdata <- function(file){
  #data <- fread("PPIvsH2RA.csv")[AGE_GROUP > 4]
  data <- fread(file)[AGE_GROUP > 4]
  data[, `:=`(ADVCKD = as.integer(start.ADVCKD != ""),
              CKD = as.integer(start.CKD != ""), 
              ESRD = as.integer(start.ESRD != ""))]
  
  data[, `:=`(ADVCKD_Day = ifelse(ADVCKD == 1, ymd(start.ADVCKD) - ymd(indexdate), ymd("2013-12-31") - ymd(indexdate)),
              CKD_Day = ifelse(CKD == 1, ymd(start.CKD) - ymd(indexdate), ymd("2013-12-31") - ymd(indexdate)),
              ESRD_Day = ifelse(ESRD == 1, ymd(start.ESRD) - ymd(indexdate), ymd("2013-12-31") - ymd(indexdate)),
              EXPCON = ifelse(EXPCON == 2, 0, 1))]
  
  
  data[, CCI := CCI + as.integer(AGE_GROUP >= 9) + as.integer(AGE_GROUP >= 11) + as.integer(AGE_GROUP >= 13) + as.integer(AGE_GROUP >= 15)]
  
  var.factor <- setdiff(varlist, "CCI")
  data[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
  return(data[ADVCKD_Day >= 0 & CKD_Day >= 0, .SD, .SDcols = c(varlist, paste0(c("ADVCKD", "CKD", "ESRD"), "_Day"))])
}


#setwd("~/ShinyApps/renal")
#z <- mkdata("data/PPI_2_PPI_1.csv")




## Matching function : 함수실행 X,  함수내부 코드 그대로 실행
matchfun <- function(data, PPI = T){
  dlist.original <- mclapply(file.path("data", list.files("data")), mkdata)
  names(dlist.original) <- list.files("data")
  
  wo <- 180
  mclapply(seq_along(dlist.original), function(i){
    data <- dlist.original[[i]][ADVCKD_Day > wo & CKD_Day > wo]
    
    if (grepl("H2RA", names(dlist.original)[i])){
      data <- data[, -c("Pre_H2RA")]
    }
    
    data$CCI <- as.integer(as.character(data$CCI))
    data$EXPCON2 <- as.integer(as.character(data$EXPCON))
    if (data[, diff(table(EXPCON)) > 0]){
      data$EXPCON2 <- 1 - as.integer(as.character(data$EXPCON))
    }
    form <- paste0("EXPCON2 ~ SEX + CCI + ", paste(grep("Pre_", names(data), value = T), collapse = " + "))
    modelps <- glm(as.formula(form), data = data, family = binomial)
    rr  <- Matching::Match(Tr=data$EXPCON2, X=modelps$fitted, M=1, replace = F, ties = F, version = "fast", caliper = 0.2)
    data$ps <- modelps$fitted
    return(list(original = data[, -c("EXPCON2")], mat = data[c(rr$index.treated, rr$index.control), -c("EXPCON2")]))
  }) -> dinfo
  
  names(dinfo) <- list.files("data")
  dinfo
  saveRDS(dinfo, "dataCKD.RDS")
}


## Data
dinfo <- readRDS("dataCKD.RDS")


## Label
label <- jstable::mk.lev(dinfo[[2]]$original)
## value with 0/1
vars01 <- names(dinfo[[2]][1])[sapply(names(dinfo[[2]][1]), function(x){identical(levels(dinfo[[2]][1][[x]]), c("0", "1"))})]

for (v in vars01){
  label[variable == v, val_label := c("No", "Yes")]
}

label[variable == "SEX", `:=`(var_label = "Sex", val_label = c("Male", "Female"))]
label[variable == "AGE_GROUP", `:=`(var_label = "Age Group",
                                    val_label = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                  "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", ">85"))]


label[variable == "EXPCON", `:=`(var_label = "Group", val_label = c("Control", "PPI"))]
#label[variable == "AKI2", `:=`(var_label = "AKI with (O7020, O9991)")]
#label[variable == "AKI3", `:=`(var_label = "AKI with (O7032, O7052)")]
#label[variable == "Pre_PPI", val_label := c("No", "Yes")]


## Make table 1

#list.tb1 <- mclapply(dinfo, function(x){
#  lapply(c("original", "mat"), function(v){
#    CreateTableOneJS(vars = setdiff(names(x[[v]]), c("ps", "EXPCON")), strata = "EXPCON", data = x[[v]], Labels = T, labeldata = label, smd = T,
#                     nonnormal = grep("_Day", names(x[[v]]), value = T), argsExact = list(simulate.p.value = T))$table
#  })
#  
#})

#saveRDS(list.tb1, "tb1CKD.RDS")


list.tb1 <- readRDS("tb1CKD.RDS")
