library(data.table)
library(fst)
library(jstable)
library(lubridate)


varlist <- c('SEX', 'AGE_GROUP', 'Pre_Smoking', 'Pre_Alcohol', 'Pre_HTN', 'Pre_DM', 'Pre_Dyslipidemia', 'Pre_Obesity', 
             'Pre_IHD', 'Pre_Afib', 'Pre_CHF', 'Pre_Stroke', 'Pre_Cirrhosis', 'Pre_GERD', 'Pre_G_ulcer', 'Pre_D_ulcer', 'Pre_Statin', 
             'Pre_Metformin', 'Pre_Aspirin', 'Pre_NSAIDs_COX2', 'Pre_Clopidogrel', 'Pre_H2RA', 'Pre_PPI', 'AKI', 'AKI2', 'AKI3', 
             'CKD', 'ADVCKD', 'ESRD', 'EXPCON')

#data.ESRD <- fread("/home/whe/ESRD.csv")

mkdata <- function(file){
  data <- merge(fread(file)[AGE_GROUP > 4], data.ESRD[, .(PERSON_ID, N185DATE)], by = "PERSON_ID")
  data[, `:=`(AKI = as.integer(start.AKI != ""),
              AKI2 = as.integer(start.AKI != "") * o1,
              AKI3 = as.integer(start.AKI != "") * o2,
              ADVCKD = as.integer(start.ADVCKD != ""),
              CKD = as.integer(start.CKD != ""), 
              ESRD = as.integer(N185DATE != ""))]
  
  data[, `:=`(AKI_Day = ifelse(AKI == 1, ymd(start.AKI) - ymd(indexdate), ymd("2013-12-31") - ymd(indexdate)),
              AKI2_Day = ifelse(AKI2 == 1, ymd(start.AKI) - ymd(indexdate), ymd("2013-12-31") - ymd(indexdate)),
              AKI3_Day = ifelse(AKI3 == 1, ymd(start.AKI) - ymd(indexdate), ymd("2013-12-31") - ymd(indexdate)),
              ADVCKD_Day = ifelse(ADVCKD == 1, ymd(start.ADVCKD) - ymd(indexdate), ymd("2013-12-31") - ymd(indexdate)),
              CKD_Day = ifelse(CKD == 1, ymd(start.CKD) - ymd(indexdate), ymd("2013-12-31") - ymd(indexdate)),
              ESRD_Day = ifelse(ESRD == 1, ymd(start.ADVCKD) - ymd(indexdate), ymd("2013-12-31") - ymd(indexdate)),
              EXPCON = ifelse(EXPCON == 2, 0, 1))]
  
  var.factor <- varlist
  data[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
  return(data[AKI_Day >= 0 & ADVCKD_Day >= 0 & CKD_Day >= 0, .SD, .SDcols = c(varlist, paste0(c("AKI", "AKI2", "AKI3", "ADVCKD", "CKD", "ESRD"), "_Day"))])
}


#setwd("~/ShinyApps/renal")
#data.PPI <- mkdata("PPI.csv")
#data.H2RA <- mkdata("H2RA.csv")


## Matching function : 함수실행 X,  함수내부 코드 그대로 실행
matchfun <- function(data, PPI = T){
  wo <- 365
  data <- data.PPI[AKI_Day > wo & ADVCKD_Day > wo & CKD_Day > wo]
  data$AGE_conti <- as.integer(as.character(data$AGE_GROUP))
  data$EXPCON2 <- as.integer(as.character(data$EXPCON))
  form <- paste0("EXPCON2 ~ SEX + AGE_conti + ", paste(grep("Pre_", setdiff(names(data), "Pre_PPI"), value = T), collapse = " + "))
  
  glm1 <- glm(as.formula(form), data = data, family = binomial)
  rr  <- Matching::Match(Tr=data$EXPCON2, X=glm1$fitted, M=1, replace = F, ties = F, version = "fast", caliper = 0.2)
  data$ps <- glm1$fitted
  
  #fwrite(data[c(rr$index.treated, rr$index.control), -c("Pre_PPI", "AGE_conti", "EXPCON2")], "mat_PPI.csv")
  
  #saveRDS(list(data.PPI = data.PPI, data.H2RA = data.H2RA, mat.PPI = data[c(rr$index.treated, rr$index.control), -c("Pre_PPI", "AGE_conti", "EXPCON2")]), "dataCKD.RDS")
  #mat <- matchit(as.formula(form), data = data, ratio = 1)

  #H2RA
  data2 <- data.H2RA
  data2$AGE_conti <- as.integer(as.character(data2$AGE_GROUP))
  data2$EXPCON2 <- 1 - as.integer(as.character(data2$EXPCON))
  form2 <- paste0("EXPCON2 ~ SEX + AGE_conti + ", paste(grep("Pre_", setdiff(names(data), "Pre_H2RA"), value = T), collapse = " + "))
  glm2 <- glm(as.formula(form2), data = data2, family = binomial)
  rr2  <- Matching::Match(Tr=data2$EXPCON2, X=glm2$fitted, M=1, replace = F, ties = F, version = "fast", caliper = 0.2)
  data2$ps <- glm2$fitted
  
  saveRDS(list(data.PPI = data.PPI[, -c("Pre_PPI")], data.H2RA = data.H2RA[, -c("Pre_H2RA")], 
               mat.PPI = data[c(rr$index.treated, rr$index.control), -c("Pre_PPI", "AGE_conti", "EXPCON2")], 
               mat.H2RA = data2[c(rr2$index.treated, rr2$index.control), -c("Pre_H2RA", "AGE_conti", "EXPCON2")]), 
          "dataCKD.RDS")
  
  #fwrite(data2[c(rr2$index.treated, rr2$index.control), -c("Pre_H2RA", "AGE_conti", "EXPCON2")], "mat_H2RA.csv")
}



dinfo <- readRDS("dataCKD.RDS")

data.PPI <- dinfo$data.PPI
mat.PPI <- dinfo$mat.PPI[, -c("ps")]
data.H2RA <- dinfo$data.H2RA
mat.H2RA <- dinfo$mat.H2RA[, -c("ps")]


## Label
label <- mk.lev(data.PPI)
## value with 0/1
vars01 <- names(data.PPI)[sapply(names(data.PPI), function(x){identical(levels(data.PPI[[x]]), c("0", "1"))})]

for (v in vars01){
  label[variable == v, val_label := c("No", "Yes")]
}

label[variable == "SEX", `:=`(var_label = "Sex", val_label = c("Male", "Female"))]
label[variable == "AGE_GROUP", `:=`(var_label = "Age Group",
                                    val_label = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                  "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", ">85"))]


label[variable == "EXPCON", `:=`(var_label = "Group", val_label = c("non-User", "User"))]
label[variable == "AKI2", `:=`(var_label = "AKI with (O7020, O9991)")]
label[variable == "AKI3", `:=`(var_label = "AKI with (O7032, O7052)")]

label <- rbind(label, mk.lev(data.H2RA[, .(Pre_PPI)]))
label[variable == "Pre_PPI", val_label := c("No", "Yes")]


## Make table 1

#list.tb1 <- parallel::mclapply(dinfo, function(x){
#  CreateTableOneJS(vars = setdiff(names(x), c("ps", "EXPCON")), strata = "EXPCON", data = x, Labels = T, labeldata = label, smd = T,
#                   nonnormal = grep("_Day", names(x), value = T), argsExact = list(simulate.p.value = T))$table
#})

#saveRDS(list.tb1, "tb1CKD.RDS")


list.tb1 <- readRDS("tb1CKD.RDS")
