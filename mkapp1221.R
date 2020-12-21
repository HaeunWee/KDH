library(data.table)
library(jstable)
library(lubridate)
library(parallel)

# 이전 osteo 는 깃헙에, 이것은 osteo 말고 나머지 dementia 등등... 업데이트

## file: file name (full path, ex: "/home/whe/period30/ppi_3_ppi_1.csv")
## agecut_greater: subset AGE_GROUP > agecut_greater 
## var.start: select `start day variable` of outcome
## varlist: variable to include
## wo: Washout period
## caliper: option for matching

mkdata <- function(file, agecut_greater = 4, var.start = "start.dementia", varlist = c('SEX', 'AGE_GROUP', 'MI', 'CHF', 'PVD', 'CD', 'DE', 'CPD', 
                                                                                       'RD', 'PUD', 'MLD', 'DWOCC', 'DWCC', 'HOP', 'AMILALEMNOS', 'MOSLD', 'MST', 
                                                                                       'AH','Pre_CKD', 'Pre_HTN', 'Pre_ARD', 'Pre_Statin', 'Pre_Metformin', 'Pre_Aspirin', 'Pre_NSAIDs_COX2', 'Pre_Clopidogrel', 'Pre_Steroid', 'Pre_Estrogen', 'Pre_Antithyroid', 'Pre_Thyroxine',  
                                                                                       'Pre_Depress', 'Pre_Parkinson', 'Pre_Epilepsy', 'EXPCON', 'CCI')){
  #data <- fread("PPIvsH2RA.csv")[AGE_GROUP > 4]
  data <- fread(file)[AGE_GROUP > agecut_greater]
  data[["Event"]] <- as.integer(data[[var.start]] != "")
  data[["Day"]] <- ifelse(data[["Event"]] == 1, ymd(data[[var.start]]) - ymd(data[["indexdate"]]), ymd("2013-12-31") - ymd(data[["indexdate"]]))
  data[, EXPCON := ifelse(EXPCON == 2, 0, 1)]
  
  data[, CCI := CCI + as.integer(AGE_GROUP >= 9) + as.integer(AGE_GROUP >= 11) + as.integer(AGE_GROUP >= 13) + as.integer(AGE_GROUP >= 15)]
  
  var.factor <- c("Event", setdiff(varlist, "CCI"))
  data[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
  return(data[, .SD, .SDcols = c(varlist, "Event", "Day")])
}



## filedir: directory including files
## filenames: name of files
## agecut_greater: subset AGE_GROUP > agecut_greater 
## var.start: select `start day variable` of outcome
## varlist: variable to include
## wo: Washout period
## caliper: option for matching

matchfun <- function(filedir = "/home/whe/period30/1218", filenames = list.files("/home/period30/1218"), agecut_greater = 4, var.start = "start.dementia", 
                     varlist = c('SEX', 'AGE_GROUP', 'MI', 'CHF', 'PVD', 'CD', 'DE', 'CPD', 'RD', 'PUD', 'MLD', 'DWOCC', 'DWCC', 'HOP', 'AMILALEMNOS', 'MOSLD', 'MST', 
                                 'AH','Pre_CKD', 'Pre_HTN', 'Pre_ARD', 'Pre_Statin', 'Pre_Metformin', 'Pre_Aspirin', 'Pre_NSAIDs_COX2', 'Pre_Clopidogrel','Pre_Steroid', 'Pre_Estrogen', 'Pre_Antithyroid', 'Pre_Thyroxine', 'Pre_Depress', 'Pre_Parkinson', 'Pre_Epilepsy', 'EXPCON', 'CCI'),
                     wo = 180, caliper = 0.2){
  
  ## Load data: Use mkdata
  dlist.original <- lapply(file.path(filedir, filenames), mkdata, agecut_greater = agecut_greater, var.start = var.start, varlist = varlist)
  names(dlist.original) <- filenames
  
  ## Save No match/match data, Sample info, label info
  dinfo <- lapply(seq_along(dlist.original), function(i){
    ## washout 
    data <- dlist.original[[i]][Day > wo]
    
    data[, CCI := as.integer(as.character(CCI))]
    
    ## Redefine EXPCON: 0, 1 중 갯수 작은 것을 0으로: 매칭하려면 Case <= Control 이어야 함.
    data[, EXPCON2 := as.integer(as.character(EXPCON))]
    if (data[, diff(table(EXPCON)) > 0]){
      data[, EXPCON2 <- 1 - as.integer(as.character(EXPCON))]
    }
    ## Formula & logistic regression for matching
    vars.indep <- setdiff(varlist, "EXPCON")
    ## Remove variables with only 1 level.
    vars.indep <- vars.indep[sapply(data[, ..vars.indep], function(x) length(setdiff(unique(x), NA))) > 1]
    
    form <- paste0("EXPCON2 ~ ", paste(vars.indep, collapse = " + "))
    modelps <- glm(as.formula(form), data = data, family = binomial)
    
    ## Run matching with `Match1` function
    ## M- 1:M Matching
    rr  <- Matching::Match(Tr=data$EXPCON2, X=modelps$fitted, M=1, replace = F, ties = F, version = "fast", caliper = caliper)
    data$ps <- modelps$fitted # Save propensity score
    
    ## Make label info
    label <- jstable::mk.lev(data)
    vars01 <- names(data)[sapply(names(data), function(x){identical(levels(data[[x]]), c("0", "1"))})]
    
    for (v in vars01){
      label[variable == v, val_label := c("No", "Yes")]
    }
    
    label[variable == "SEX", `:=`(var_label = "Sex", val_label = c("Male", "Female"))]
    label[variable == "AGE_GROUP", `:=`(var_label = "Age Group")]
    
    ref.AGE_GROUP <- c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",  "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", ">85")
    for (v in 0:18){
      label[variable == "AGE_GROUP" & level == v, val_label := ref.AGE_GROUP[v + 1]]
    }
    
    ref.control <- ifelse(grepl("h2ra", filenames[i]), "H2RA", "No PPI")
    label[variable == "EXPCON", `:=`(var_label = "Group", val_label = c(ref.control, "PPI"))]
    
    label[variable == "PVD", `:=`(var_label = "Previous Peripheral vascular disease", val_label = c("No", "Yes"))]
    label[variable == "CD", `:=`(var_label = "Previous Cerebrovascular disease", val_label = c("No", "Yes"))]
    label[variable == "DE", `:=`(var_label = "Previous Dementia", val_label = c("No", "Yes"))]
    label[variable == "CPD", `:=`(var_label = "Previous Chronic pulmonary disease:", val_label = c("No", "Yes"))]
    label[variable == "RD", `:=`(var_label = "Previous Rheumatic disease", val_label = c("No", "Yes"))]
    # renal disease 에서 수정함(12/18)
    label[variable == "PUD", `:=`(var_label = "Previous Peptic ulcer disease", val_label = c("No", "Yes"))]
    label[variable == "MLD", `:=`(var_label = "Previous Mild liver disease", val_label = c("No", "Yes"))]
    label[variable == "DWOCC", `:=`(var_label = "Previous Diabetes without chronic complication", val_label = c("No", "Yes"))]
    label[variable == "DWCC", `:=`(var_label = "Previous Diabetes with chronic complication", val_label = c("No", "Yes"))]
    label[variable == "HOP", `:=`(var_label = "Previous Hemiplegia or paraplegia", val_label = c("No", "Yes"))]
    label[variable == "AMILALEMNOS", `:=`(var_label = "Previous Any malignancy except skin", val_label = c("No", "Yes"))]
    label[variable == "MOSLD", `:=`(var_label = "Previous Moderate or severe liver disease", val_label = c("No", "Yes"))]
    label[variable == "MST", `:=`(var_label = "Previous Metastatic solid tumor", val_label = c("No", "Yes"))]
    label[variable == "AH", `:=`(var_label = "Previous AIDS/HIV")]
    if (nrow(label[variable == "AH"]) == 2){
      label[variable == "AH", val_label := c("No", "Yes")]
    } else{
      label[variable == "AH", val_label := "No"]
    }
    
    
    return(list(original = data[, -c("EXPCON2")], mat = data[c(rr$index.treated, rr$index.control), -c("EXPCON2")], 
                info = data.table(Outcome = gsub("start.", "", var.start), N_Original = nrow(dlist.original[[i]]), N_RemovePrevEvent = nrow(dlist.original[[i]][Day >= 0]), N_RemoveWashout = nrow(data),  N_AfterMatcing = length(rr$mdata$Y)),
                label = label[])
    )
  })
  
  names(dinfo) <- filenames
  return(dinfo)
}



##Run all outcome 

for (oc in c("dementia", "vasculardementia", "cerebralinfarction", "osteoporosis", "ovf", "ohf")){
  
  ## Osteoporosis : 50세 
  agecut <- ifelse(oc %in% c("osteoporosis", "ovf", "ohf"), 10, 4)
  
  ## Run matchfun
  dinfo <- matchfun(filedir = "/home/whe/period30/1218", filenames = list.files("/home/whe/period30/1218"), 
                    agecut_greater = agecut, var.start = paste0("start.", oc),
                    varlist = c('SEX', 'AGE_GROUP', 'MI', 'CHF', 'PVD', 'CD', 'DE', 'CPD', 'RD', 'PUD', 'MLD', 'DWOCC', 'DWCC', 'HOP', 'AMILALEMNOS', 'MOSLD', 'MST', 
                                'AH','Pre_CKD', 'Pre_HTN', 'Pre_ARD', 'Pre_Statin', 'Pre_Metformin', 'Pre_Aspirin', 'Pre_NSAIDs_COX2', 'Pre_Clopidogrel', 'Pre_Steroid', 'Pre_Estrogen', 'Pre_Antithyroid', 'Pre_Thyroxine', 'Pre_Depress', 'Pre_Parkinson', 'Pre_Epilepsy', 'EXPCON', 'CCI'),
                    wo = 180, caliper = 0.2)
  ## 12/18 CKD 추가함. HTN 추가함
  
  ## Make table 1
  list.tb1 <- lapply(dinfo, function(x){
    lapply(c("original", "mat"), function(v){
      CreateTableOneJS(vars = setdiff(names(x[[v]]), c("ps", "EXPCON")), strata = "EXPCON", data = x[[v]], Labels = T, labeldata = x[["label"]], smd = T,
                       nonnormal = "Day", argsExact = list(simulate.p.value = T))$table
    })
    
  })
  
  
  ## Make shiny app folder
  dir.create(paste0("/home/js/ShinyApps/", oc, "_v5"), showWarnings = F)
  
  ## Copy app.R 
  file.copy("/home/whe/period30/1218/app.R", paste0("/home/js/ShinyApps/", oc, "_v5/", "app.R"), overwrite  = T)
  
  saveRDS(dinfo, paste0("/home/js/ShinyApps/", oc, "_v5/", "dinfo.RDS"))
  saveRDS(list.tb1, paste0("/home/js/ShinyApps/", oc, "_v5/", "tb1.RDS"))
  print(paste0(oc, " end"))
}

#1027 v4
#1218 v5

for (oc in c("dementia", "vasculardementia", "cerebralinfarction", "osteoporosis", "ovf", "ohf")){
  file.copy("/home/whe/period30/1218/app.R", paste0("/home/js/ShinyApps/", oc, "_v5/", "app.R"), overwrite  = T)
}
