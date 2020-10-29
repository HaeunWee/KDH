library(fst); library(data.table); library(stringr)
library(lubridate); library(magrittr); library(parallel)
setwd("/home/whe")

#------------약물 검색 함수용-----------
druginfo <- fread("건강보험심사평가원_의약품 주성분 정보 2019.12.csv")


#----------- 약물 이름으로 검색 함수------
drugf <- function(x){
  return(druginfo[grep(x,druginfo$주성분명칭, ignore.case = TRUE)][`투여`=="내복"][["코드"]])
}

dementia.drug <- c('donepezil', 'rivastigmin', 'galantamin', 'emantin')
cerebralinfarction.drug <- c('aspirin', 'clopidogrel', 'triflusal', 
                             'cilostazol', 'ozagrel sodium', 'sarpogrelate', 
                             'warfarin', 'rivaroxaban', 'apixaban', 'edoxaban', 
                             'dabigatran etexilate')
osteoporosis.drug <- c('alendron', 'etidron','pamidron', 'risedron', 'ibandron','zoledron', 
                       'raloxifen', 'bazedoxifen', 'catonin', 'denosumab', 'teriparat')



#------------ 현재 용도 없음-----------
t14.pre <- list(
  Smoking = c("J41", "J42", "J43", "Z72", "F17"),
  Alcohol = c("F10", "K70", "K85", "K86", "T51", "X45", "X65", "Y15", "Y91"),
  CKD = c("N18", "N19"),              ## 만성콩팥질환(Chronic kidney disease)
  HTN = paste0("I", 10:15),           ## 고혈압            
  DM = paste0("E", 10:11),            ## 당뇨
  Dyslipidemia = "E78",               ## 이상지질혈증 
  Obesity = paste0("E", 65:68),       ## 비만
  IHD = paste0("I", 20:25),           ## 허혈성심장질환(Ischemic heart disease)
  Afib = "I48",                       ## 심방세동
  CHF = "I50",                        ## 심부전(Congestive heart failure)
  Stroke = paste0("I", 60:69),        ## 뇌졸중
  Cirrhosis = "K74",                  ## 간경화
  GERD = "K21",                       ## 위식도역류질환
  G_ulcer= "K25",                     ## 위궤양
  D_ulcer = "K27",                    ## 십이지장궤양
  Depress = c("F32", "F33"),          ## 우울증
  Parkinson = c("G20", "G21", "G22"), ## 파킨슨병
  Epilepsy = c("G40", "G41")          ## 간질
)
t16.pre <- list(
  Statin = druginfo[grep("statin", druginfo[[3]])][`투여` == "내복"][["코드"]],
  Metformin = druginfo[grep("metformin", druginfo[[3]])][`투여` == "내복"][["코드"]],
  Aspirin = druginfo[grep("aspirin", druginfo[[3]])][`투여` == "내복"][["코드"]],
  NSAIDs_COX2 = c("347701ACH", "347701ATB", "347702ACH", "347702ATB", "347703ACH", "636401ACH", "636901ATB",
                  druginfo[grep("dexibuprofen|diclofenac|etodolac|ibuprofen|ketorolac|loxoprofen|mefenamic|nabumetone|nimesulide|sulindac|talniflumate|tiaprofenic|zaltoprofen|naproxen|meloxicam|piroxicam|lornoxicam", druginfo[[3]])][`투여` == "내복"][["코드"]]
  ),
  Clopidogrel = druginfo[grep("clopidogrel", druginfo[[3]])][`투여` == "내복"][["코드"]],
  H2RA = c("133301ATB", "133302ATB", "133303ATB", "133305ATR", "157301ATB", "157302ATB", "157302ATD", "222801ATB", "222803ATB", "222805ATB", 
           "202701ACH", "202701ATB", "202704ATB", "489302ATB", "225201ACR", "225202ACR", "271800ATB", "631800ATB"),
  PPI = c("367201ACH", "367201ATB", "367201ATD", "367202ACH", "367202ATB", "367202ATD", "498001ACH", "498002ACH", "509901ACH", "509902ACH", 
          "670700ATB", "204401ACE", "204401ATE", "204402ATE", "204403ATE", "664500ATB", "640200ATB", "664500ATB", 
          "208801ATE", "208802ATE", "656701ATE", "519201ATE", "519202ATE", "656701ATE", "519203ATE", "222201ATE", "222202ATE", "222203ATE", 
          "181301ACE", "181301ATD", "181302ACE", "181302ATD", "181302ATE", "621901ACR", "621902ACR", "505501ATE"),
  Steroid = unique(c(drugf('prednisolone'), drugf('hydrocortisone'), drugf('triamcinolone'), drugf('dexamethasone'), drugf('betamethasone'), drugf("deflazacort"), drugf("prednisone"))),
  Estrogen = drugf('estrogen'),
  Antithyroid = drugf('methimazole'),
  Thyroxine = drugf('levothyroxine')
)
code.ppi <-  c("367201ACH", "367201ATB", "367201ATD", "367202ACH", "367202ATB", 
               "367202ATD", "498001ACH", "498002ACH", "509901ACH", "509902ACH", 
               "670700ATB", "204401ACE", "204401ATE", "204402ATE", "204403ATE", 
               "664500ATB", "640200ATB", "664500ATB", "208801ATE", "208802ATE", 
               "656701ATE", "519201ATE", "519202ATE", "656701ATE", "519203ATE", 
               "222201ATE", "222202ATE", "222203ATE", "181301ACE", "181301ATD", 
               "181302ACE", "181302ATD", "181302ATE", "621901ACR", "621902ACR", 
               "505501ATE")
code.h2ra <- c("133301ATB", "133302ATB", "133303ATB", "133305ATR", "157301ATB", 
               "157302ATB", "157302ATD", "222801ATB", "222803ATB", "222805ATB", 
               "202701ACH", "202701ATB", "202704ATB", "489302ATB", "225201ACR", 
               "225202ACR", "271800ATB", "631800ATB")

#----------- CCI 점수 계산용 ---------------

nm <- function(x, y=0, z=9){
  x<-paste0(x,y:z)
  return(x)
}

nmm <- function(x, y=100, z=199){
  x <- paste0(x,str_sub(y:z,2,-1))
  return(x) 
}


t14.cci <- list(
  MI = c(nm("I12"),nm("I22"),"I252"),                                            # 점
  CHF = c("I099", "I110", "I130", "I132", "I255", "I420",                        # 점
          nm("I42",5,9), nm("I43"), nm("I50"), "P290"),
  PVD = c(nm("I70"), nm("I71"), "I731", "I738", "I739",                           # 점
          "I771", "I790", "I792", "K551", "K558", 
          "K559", "Z958", "Z959"),
  CD = c(nm("G45"), nm("G46"), "H340", nm("I",600,699)),                         # 점
  DE = c(nmm("F",1000,1039), "F051", nm("G30"), "G31.1"),                        # 점
  CPD = c("I278", "I279", nmm("J",1400,1479),                                    # 점
          nmm("J",1600,1679), "J684", "J701", "J703"),
  RD = c(nm("M05"), nm("M06"), "M315", nmm("M",1320,1349), "M351",               # 점 
         "M353", "M360"),
  PUD = c(nmm("K",1250,1289)),                                                   # 점
  MLD = c(nm("B18"), nmm("K",1700,1703) , "K709", "K713", "K714",                # 점
          "K715", "K717", nmm("K",1730,1749), "K760", "K762",
          "K763", "K764", "K768", "K769", "Z944"),
  DWOCC = c("E100", "E101", "E106", "E108",                                      # 점
            "E109", "E110", "E111", "E116", 
            "E118", "E119", "E120", "E121", 
            "E126", "E128", "E129", "E130", 
            "E131", "E136", "E138", "E139", 
            "E140", "E141", "E146", "E148", 
            "E149"),
  DWCC = c(nmm("E",1102,1105), "E107",                                           # 2점
           nmm("E",1112,1115), "E117", 
           nmm("E",1122,1125), "E127", 
           nmm("E",1132,1135), "E137", 
           nmm("E",1142,1145), "E147"),
  HOP = c("G041", "G114", "G801", "G802", nm("G81"),                             # 2점 
          nm("G82"), nmm("G",1830,1834), "G839"),
  AMILALEMNOS =                                                                  # 2점
    c(nmm("C",1000,1269), nmm("C",1300,1349), nmm("C",1370,1419), nm("C43"),
      nmm("C",1450,1589), nmm("C",1600,1769), nmm("C",1810,1859), nm("C88"),
      nmm("C", 1900,1979)),
  MOSLD = c("I850", "I859", "I864", "I982", "K704",                              # 3점
            "K711", "K721", "K729", "K765", "K766", 
            "K767"),
  MST = c(nmm("C",1770,1809)),                                                   # 6점
  AH = c(nmm("B",1200,1229), nm("B24"))                                          # 6점
  
)

cciscore <- list(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 6, 6)

# 여기까지 cci


# PPI 분석에 필요한거 + 과거력 분석에 필요한것 불러오기



#--------진단명 리스트------

# 진단명, 약물리스트, 행위리스트에 각각 새로 추가해야함.
# 이름규칙 = dia.___*, ___.d.l, ___.p.l 에서 ___에 들어가는 3글자 일치.
# 이후에 함수 돌릴 때 변수 벡터에 '___*' 이름 추가하면 자동계산.

dia.dementia <- c("F000", "F001", "F002")
dia.vasculardementia <- c("F01")
dia.cerebralinfarction <- c("I63")
dia.osteoporosis <- c("M810", "M815", "M818", "M819", "M800", "M805", "M808", "M809")
#dia.ov1 <- c("M800", "M805", "M808", "M809")
#dia.ov2 <- c("M810", "M815", "M818", "M819", "S220", "S221", "S320", "M484", "M485")
dia.ovf <- c("M800", "M805", "M808", "M809", "M810", "M815", "M818", "M819", "S220", "S221", "S320", "M484", "M485")
dia.ohf <- c("S720", "S721")
dia.aki <- c("N17", "N170", "N171", "N172", "N178", "N179")

#--------약물검색 실행--------

code.donepezil <- drugf(dementia.drug[1])
code.rivastigmine <- drugf(dementia.drug[2])
code.galatamine <- drugf(dementia.drug[3])
code.memantine <- drugf(dementia.drug[4])


code.aspirin <- drugf(cerebralinfarction.drug[1])
code.clopidogrel <- drugf(cerebralinfarction.drug[2])
code.triflusal <- drugf(cerebralinfarction.drug[3])
code.cliostazol <- drugf(cerebralinfarction.drug[4])
# 아래는 B01AC 기준
code.linecin <- c(drugf(cerebralinfarction.drug[5]),
                  drugf(cerebralinfarction.drug[6]))
code.warfarin <- drugf(cerebralinfarction.drug[7])
code.rivaroxaban <- drugf(cerebralinfarction.drug[8])
code.apixaban <- drugf(cerebralinfarction.drug[9])
code.edoxaban <- drugf(cerebralinfarction.drug[10])
code.pradaxa <- drugf(cerebralinfarction.drug[11])

code.bisphosphonates <- c(drugf(osteoporosis.drug[1]),
                          drugf(osteoporosis.drug[2]),
                          drugf(osteoporosis.drug[3]),
                          drugf(osteoporosis.drug[4]),
                          drugf(osteoporosis.drug[5]),
                          drugf(osteoporosis.drug[6]))
code.serm <- c(drugf(osteoporosis.drug[7]),drugf(osteoporosis.drug[8]))
code.calcitonin <- drugf(osteoporosis.drug[9])
code.denosumab <- drugf(osteoporosis.drug[10])
code.teriparatide <- drugf(osteoporosis.drug[11])

#-------- drug list----

vas.d.l = NULL
dem.d.l <- list(donepezil = code.donepezil, rivastigmine = code.rivastigmine, 
                galatamine = code.galatamine, memantine = code.memantine)
cer.d.l <- list(aspirin = code.aspirin, clopidogrel = code.clopidogrel, 
                triflusal = code.triflusal, cliostazol = code.cliostazol, 
                linecin = code.linecin, warfarin = code.warfarin, 
                rivaroxaban = code.rivaroxaban, apixaban = code.apixaban,
                edoxaban = code.edoxaban, pradaxa = code.pradaxa)
ost.d.l <- list(bisphosphonates = code.bisphosphonates,
                serm = code.serm, calcitonin = code.calcitonin,
                denosumab = code.denosumab, teriparatide = code.teriparatide)
#ov1.d.l <- NULL
#ov2.d.l <- NULL
ovf.d.l <- NULL
ohf.d.l <- NULL
aki.d.l <- NULL

#----- procedures list ----

vas.p.l <- NULL
dem.p.l <- NULL
cer.p.l <- NULL
ost.p.l <- NULL
#ov1.p.l <- c("N0471", "N0472", "N0473", "N0474", "N0630")
#ov2.p.l <- c("N0471", "N0472", "N0473", "N0474", "N0630")
ovf.p.l <- c("N0471", "N0472", "N0473", "N0474", "N0630")
ohf.p.l <- c("N0601", "N0911", "N0473", "N0981", "N0641", "N0652", "N0654", "N0715")
aki.p.l <- NULL

# ------- 큰파일 정리 -------= #

jk <- read_fst("jk.fst", as.data.table = T); setkey(jk, PERSON_ID)
t12 <- read_fst("t1_120.fst", as.data.table = T); setkey(t12, KEY_SEQ)
t13 <- read_fst("t1_130.fst", as.data.table = T); setkey(t13, KEY_SEQ)
t14 <- read_fst("t1_140.fst", as.data.table = T); setkey(t14, KEY_SEQ)
t16 <- read_fst("t1_160.fst", as.data.table = T); setkey(t16, KEY_SEQ)
# 큰놈들 불러오기



t12 <- t12[, .(PERSON_ID, KEY_SEQ, RECU_FR_DT)][order(PERSON_ID, RECU_FR_DT, KEY_SEQ)]
jk <- jk[order(PERSON_ID, STND_Y)]
t12[, RECU_FR_DT := ymd(RECU_FR_DT)]
t12.firstcome <- t12[, .SD[1], by="PERSON_ID"][, .(PERSON_ID, start.come = RECU_FR_DT)]
# 큰놈들 정리하기

t14.sym <- t14[str_sub(SICK_SYM, 1, 3) %in% unique(unlist(t14.pre))]
t14.sym <-
  merge(t12[, .(PERSON_ID, KEY_SEQ)][KEY_SEQ %in% t14.sym$KEY_SEQ],t14.sym,by="KEY_SEQ")
t16.sym <- t16[GNL_NM_CD %in% unique(unlist(t16.pre))]
t16.sym <-
  merge(t12[, .(PERSON_ID, KEY_SEQ)][KEY_SEQ %in% t16.sym$KEY_SEQ],t16.sym,by="KEY_SEQ")
t14.sym[, RECU_FR_DT := ymd(RECU_FR_DT)]
t16.sym[, RECU_FR_DT := ymd(RECU_FR_DT)]
t14.mkcci <- merge(t12[, .(PERSON_ID, KEY_SEQ)],
                   t14[SICK_SYM %in% unique(unlist(t14.cci))],
                   by="KEY_SEQ")[, RECU_FR_DT := ymd(RECU_FR_DT)]
no.ppi <- t12[KEY_SEQ %in% t16[GNL_NM_CD %in% code.ppi]$KEY_SEQ]$PERSON_ID
no.ppi <- setdiff(t12$PERSON_ID, no.ppi)


#--------------- 만들었다가 버린 함수 --------------

# Who is sick
WIS <- function(dia, d.l = NULL, p.l = NULL, nms){
  
  sym <- merge(t12[KEY_SEQ %in% t14[SICK_SYM %in% dia]$KEY_SEQ][, .(PERSON_ID, KEY_SEQ)],
               t14[SICK_SYM %in% dia][, .(KEY_SEQ, start.sym = RECU_FR_DT)], 
               by = 'KEY_SEQ')
  drug <- merge(t12[KEY_SEQ %in% t16[GNL_NM_CD %in% unique(unlist(d.l))]$KEY_SEQ][, .(PERSON_ID, KEY_SEQ)], 
                t16[GNL_NM_CD %in% unique(unlist(d.l))][, .(KEY_SEQ, start.drug = RECU_FR_DT)], 
                by = "KEY_SEQ")
  procedure <- merge(t12[KEY_SEQ %in% t13[DIV_CD %in% unique(unlist(p.l))]$KEY_SEQ][, .(PERSON_ID, KEY_SEQ)],
                     t13[DIV_CD %in% unique(unlist(p.l))][, .(KEY_SEQ, start.procedure = RECU_FR_DT)],
                     by = "KEY_SEQ")
  
  drug[, start.drug := ymd(start.drug)]
  drug <- drug[order(PERSON_ID, start.drug)][, .SD[1], key = PERSON_ID]
  sym[, start.sym := ymd(start.sym)]
  sym <- sym[order(PERSON_ID, start.sym)][, .SD[1], key = PERSON_ID]
  procedure[, start.procedure := ymd(start.procedure)]
  procedure <- procedure[order(PERSON_ID, start.procedure)][, .SD[1], key = PERSON_ID]
  
  if(is.null(d.l)){
    sick <- sym[, .(PERSON_ID, start.sick = start.sym)]
    
    
  }else{
    sick <- merge(sym, drug, by = "PERSON_ID")
    sicka <- sick[start.sym < start.drug][, start.sick := start.drug]
    sickb <- sick[start.sym >= start.drug][, start.sick := start.sym]
    sick <- rbind(sicka, sickb)[, .(PERSON_ID, start.sick)][order(PERSON_ID)]
    
  }
  
  if(is.null(p.l)){
    
  }else{
    sick <-merge(sick, procedure, by="PERSON_ID")
    sicka <- sick[start.sick < start.procedure][, start.sick := start.procedure]
    sickb <- sick[start.sick >= start.procedure][, start.sick := start.sick]
    sick <- rbind(sicka,sickb)[, .(PERSON_ID, start.sick)][order(PERSON_ID)]
  }
  
  
  colnames(sick) = c('PERSON_ID', paste0('start.',nms))
  return(sick)
  # 병걸린 사람들 골라내기
}

# ----------- 사용할 함수 ----------
WIS <- function(dia, d.l = NULL, p.l = NULL, nms){
  
  d.l <- unique(unlist(d.l))
  p.l <- unique(unlist(p.l))

  if(is.null(d.l)){
    if(is.null(p.l)){
      sick <- t12[KEY_SEQ %in% t14[SICK_SYM %in% dia]$KEY_SEQ][order(PERSON_ID, RECU_FR_DT)][, .SD[1], keyby='PERSON_ID'][, .(PERSON_ID, RECU_FR_DT)]
    }
    else{
      sick <- t12[KEY_SEQ %in% intersect(t14[SICK_SYM %in% dia]$KEY_SEQ, t13[DIV_CD %in% p.l]$KEY_SEQ)][order(PERSON_ID, RECU_FR_DT)][, .SD[1], keyby='PERSON_ID'][, .(PERSON_ID, RECU_FR_DT)]
    }
    
    
  }else{
    if(is.null(p.l)){
      sick <- t12[KEY_SEQ %in% intersect(t14[SICK_SYM %in% dia]$KEY_SEQ, t16[GNL_NM_CD %in% d.l]$KEY_SEQ)][order(PERSON_ID, RECU_FR_DT)][, .SD[1], keyby='PERSON_ID'][, .(PERSON_ID, RECU_FR_DT)]
    }else{
      sick <- t12[KEY_SEQ %in% intersect(t14[SICK_SYM %in% dia]$KEY_SEQ, t13[DIV_CD %in% p.l]$KEY_SEQ, t16[GNL_NM_CD %in% d.l]$KEY_SEQ)][order(PERSON_ID, RECU_FR_DT)][, .SD[1], keyby='PERSON_ID'][, .(PERSON_ID, RECU_FR_DT)]
    }

    
  }
    colnames(sick) = c('PERSON_ID', paste0('start.',nms))
  return(sick)
  # 병걸린 사람들 골라내기
}


HTA <- function(x, y, z, tot){
  # x = 1 (PPI vs 나머지) 2 (H2RA vs 나머지)
  # y = 과거력을 index date y일 전부터 index date까지 검색함
  #     ppi(1) or h2ra(2) 첫 복용일 포함 복용전 y일을 확보하지 못할 경우 삭제
  # z = ppi(1) or h2ra(2) 첫 복용일 포함 z일 이전 질병 발병자들만 질병 발병자들로봄 (wash-out period)
  # w = 자료 모두 들어있는 것
  TOT.temp <- tot
  TOT.temp[, EXPCON := 3]
  TOT.temp[, indexdate := ymd(20000303)]
  # EXPCON = 1 (실험군) 2 (대조군) 3(버리는군)
  if(x==1){
    TOT.temp[!is.na(start.ppi)][start.ppi >= start.come + y]$EXPCON <- 1   
    TOT.temp[is.na(start.ppi)]$EXPCON <- 2
    TOT.temp <- TOT.temp[EXPCON == 1 | EXPCON ==2]
    TOT.temp[EXPCON == 1]$indexdate <- TOT.temp[EXPCON == 1]$start.ppi
    # 실험군은 ppi를 처음으로 처방받은 날짜가 indexdate
    TOT.temp[EXPCON == 2]$indexdate <- TOT.temp[EXPCON ==2 ]$start.come + y
    # 대조군은 방문일 포함 y일 이후가 indexdate
    
    
  }else{
    TOT.temp[!is.na(start.h2ra)][start.h2ra > start.come + y]$EXPCON <- 1
    TOT.temp[is.na(start.h2ra)]$EXPCON <- 2
    TOT.temp <- TOT.temp[EXPCON == 1 | EXPCON ==2]
    TOT.temp[EXPCON == 1]$indexdate <- TOT.temp[EXPCON == 1]$start.h2ra
    TOT.temp[EXPCON == 2]$indexdate <- TOT.temp[EXPCON ==2 ]$start.come + y
    
  }
  
  ## 위암연구의 과거력과 모양 동일
  vars.t14pre <- lapply(1:length(t14.pre), function(i){
    person.ev <- merge(TOT.temp[, .(PERSON_ID, indexdate)], t14.sym[str_sub(SICK_SYM, 1, 3) %in% t14.pre[[i]]], by = "PERSON_ID")[order(PERSON_ID, RECU_FR_DT)][, .SD[1], keyby = "PERSON_ID"][RECU_FR_DT <= indexdate][, .(PERSON_ID, var = 1)]
    vv <- merge(TOT.temp[, .(PERSON_ID)], person.ev[!is.na(PERSON_ID)], by = "PERSON_ID", all = T)[, var := ifelse(is.na(var), 0, 1)]$var
    return(vv)
  }) %>% Reduce(cbind, .)
  colnames(vars.t14pre) <- paste0("Pre_", names(t14.pre))
  
  vars.t16pre <- lapply(1:length(t16.pre), function(i){
    person.ev <- merge(TOT.temp[, .(PERSON_ID, indexdate)], t16.sym[GNL_NM_CD %in% t16.pre[[i]]], by = "PERSON_ID")[order(PERSON_ID, RECU_FR_DT)][, .SD[1], keyby = "PERSON_ID"][ymd(RECU_FR_DT) <= ymd(indexdate)][, .(PERSON_ID, var = 1)]
    vv <- merge(TOT.temp[, .(PERSON_ID)], person.ev[!is.na(PERSON_ID)], by = "PERSON_ID", all = T)[, var := ifelse(is.na(var), 0, 1)]$var
    return(vv)
  }) %>% Reduce(cbind, .)
  colnames(vars.t16pre) <- paste0("Pre_", names(t16.pre))
  
  vars.t14cci <- lapply(1:length(t14.cci), function(i){
    person.ev <- merge(TOT.temp[, .(PERSON_ID, indexdate)], t14.mkcci[SICK_SYM %in% t14.cci[[i]]], by = "PERSON_ID")[order(PERSON_ID, RECU_FR_DT)][, .SD[1], keyby = "PERSON_ID"][RECU_FR_DT <= indexdate][, .(PERSON_ID, var = cciscore[[i]])]
    vv <- merge(TOT.temp[, .(PERSON_ID)], person.ev[!is.na(PERSON_ID)], by = "PERSON_ID", all = T)[, var := ifelse(is.na(var), 0, var)]$var
    return(vv)
  }) %>% Reduce(cbind, .)
  colnames(vars.t14cci) <- names(t14.cci)
  
  TOT.temp <- cbind(TOT.temp, vars.t14pre)
  TOT.temp <- cbind(TOT.temp, vars.t16pre)
  TOT.temp <- cbind(TOT.temp, vars.t14cci)
  TOT.temp[, STND_Y := as.integer(str_sub(indexdate, 1, 4))]
  TOT.temp <- merge(TOT.temp,jk[PERSON_ID %in% TOT.temp$PERSON_ID],by=c("PERSON_ID","STND_Y"))
  TOT.temp[, CCI := MI + CHF + PVD + CD + DE + CPD + RD + PUD + MLD + DWOCC + DWCC + HOP + AMILALEMNOS + MOSLD + MST + AH]
  
  return(TOT.temp)
}




# 질병 - 약물 and 로 묶고 진단일과 처방일 중 느린것을 질병 시작일로 택
# 질병 코드는 dia 에 약물코드는 d.l에 이름은 nms에
# ex ) dementia 의 경우 dia.dementia, dem.d.l, "dementia" 로 저장

# --------- Window Period를 위한 정리 -----

t16ppi<-t16[GNL_NM_CD %in% code.ppi]
t16h2ra<-t16[GNL_NM_CD %in% code.h2ra]

t16ppi<-merge(t12[, .(KEY_SEQ, PERSON_ID)][KEY_SEQ %in% t16ppi$KEY_SEQ], t16ppi, by = "KEY_SEQ")
t16h2ra<-
  merge(t12[, .(KEY_SEQ, PERSON_ID)][KEY_SEQ %in% t16h2ra$KEY_SEQ], t16h2ra, by = "KEY_SEQ")

#ppi 정리
t16ppi[, RECU_FR_DT := ymd(RECU_FR_DT)]
t16ppi <- t16ppi[order(PERSON_ID)]
t16ppi[, periodstart.ppi := RECU_FR_DT]
t16ppi <- t16ppi[order(PERSON_ID, RECU_FR_DT,MDCN_EXEC_FREQ)]

#H2RA 정리
t16h2ra[, RECU_FR_DT := ymd(RECU_FR_DT)]
t16h2ra <- t16h2ra[order(PERSON_ID)]
t16h2ra[, periodstart.h2ra := RECU_FR_DT]
t16h2ra <- t16h2ra[order(PERSON_ID, RECU_FR_DT,MDCN_EXEC_FREQ)]

# ---------Window Period 함수------------

PPIWP <- function(x, y){
  #x = Window period
  #y = 0, 30, 60, 90, 120
  
  t16ppif <- t16ppi
  t16ppif <- t16ppif[order(PERSON_ID, RECU_FR_DT, MDCN_EXEC_FREQ, KEY_SEQ, SEQ_NO)]
  
  t16ppif[, periodfinish.ppi := RECU_FR_DT + MDCN_EXEC_FREQ]
  t16ppif$periodfinish.ppi <- c(as_date(1),t16ppif$periodfinish.ppi[-nrow(t16ppif)])
  t16ppif$periodstart.ppi <- c(as_date(1),t16ppif$periodstart.ppi[-nrow(t16ppif)])
  t16ppif[, jj := PERSON_ID]
  t16ppif$jj <- c(1,t16ppif$PERSON_ID[-nrow(t16ppif)])
  
  t16ppif[, oo := 1]
  t16ppif[, do := RECU_FR_DT -periodfinish.ppi]
  t16ppif[do < 0]$do <- 0
  t16ppif[jj==PERSON_ID][periodfinish.ppi + x >= RECU_FR_DT]$oo <- 0
  t16ppif <- rbind(t16ppif,t16ppif)
  t16ppif <- t16ppif[order(PERSON_ID, RECU_FR_DT, MDCN_EXEC_FREQ, KEY_SEQ, SEQ_NO)]
  
  t16ppif$jjj <- c(t16ppif$oo[-1],1)
  t16ppif[, oo := oo * jjj]
  
  t16ppif <- cbind(t16ppif, num = c(1:nrow(t16ppif)))
  t16ppif[, ooo := oo * num]
  t16ppifo <- t16ppif[ooo == 0]
  t16ppifo <- cbind(t16ppifo, numm = c(nrow(t16ppifo):1))
  t16ppifo[, ooo := num + numm + nrow(t16ppif)]
  t16ppif[ooo == 0]$ooo <- t16ppifo$ooo
  
  t16ppif.day <- t16ppif[order(oo)][, .SD[1], by=c("PERSON_ID", "RECU_FR_DT", "MDCN_EXEC_FREQ", "KEY_SEQ", "SEQ_NO", "ooo")]
  t16ppif.day[oo == 1]$do <- 0
  t16ppif.day <- t16ppif.day[order(num)]
  t16ppif.day[, dsum.ppi := sum(do) - do[1], keyby=c("PERSON_ID", "ooo")]
  
  t16ppif <- t16ppif[order(RECU_FR_DT)]
  t16ppif.fin <- t16ppif[, .SD[.N], by = c('PERSON_ID','ooo')]
  t16ppif <- t16ppif[order(RECU_FR_DT + MDCN_EXEC_FREQ)]
  t16ppif.ini <- t16ppif[, .SD[1], by = c('PERSON_ID','ooo')]
  t16ppifpr <- merge(t16ppif.ini[, .(PERSON_ID, ooo, periodstart.ppi = RECU_FR_DT)], t16ppif.fin[, .(PERSON_ID, ooo, periodfinish.ppi = RECU_FR_DT + MDCN_EXEC_FREQ)], keyby=c("PERSON_ID, ooo"))
  t16ppifpr <- merge(t16ppifpr, t16ppif.day[, .(PERSON_ID, dsum.ppi, ooo)], keyby=c("PERSON_ID, ooo"))
  
  
  t16ppifpr <- t16ppifpr[, .(PERSON_ID, nperiod.ppi = periodfinish.ppi - periodstart.ppi - dsum.ppi, periodstart.ppi)]
  t16ppifpr <- t16ppifpr[order(PERSON_ID, nperiod.ppi)]
  t16ppifpr <- t16ppifpr[nperiod.ppi >= y]
  t16ppifpr <- t16ppifpr[order(PERSON_ID, periodstart.ppi)]
  
  if(y==0){
    t16ppifpr <- t16ppifpr[order(PERSON_ID, nperiod.ppi, -periodstart.ppi)]
    t16ppifpr <- t16ppifpr[, .SD[.N], by="PERSON_ID"]
  } else {
    t16ppifpr <- t16ppifpr[, .SD[1], by="PERSON_ID"]
  }
  
  return(t16ppifpr)
}

H2RAWP <- function(x, y){
  #x = Window period
  #y = 30, 60, 90, 120
  
  t16h2raf <- t16h2ra
  t16h2raf <- t16h2raf[order(PERSON_ID, RECU_FR_DT, MDCN_EXEC_FREQ, KEY_SEQ, SEQ_NO)]
  
  t16h2raf[, periodfinish.h2ra := RECU_FR_DT + MDCN_EXEC_FREQ]
  t16h2raf$periodfinish.h2ra <- c(as_date(1),t16h2raf$periodfinish.h2ra[-nrow(t16h2raf)])
  t16h2raf$periodstart.h2ra <- c(as_date(1),t16h2raf$periodstart.h2ra[-nrow(t16h2raf)])
  t16h2raf[, jj := PERSON_ID]
  t16h2raf$jj <- c(1,t16h2raf$PERSON_ID[-nrow(t16h2raf)])
  
  t16h2raf[, oo := 1]
  t16h2raf[, do := RECU_FR_DT -periodfinish.h2ra]
  t16h2raf[do < 0]$do <- 0
  t16h2raf[jj==PERSON_ID][periodfinish.h2ra + x >= RECU_FR_DT]$oo <- 0
  t16h2raf <- rbind(t16h2raf,t16h2raf)
  t16h2raf <- t16h2raf[order(PERSON_ID, RECU_FR_DT, MDCN_EXEC_FREQ, KEY_SEQ, SEQ_NO)]
  
  t16h2raf$jjj <- c(t16h2raf$oo[-1],1)
  t16h2raf[, oo := oo * jjj]
  
  t16h2raf <- cbind(t16h2raf, num = c(1:nrow(t16h2raf)))
  t16h2raf[, ooo := oo * num]
  t16h2rafo <- t16h2raf[ooo == 0]
  t16h2rafo <- cbind(t16h2rafo, numm = c(nrow(t16h2rafo):1))
  t16h2rafo[, ooo := num + numm + nrow(t16h2raf)]
  t16h2raf[ooo == 0]$ooo <- t16h2rafo$ooo
  
  t16h2raf.day <- t16h2raf[order(oo)][, .SD[1], by=c("PERSON_ID", "RECU_FR_DT", "MDCN_EXEC_FREQ", "KEY_SEQ", "SEQ_NO", "ooo")]
  t16h2raf.day[oo == 1]$do <- 0
  t16h2raf.day <- t16h2raf.day[order(num)]
  t16h2raf.day[, dsum.h2ra := sum(do) - do[1], keyby=c("PERSON_ID", "ooo")]
  
  t16h2raf <- t16h2raf[order(RECU_FR_DT)]
  t16h2raf.fin <- t16h2raf[, .SD[.N], by = c('PERSON_ID','ooo')]
  t16h2raf <- t16h2raf[order(RECU_FR_DT + MDCN_EXEC_FREQ)]
  t16h2raf.ini <- t16h2raf[, .SD[1], by = c('PERSON_ID','ooo')]
  t16h2rafpr <- merge(t16h2raf.ini[, .(PERSON_ID, ooo, periodstart.h2ra = RECU_FR_DT)], t16h2raf.fin[, .(PERSON_ID, ooo, periodfinish.h2ra = RECU_FR_DT + MDCN_EXEC_FREQ)], keyby=c("PERSON_ID, ooo"))
  t16h2rafpr <- merge(t16h2rafpr, t16h2raf.day[, .(PERSON_ID, dsum.h2ra, ooo)], keyby=c("PERSON_ID, ooo"))
  
  
  t16h2rafpr <- t16h2rafpr[, .(PERSON_ID, nperiod.h2ra = periodfinish.h2ra - periodstart.h2ra - dsum.h2ra, periodstart.h2ra)]
  t16h2rafpr <- t16h2rafpr[order(PERSON_ID, nperiod.h2ra)]
  t16h2rafpr <- t16h2rafpr[nperiod.h2ra >= y]
  t16h2rafpr <- t16h2rafpr[order(PERSON_ID, periodstart.h2ra)]
  
  if(y==0){
    t16h2rafpr <- t16h2rafpr[order(PERSON_ID, nperiod.h2ra, -periodstart.h2ra)]
    t16h2rafpr <- t16h2rafpr[, .SD[.N], by="PERSON_ID"]
  } else {
    t16h2rafpr <- t16h2rafpr[, .SD[1], by="PERSON_ID"]
  }
  
  return(t16h2rafpr)
}


# ------ 골다공증만 따로 건드려주기 #* 필요없음. 삭제. -----


#out.dvc[, year.ost := substr(start.osteoporosis, 1, 4)]
#out.dvc[, year.ov1 := substr(start.ov1, 1, 4)]
#out.dvc[, year.ov2 := substr(start.ov2, 1, 4)]
#out.dvc[, year.ohf := substr(start.ohf, 1, 4)]

#out.dvc[, STND_Y := as.integer(year.ost)]
#out.dvc[PERSON_ID %in% merge(out.dvc, jk[, .(STND_Y, PERSON_ID, AGE_GROUP)][AGE_GROUP < 11], by=c('STND_Y', 'PERSON_ID'))$PERSON_ID]$start.osteoporosis <- NA
#out.dvc[, STND_Y := as.integer(year.ov1)]
#out.dvc[PERSON_ID %in% merge(out.dvc, jk[, .(STND_Y, PERSON_ID, AGE_GROUP)][AGE_GROUP < 11], by=c('STND_Y', 'PERSON_ID'))$PERSON_ID]$start.ov1 <- NA
#out.dvc[, STND_Y := as.integer(year.ov2)]
#out.dvc[PERSON_ID %in% merge(out.dvc, jk[, .(STND_Y, PERSON_ID, AGE_GROUP)][AGE_GROUP < 11], by=c('STND_Y', 'PERSON_ID'))$PERSON_ID]$start.ov2 <- NA
#out.dvc[, STND_Y := as.integer(year.ohf)]
#out.dvc[PERSON_ID %in% merge(out.dvc, jk[, .(STND_Y, PERSON_ID, AGE_GROUP)][AGE_GROUP < 11], by=c('STND_Y', 'PERSON_ID'))$PERSON_ID]$start.ohf <- NA
#out.dvc <- out.dvc[,-c(10, 11, 12, 13, 14)]

#------------ 통함수 ------------  #

ALLINONE <- function(x, y, z, w, p, hra = TRUE){
  
  if(length(y) < 2){
    stop('length y should be more than 2')
  }
  if(0 %in% y){
    
  } else{
    stop('y should include 0')
  }
  y <- y[order(y)]
  if(y[1] < 0){
    stop('y should be positive')
  }
  
  if(length(x) < 2) {
    stop('length x should be more than 2')
  }
  
  

  
  #x = c('dementia', 'vasculardementia', 'cerebralinfarction', 'osteoporosis', 'ovf', 'ohf')
  #y = c(0, 30, 60, 90, 180, 365)
  #z = 365 #첫 방문부터 며칠 후를 indexdate로 볼지
  #w = 180 #washout period
  #p = 30 #(window period)

      out.dvc <- eval(parse(text = paste0("WIS(dia.",x[1],",", substr(x[1], 1, 3), ".d.l,", substr(x[1], 1, 3), ".p.l,", "'",x[1], "')")))

      for(n in (2:length(x))){
        out.dvc <- merge(out.dvc, eval(parse(text = paste0("WIS(dia.",x[n],",", substr(x[n], 1, 3), ".d.l,", substr(x[n], 1, 3), ".p.l,", "'",x[n], "')"))),
                         by = "PERSON_ID", all = T)
      }
      
      
#    lapply(2:length(x), function(n){
#      out.dvc <- merge(out.dvc, eval(parse(text = paste0("WIS(dia.",x[n],",", substr(x[n], 1, 3), ".d.l,", substr(x[n], 1, 3), ".p.l,", "'",x[n], "')"))),
#                       by = "PERSON_ID", all = T)
#    })
    out.dvc <- merge(out.dvc, t12.firstcome, by = "PERSON_ID", all = T)

    #*
    if('ovf' %in% x && 'ohf' %in% x){
      out.dvc[!is.na(start.ovf)][!is.na(start.ohf)][start.ovf < start.ohf]$start.ohf <- NA
      out.dvc[!is.na(start.ovf)][!is.na(start.ohf)][start.ovf > start.ohf]$start.ovf <- NA
    }
        
for(i in y) {
  
  if(hra == TRUE){
    
    # H2RA 포함할때
      nperiod <- merge(PPIWP(p, i), H2RAWP(p, i), all=T, keyby="PERSON_ID")
      colnames(nperiod) <- c("PERSON_ID", "nperiod.ppi", "periodstart.ppi", "nperiod.h2ra", "periodstart.h2ra")
      nperiod <- nperiod[, .(PERSON_ID, period.ppi = nperiod.ppi, 
                             start.ppi = ymd(periodstart.ppi), 
                             period.h2ra = nperiod.h2ra, 
                             start.h2ra = ymd(periodstart.h2ra))]

  } else{
    nperiod <- PPIWP(p, i)
    nperiod <- merge(nperiod, t12[PERSON_ID %in% no.ppi][, .SD[1], keyby='PERSON_ID'][, .(PERSON_ID)], by = "PERSON_ID", all = T)
    colnames(nperiod) <- c("PERSON_ID", "nperiod.ppi", "periodstart.ppi")
    nperiod <- nperiod[, .(PERSON_ID, period.ppi = nperiod.ppi, start.ppi = ymd(periodstart.ppi))]
    ################### 위는 H2RA 포함 안할때 쓰는것########################
  }


  

  
  TOT <- out.dvc
  TOT <- merge(nperiod, TOT, by = "PERSON_ID")
  TOT[is.na(period.ppi)]$period.ppi <- 0
  
  if(hra == TRUE){
     TOT[is.na(period.h2ra)]$period.h2ra <- 0        ####### H2RA 포함 할 때 쓰는것
      H2RA <- HTA(2, z, w, TOT)                   ########
  }

  PPI <- HTA(1, z, w, TOT)

  for(j in x){
    eval(parse(text = paste0("PPI[, day.", j, ":= ymd(start.", j, ")- ymd(start.come)]")))
    eval(parse(text = paste0("PPI[, day.", j, "CON",
                             ":= as.integer(is.na(day.",j,")) * (ymd(20131231) - ymd(start.come))]")))
    eval(parse(text = paste0("PPI[day.", j, "CON == 0]$day.",j, "CON <- NA")))
if(hra == TRUE){
  ######### H2RA 포함할때 쓰는것 #2 ##############################
          eval(parse(text = paste0("H2RA[, day.", j, ":= ymd(start.", j, ")- ymd(start.come)]")))
      eval(parse(text = paste0("H2RA[, day.", j, "CON",
                               ":= as.integer(is.na(day.",j,")) * (ymd(20131231) - ymd(start.come))]")))
      eval(parse(text = paste0("H2RA[day.", j, "CON == 0]$day.",j, "CON <- NA")))
}


    

  }
  
  eval(parse(text = paste0("PPI", i, "<- PPI")))

  if(hra == TRUE){
    eval(parse(text = paste0("H2RA", i, "<- H2RA")))
  }
  
  }
    

    
  list.1 <- lapply(2:length(y), function(k){
  eval(parse(text = paste0(
    "vv <-rbind(PPI", y[k],"[period.ppi >=", y[k], "], PPI", y[1], "[period.ppi == ", y[1], "])"
  )))
  eval(parse(text = paste0(
    "vv[period.ppi >= ", y[k], "]$EXPCON <- 1"
  )))
  eval(parse(text = paste0(
    "vv[period.ppi == ", y[1], "]$EXPCON <- 2"
  )))
  return(vv)
  
})

list.2 <- lapply(2:length(y), function(k){
  eval(parse(text = paste0(
    "vv <- rbind(PPI", y[k], "[period.ppi >= ", y[k], "], PPI0[period.ppi <", y[2], "])"
  )))
  eval(parse(text = paste0(
    "vv[period.ppi >=", y[k], "]$EXPCON <- 1" 
  )))
  eval(parse(text = paste0(
    "vv[period.ppi <", y[2], "]$EXPCON <- 2" 
  )))
  return(vv)
  
})


if(hra == T){
  ########## H2RA 포함할때 쓰는것 #4 /4 
  list.3 <- lapply(2:length(y), function(k){
    
    eval(parse(text = paste0(
      "vv <- rbind(PPI", y[k], "[PERSON_ID %in% H2RA0[period.h2ra == 0]$PERSON_ID], H2RA", y[k],"[PERSON_ID %in% PPI0[period.ppi == 0]$PERSON_ID])"
    )))
  
    vv[period.h2ra == 0]$EXPCON <- 1
    vv[period.ppi == 0]$EXPCON <- 2
    
    return(vv)
    
    
  })
  
  
  out.list <- list("PPIneveruse" = list.1, "PPI<30" = list.2, "H2RA" = list.3)
} else{
  out.list <- list("PPIneveruse" = list.1, "PPI<30" = list.2)
}





return(out.list)

}


# l <- ALLINONE(c('dementia','vasculardementia','cerebralinfarction', 'osteoporosis', 'ovf', 'ohf'), c(0, 30, 60, 90, 180, 365), 365, 180, 30)

# fwrite(df(as.data.table(ll[[1]][1])), "/home/whe/period30/1027/ppi_2_ppi_0.csv")
# fwrite(df(as.data.table(ll[[1]][2])), "/home/whe/period30/1027/ppi_3_ppi_0.csv")
# fwrite(df(as.data.table(ll[[1]][3])), "/home/whe/period30/1027/ppi_4_ppi_0.csv")
# fwrite(df(as.data.table(ll[[1]][4])), "/home/whe/period30/1027/ppi_5_ppi_0.csv")
# fwrite(df(as.data.table(ll[[2]][1])), "/home/whe/period30/1027/ppi_2_ppi_1.csv")
# fwrite(df(as.data.table(ll[[2]][2])), "/home/whe/period30/1027/ppi_3_ppi_1.csv")
# fwrite(df(as.data.table(ll[[2]][3])), "/home/whe/period30/1027/ppi_4_ppi_1.csv")
# fwrite(df(as.data.table(ll[[2]][4])), "/home/whe/period30/1027/ppi_5_ppi_1.csv")
# fwrite(df(as.data.table(ll[[3]][1])), "/home/whe/period30/1027/ppi_2_h2ra_2.csv")
# fwrite(df(as.data.table(ll[[3]][2])), "/home/whe/period30/1027/ppi_3_h2ra_3.csv")
# fwrite(df(as.data.table(ll[[3]][3])), "/home/whe/period30/1027/ppi_4_h2ra_4.csv")
# fwrite(df(as.data.table(ll[[3]][4])), "/home/whe/period30/1027/ppi_5_h2ra_5.csv")
# fwrite(df(as.data.table(ll[[1]][5])), "/home/whe/period30/1027/ppi_6_ppi_0.csv")
# fwrite(df(as.data.table(ll[[2]][5])), "/home/whe/period30/1027/ppi_6_ppi_1.csv")
# fwrite(df(as.data.table(ll[[3]][1])), "/home/whe/period30/1027/ppi_2_h2ra_2.csv")
# fwrite(df(as.data.table(ll[[3]][2])), "/home/whe/period30/1027/ppi_3_h2ra_3.csv")
# fwrite(df(as.data.table(ll[[3]][3])), "/home/whe/period30/1027/ppi_4_h2ra_4.csv")
# fwrite(df(as.data.table(ll[[3]][4])), "/home/whe/period30/1027/ppi_5_h2ra_5.csv")
# fwrite(df(as.data.table(ll[[3]][5])), "/home/whe/period30/1027/ppi_6_h2ra_6.csv")



# 365, 730, 1095, 1460, 1825 는 그대로
# 1-90 91-365 는 nperiod 수작업

kk <- read_fst("t1_120.fst", as.data.table = T); setkey(t12, KEY_SEQ)
kk [, day := RECU_FR_DT[.N]-RECU_FR_DT[1], keyby="PERSON_ID"]
no<-kk[day < 365]$PERSON_ID
df <- function(x){
  x <- x[PERSON_ID %in% setdiff(x$PERSON_ID, no)]
  return(x)
}
rm(kk)

#l <- ALLINONE(c('dementia','vasculardementia','cerebralinfarction', 'osteoporosis', 'ovf', 'ohf'), c(0, 365, 730, 1095, 1460, 1825), 365, 180, 30, hra = FALSE)


#ppi365ppi0 <- as.data.table(l[[1]][[1]])
#ppi730ppi0 <- as.data.table(l[[1]][[2]])
#ppi1095ppi0 <- as.data.table(l[[1]][[3]])
#ppi1460ppi0 <- as.data.table(l[[1]][[4]])
#ppi1825ppi0 <- as.data.table(l[[1]][[5]])



#fwrite(df(ppi365ppi0), "/home/whe/osteo/3650.csv")
#fwrite(df(ppi730ppi0), "/home/whe/osteo/7300.csv")
#fwrite(df(ppi1095ppi0), "/home/whe/osteo/10950.csv")
#fwrite(df(ppi1460ppi0), "/home/whe/osteo/14600.csv")
#fwrite(df(ppi1825ppi0), "/home/whe/osteo/18250.csv")

#ppi90 <- fread('/home/whe/period30/1027/ppi_4_ppi_0.csv')

#ppi0 <- fread('/home/whe/period30/1027/ppi_2_ppi_1.csv')

#ppi730 <- fread('/home/whe/osteo/7300.csv')
#ppi1825 <- fread('/home/whe/osteo/18250.csv')
#ppi1460 <- fread('/home/whe/osteo/14600.csv')
#ppi1095 <- fread('/home/whe/osteo/10950.csv')
#ppi365 <- fread('/home/whe/osteo/3650.csv')
#ppimax<- fread('/home/whe/period30/1027/ppi.csv')
#ppifast <- fread('/home/whe/period30/1027/ppifast.csv')

#ppimax는 ALLINONE 함수 내에서 PPI0 만 수작업으로 얻어낸 것 (가장 긴 주기의 period)
#ppifast는 ALLINONE 함수 내에서 PPI1 만 수작업으로 얻어낸 것 (가장 빠른 주기의 period)

#ppi0 <- rbind(ppifast[PERSON_ID %in% ppimax[period.ppi <= 90]$PERSON_ID][period.ppi > 0], ppi365[period.ppi == 0])
# ppi(1, 30) 한거 가져다 두어야함
# 0 vs 1~90 <- 이때 indexdate를 1일 이상일때 가장 빠른 ppi 섭취로 두었기 때문에 n수가 조금 줄어듦.
#ppi90 <- rbind(ppi90[PERSON_ID %in% ppimax[period.ppi <= 365]$PERSON_ID][period.ppi > 0][, -c('period.h2ra', 'start.h2ra')], ppi365[period.ppi == 0])

#fwrite(ppi0, '/home/whe/osteo/00.csv')
#fwrite(ppi90, '/home/whe/osteo/900.csv')
