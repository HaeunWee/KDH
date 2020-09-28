library(fst); library(data.table); library(stringr)
library(lubridate); library(magrittr); library(parallel)
setwd("/home/whe")

#------------약물 검색 함수용-----------
druginfo <- fread("건강보험심사평가원_의약품 주성분 정보 2019.12.csv")


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
  D_ulcer = "K27"                     ## 십이지장궤양
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
          "181301ACE", "181301ATD", "181302ACE", "181302ATD", "181302ATE", "621901ACR", "621902ACR", "505501ATE")
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

#------------- PPI, H2RA windowperiod 2일로 설정한것 파일 불러오기-------

nperiod0 <- fread("nperiod0.csv")
nperiod30 <- fread("nperiod30.csv")
nperiod60 <- fread("nperiod60.csv")
nperiod90 <- fread("nperiod90.csv")
nperiod180 <- fread("nperiod180.csv")

nperiod0 <- nperiod0[, .(PERSON_ID, period.ppi = nperiod.ppi0, 
                         start.ppi = ymd(periodstart.ppi0), 
                         period.h2ra = nperiod.h2ra0, 
                         start.h2ra = ymd(periodstart.h2ra0))]

nperiod30 <- nperiod30[, .(PERSON_ID, period.ppi = nperiod.ppi30, 
                           start.ppi = ymd(periodstart.ppi30), 
                           period.h2ra = nperiod.h2ra30, 
                           start.h2ra = ymd(periodstart.h2ra30))]

nperiod60 <- nperiod60[, .(PERSON_ID, period.ppi = nperiod.ppi60, 
                           start.ppi = ymd(periodstart.ppi60), 
                           period.h2ra = nperiod.h2ra60, 
                           start.h2ra = ymd(periodstart.h2ra60))]

nperiod90 <- nperiod90[, .(PERSON_ID, period.ppi = nperiod.ppi90, 
                           start.ppi = ymd(periodstart.ppi90), 
                           period.h2ra = nperiod.h2ra90, 
                           start.h2ra = ymd(periodstart.h2ra90))]

nperiod180 <- nperiod180[, .(PERSON_ID, period.ppi = nperiod.ppi180, 
                             start.ppi = ymd(periodstart.ppi180), 
                             period.h2ra = nperiod.h2ra180, 
                             start.h2ra = ymd(periodstart.h2ra180))]

t14.mkcci <- merge(t12[, .(PERSON_ID, KEY_SEQ)],
                   t14[SICK_SYM %in% unique(unlist(t14.cci))],
                   by="KEY_SEQ")[, RECU_FR_DT := ymd(RECU_FR_DT)]


# PPI 분석에 필요한거 + 과거력 분석에 필요한것 불러오기




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
ov1.d.l <- NULL
ov2.d.l <- NULL
ohf.d.l <- NULL
aki.d.l <- NULL

#----- procedures list ----

vas.p.l <- NULL
dem.p.l <- NULL
cer.p.l <- NULL
ost.p.l <- NULL
ov1.p.l <- c("N0471", "N0472", "N0473", "N0474", "N0630")
ov2.p.l <- c("N0471", "N0472", "N0473", "N0474", "N0630")
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


HTA <- function(x, y, z){
  # x = 1 (PPI vs 나머지) 2 (H2RA vs 나머지)
  # y = 과거력을 index date y일 전부터 index date까지 검색함
  #     ppi(1) or h2ra(2) 첫 복용일 포함 복용전 y일을 확보하지 못할 경우 삭제
  # z = ppi(1) or h2ra(2) 첫 복용일 포함 z일 이전 질병 발병자들만 질병 발병자들로봄 (wash-out period)
  TOT.temp <- TOT
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
  
  vars.t14cci <- lapply(1:length(t14.cci), function(i){
    person.ev <- merge(TOT.temp[, .(PERSON_ID, indexdate)], t14.mkcci[SICK_SYM %in% t14.cci[[i]]], by = "PERSON_ID")[order(PERSON_ID, RECU_FR_DT)][, .SD[1], keyby = "PERSON_ID"][RECU_FR_DT <= indexdate][, .(PERSON_ID, var = cciscore[[i]])]
    vv <- merge(TOT.temp[, .(PERSON_ID)], person.ev, by = "PERSON_ID", all = T)[, var := ifelse(is.na(var), 0, var)]$var
    return(vv)
  }) %>% Reduce(cbind, .)
  colnames(vars.t14cci) <- names(t14.cci)
  
  TOT.temp <- cbind(TOT.temp, vars.t14cci)
  TOT.temp[, STND_Y := as.integer(str_sub(indexdate, 1, 4))]
  TOT.temp <- merge(TOT.temp,jk[PERSON_ID %in% TOT.temp$PERSON_ID],by=c("PERSON_ID","STND_Y"))
  TOT.temp[, CCI := MI + CHF + PVD + CD + DE + CPD + RD + PUD + MLD + DWOCC + DWCC + HOP + AMILALEMNOS + MOSLD + MST + AH]
  
  return(TOT.temp)
}




# 질병 - 약물 and 로 묶고 진단일과 처방일 중 느린것을 질병 시작일로 택
# 질병 코드는 dia 에 약물코드는 d.l에 이름은 nms에
# ex ) dementia 의 경우 dia.dementia, dem.d.l, "dementia" 로 저장


# ------ 골다공증만 따로 건드려주기 #* 부분에 포함될 것 -----

out.dvc[!is.na(start.ov1)][!is.na(start.ohf)][start.ov1 < start.ohf]$start.ohf <- NA
out.dvc[!is.na(start.ov1)][!is.na(start.ohf)][start.ov1 > start.ohf]$start.ov1 <- NA
out.dvc[!is.na(start.ov2)][!is.na(start.ohf)][start.ov2 < start.ohf]$start.ohf <- NA
out.dvc[!is.na(start.ov2)][!is.na(start.ohf)][start.ov2 > start.ohf]$start.ov2 <- NA
out.dvc[, year.ost := substr(start.osteoporosis, 1, 4)]
out.dvc[, year.ov1 := substr(start.ov1, 1, 4)]
out.dvc[, year.ov2 := substr(start.ov2, 1, 4)]
out.dvc[, year.ohf := substr(start.ohf, 1, 4)]

out.dvc[, STND_Y := as.integer(year.ost)]
out.dvc[PERSON_ID %in% merge(out.dvc, jk[, .(STND_Y, PERSON_ID, AGE_GROUP)][AGE_GROUP < 11], by=c('STND_Y', 'PERSON_ID'))$PERSON_ID]$start.osteoporosis <- NA
out.dvc[, STND_Y := as.integer(year.ov1)]
out.dvc[PERSON_ID %in% merge(out.dvc, jk[, .(STND_Y, PERSON_ID, AGE_GROUP)][AGE_GROUP < 11], by=c('STND_Y', 'PERSON_ID'))$PERSON_ID]$start.ov1 <- NA
out.dvc[, STND_Y := as.integer(year.ov2)]
out.dvc[PERSON_ID %in% merge(out.dvc, jk[, .(STND_Y, PERSON_ID, AGE_GROUP)][AGE_GROUP < 11], by=c('STND_Y', 'PERSON_ID'))$PERSON_ID]$start.ov2 <- NA
out.dvc[, STND_Y := as.integer(year.ohf)]
out.dvc[PERSON_ID %in% merge(out.dvc, jk[, .(STND_Y, PERSON_ID, AGE_GROUP)][AGE_GROUP < 11], by=c('STND_Y', 'PERSON_ID'))$PERSON_ID]$start.ohf <- NA
out.dvc <- out.dvc[,-c(10, 11, 12, 13, 14)]

#------------ 통함수 ------------ 아직 오류나서 변수 대입해놓고 손으로 실행해야함 #

ALLINONE <- function(x, y, z, w){
  
  #x = c('dementia', 'vasculardementia', 'cerebralinfarction', 'osteoporosis', 'ov1', 'ov2', 'ohf')
  #y = c(0, 30, 60, 90, 180)
  #z = 365
  #w = 180

      out.dvc <- eval(parse(text = paste0("WIS(dia.",x[1],",", substr(x[1], 1, 3), ".d.l,", substr(x[1], 1, 3), ".p.l,", "'",x[1], "')")))
    for(n in 2:length(x)){
      out.dvc <- merge(out.dvc, eval(parse(text = paste0("WIS(dia.",x[n],",", substr(x[n], 1, 3), ".d.l,", substr(x[n], 1, 3), ".p.l,", "'",x[n], "')"))),
                       by = "PERSON_ID", all = T)
    }
    out.dvc <- merge(out.dvc, t12.firstcome, by = "PERSON_ID", all = T)

    #*
    
for(i in y) {
  nperiod <- eval(parse(text = paste0("nperiod",i)))
  TOT <- out.dvc
  TOT <- merge(nperiod, TOT, by = "PERSON_ID")
  TOT[is.na(period.ppi)]$period.ppi <- 0
  TOT[is.na(period.h2ra)]$period.h2ra <- 0
  PPI <- HTA(1, z, w)
  H2RA <- HTA(2, z, w)
  for(j in x){
    eval(parse(text = paste0("PPI[, day.", j, ":= ymd(start.", j, ")- ymd(start.come)]")))
    eval(parse(text = paste0("PPI[, day.", j, "CON",
                             ":= as.integer(is.na(day.",j,")) * (ymd(20131231) - ymd(start.come))]")))
    eval(parse(text = paste0("PPI[day.", j, "CON == 0]$day.",j, "CON <- NA")))

        eval(parse(text = paste0("H2RA[, day.", j, ":= ymd(start.", j, ")- ymd(start.come)]")))
    eval(parse(text = paste0("H2RA[, day.", j, "CON",
                             ":= as.integer(is.na(day.",j,")) * (ymd(20131231) - ymd(start.come))]")))
    eval(parse(text = paste0("H2RA[day.", j, "CON == 0]$day.",j, "CON <- NA")))

    

  }
  
  eval(parse(text = paste0("PPI", i, "<- PPI")))
  eval(parse(text = paste0("H2RA", i, "<- H2RA")))
  
  
  }

    # PPI 30 vs PPI never use
    
    PPI.2.PPI.0 <- rbind(PPI30[period.ppi >= 30],PPI0[period.ppi == 0])
    PPI.2.PPI.0[period.ppi >= 30]$EXPCON <- 1
    PPI.2.PPI.0[period.ppi == 0]$EXPCON <- 2
    fwrite(PPI.2.PPI.0, "PPI_2_PPI_0.csv")
    
    # PPI 30 vs PPI < 30
    
    PPI.2.PPI.1 <- rbind(PPI30[period.ppi >= 30],PPI0[period.ppi < 30])
    PPI.2.PPI.1[period.ppi >= 30]$EXPCON <- 1
    PPI.2.PPI.1[period.ppi < 30]$EXPCON <- 2
    fwrite(PPI.2.PPI.1, "PPI_2_PPI_1.csv")
    
    # PPI 30 vs H2RA 30
    
    PPI.2.H2RA.2 <- rbind(PPI30[PERSON_ID %in% H2RA0[period.h2ra==0]$PERSON_ID], H2RA30[PERSON_ID %in% PPI0[period.ppi==0]$PERSON_ID])
    PPI.2.H2RA.2[period.h2ra == 0]$EXPCON <- 1
    PPI.2.H2RA.2[period.ppi == 0]$EXPCON <- 2
    fwrite(PPI.2.H2RA.2, "PPI_2_H2RA_2.csv")
    
    # PPI 60 vs PPI never use
    
    PPI.3.PPI.0 <- rbind(PPI60[period.ppi >= 60],PPI0[period.ppi == 0])
    PPI.3.PPI.0[period.ppi >= 60]$EXPCON <- 1
    PPI.3.PPI.0[period.ppi == 0]$EXPCON <- 2
    fwrite(PPI.3.PPI.0, "PPI_3_PPI_0.csv")
    
    # PPI 60 vs PPI < 30
    
    PPI.3.PPI.1 <- rbind(PPI60[period.ppi >= 60],PPI0[period.ppi < 30])
    PPI.3.PPI.1[period.ppi >= 60]$EXPCON <- 1
    PPI.3.PPI.1[period.ppi < 30]$EXPCON <- 2
    fwrite(PPI.3.PPI.1, "PPI_3_PPI_1.csv")
    
    # PPI 60 vs H2RA 60
    
    PPI.3.H2RA.3 <- rbind(PPI60[PERSON_ID %in% H2RA0[period.h2ra==0]$PERSON_ID], H2RA60[PERSON_ID %in% PPI0[period.ppi==0]$PERSON_ID])
    PPI.3.H2RA.3[period.h2ra == 0]$EXPCON <- 1
    PPI.3.H2RA.3[period.ppi == 0]$EXPCON <- 2
    fwrite(PPI.3.H2RA.3, "PPI_3_H2RA_3.csv")
    
    # PPI 90 vs PPI never use
    
    PPI.4.PPI.0 <- rbind(PPI90[period.ppi >= 90],PPI0[period.ppi == 0])
    PPI.4.PPI.0[period.ppi >= 90]$EXPCON <- 1
    PPI.4.PPI.0[period.ppi == 0]$EXPCON <- 2
    fwrite(PPI.4.PPI.0, "PPI_4_PPI_0.csv")
    
    # PPI 90 vs PPI < 30
    
    PPI.4.PPI.1 <- rbind(PPI90[period.ppi >= 90],PPI0[period.ppi < 30])
    PPI.4.PPI.1[period.ppi >= 90]$EXPCON <- 1
    PPI.4.PPI.1[period.ppi < 30]$EXPCON <- 2
    fwrite(PPI.4.PPI.1, "PPI_4_PPI_1.csv")
    
    # PPI 90 vs H2RA 90
    
    PPI.4.H2RA.4 <- rbind(PPI90[PERSON_ID %in% H2RA0[period.h2ra==0]$PERSON_ID], H2RA90[PERSON_ID %in% PPI0[period.ppi==0]$PERSON_ID])
    PPI.4.H2RA.4[period.h2ra == 0]$EXPCON <- 1
    PPI.4.H2RA.4[period.ppi == 0]$EXPCON <- 2
    fwrite(PPI.4.H2RA.4, "PPI_4_H2RA_4.csv")
    
    # PPI 180 vs PPI never use
    
    PPI.5.PPI.0 <- rbind(PPI180[period.ppi >= 180],PPI0[period.ppi == 0])
    PPI.5.PPI.0[period.ppi >= 180]$EXPCON <- 1
    PPI.5.PPI.0[period.ppi == 0]$EXPCON <- 2
    fwrite(PPI.5.PPI.0, "PPI_5_PPI_0.csv")
    
    # PPI 180 vs PPI < 30
    
    PPI.5.PPI.1 <- rbind(PPI180[period.ppi >= 180],PPI0[period.ppi < 30])
    PPI.5.PPI.1[period.ppi >= 180]$EXPCON <- 1
    PPI.5.PPI.1[period.ppi < 30]$EXPCON <- 2
    fwrite(PPI.5.PPI.1, "PPI_5_PPI_1.csv")
    
    # PPI 180 vs H2RA 180
    
    PPI.5.H2RA.5 <- rbind(PPI180[PERSON_ID %in% H2RA0[period.h2ra==0]$PERSON_ID], H2RA180[PERSON_ID %in% PPI0[period.ppi==0]$PERSON_ID])
    PPI.5.H2RA.5[period.h2ra == 0]$EXPCON <- 1
    PPI.5.H2RA.5[period.ppi == 0]$EXPCON <- 2
    fwrite(PPI.5.H2RA.5, "PPI_5_H2RA_5.csv")
    
    
    
}













