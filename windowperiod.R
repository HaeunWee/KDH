library(fst)             ## For Fst binary format
library(data.table)
library(lubridate)
library(magrittr)
library(stringr)         ## For str_sub: replace substr
library(parallel)
setwd("/home/whe")
t16 <- read_fst("t1_160.fst", as.data.table = T)
PPI <- fread("PPI.csv")
H2RA <- fread("H2RA.csv")
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
t16ppi<-t16[GNL_NM_CD %in% code.ppi]
t16h2ra<-t16[GNL_NM_CD %in% code.h2ra]
rm(t16)

t12 <- read_fst("t1_120.fst", as.data.table = T)
t12.perkey<-t12[,.(PERSON_ID,KEY_SEQ)]
rm(t12)
t16ppi<-merge(t12.perkey[KEY_SEQ %in% t16ppi$KEY_SEQ], t16ppi, by = "KEY_SEQ")
t16h2ra<-
  merge(t12.perkey[KEY_SEQ %in% t16h2ra$KEY_SEQ], t16h2ra, by = "KEY_SEQ")

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



PPIWP <- function(x){
  #x = Window period
  
  t16ppif <- t16ppi
  t16ppif <- t16ppif[order(PERSON_ID, RECU_FR_DT, MDCN_EXEC_FREQ, KEY_SEQ, SEQ_NO)]
  
  t16ppif[, periodfinish.ppi := RECU_FR_DT + MDCN_EXEC_FREQ]
  t16ppif$periodfinish.ppi <- c(as_date(1),t16ppif$periodfinish.ppi[-nrow(t16ppif)])
  t16ppif$periodstart.ppi <- c(as_date(1),t16ppif$periodstart.ppi[-nrow(t16ppif)])
  t16ppif[, jj := PERSON_ID]
  t16ppif$jj <- c(1,t16ppif$PERSON_ID[-nrow(t16ppif)])
  
  t16ppif[, oo := 1]
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
  
  t16ppif <- t16ppif[order(RECU_FR_DT)]
  t16ppif.fin <- t16ppif[, .SD[.N], by = c('PERSON_ID','ooo')]
  t16ppif <- t16ppif[order(RECU_FR_DT + MDCN_EXEC_FREQ)]
  t16ppif.ini <- t16ppif[, .SD[1], by = c('PERSON_ID','ooo')]
  t16ppifpr <- merge(t16ppif.ini[, .(PERSON_ID, ooo, periodstart.ppi = RECU_FR_DT)], t16ppif.fin[, .(PERSON_ID, ooo, periodfinish.ppi = RECU_FR_DT + MDCN_EXEC_FREQ)], keyby=c("PERSON_ID, ooo"))
  t16ppifpr <- t16ppifpr[, .(PERSON_ID, nperiod.ppi = periodfinish.ppi - periodstart.ppi)]
  t16ppifpr <- t16ppifpr[order(PERSON_ID, nperiod.ppi)]
  t16ppifpr <- t16ppifpr[, .SD[.N], by="PERSON_ID"]
  
  return(t16ppifpr)
}

H2RAWP <- function(x){
  #x = Window period
  
  t16h2raf <- t16h2ra
  t16h2raf <- t16h2raf[order(PERSON_ID, RECU_FR_DT, MDCN_EXEC_FREQ, KEY_SEQ, SEQ_NO)]
  
  t16h2raf[, periodfinish.h2ra := RECU_FR_DT + MDCN_EXEC_FREQ]
  t16h2raf$periodfinish.h2ra <- c(as_date(1),t16h2raf$periodfinish.h2ra[-nrow(t16h2raf)])
  t16h2raf$periodstart.h2ra <- c(as_date(1),t16h2raf$periodstart.h2ra[-nrow(t16h2raf)])
  t16h2raf[, jj := PERSON_ID]
  t16h2raf$jj <- c(1,t16h2raf$PERSON_ID[-nrow(t16h2raf)])
  
  t16h2raf[, oo := 1]
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
  
  t16h2raf <- t16h2raf[order(RECU_FR_DT)]
  t16h2raf.fin <- t16h2raf[, .SD[.N], by = c('PERSON_ID','ooo')]
  t16h2raf <- t16h2raf[order(RECU_FR_DT + MDCN_EXEC_FREQ)]
  t16h2raf.ini <- t16h2raf[, .SD[1], by = c('PERSON_ID','ooo')]
  t16h2rafpr <- merge(t16h2raf.ini[, .(PERSON_ID, ooo, periodstart.h2ra = RECU_FR_DT)], t16h2raf.fin[, .(PERSON_ID, ooo, periodfinish.h2ra = RECU_FR_DT + MDCN_EXEC_FREQ)], keyby=c("PERSON_ID, ooo"))
  t16h2rafpr <- t16h2rafpr[, .(PERSON_ID, nperiod.h2ra = periodfinish.h2ra - periodstart.h2ra)]
  t16h2rafpr <- t16h2rafpr[order(PERSON_ID, nperiod.h2ra)]
  t16h2rafpr <- t16h2rafpr[, .SD[.N], by="PERSON_ID"]
  
  return(t16h2rafpr)
}

t16ppipr <- PPIWP(2)
t16h2rapr <- H2RAWP(2)
nperiod <- merge(t16ppipr, t16h2rapr, all=T)
fwrite(nperiod, "nperiod.csv")
