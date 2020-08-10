setwd('/home/whe')
library(shiny)
library(knitr)
library(data.table)
library(fst)             ## For Fst binary format
library(lubridate)
library(magrittr)
library(stringr)
library(parallel)

PPI <- fread("PPI.csv")
H2RA <- fread("H2RA.csv")

# 기존

PPI0 <- fread("PPI0.csv")
H2RA0 <- fread("H2RA0.csv")

# 가장 긴 period

PPI30 <- fread("PPI30.csv")
H2RA30 <- fread("H2RA30.csv")

# 30일 넘는 period 중 가장 빠른 period

PPI60 <- fread("PPI60.csv")
H2RA60 <- fread("H2RA60.csv")

# 60일 넘는 period 중 가장 빠른 period

PPI90 <- fread("PPI90.csv")
H2RA90 <- fread("H2RA90.csv")

# 90일 넘는 period 중 가장 빠른 period

PPI180 <- fread("PPI180.csv")
H2RA180 <- fread("H2RA180.csv")

# 180일 넘는 period 중 가장 빠른 period



# EXPCON 1 : 실험군 vs EXPCON 2 : 대조군

# 이름 규칙
# 0 :: 0
# 1 :: 1 ~ 
# 2 :: 30 ~ 
# 3 :: 60 ~ 
# 4 :: 90 ~ 
# 5 :: 180 ~


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

