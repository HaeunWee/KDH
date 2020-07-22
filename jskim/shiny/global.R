library(data.table)
library(fst)
library(jstable)


data.PPI <- fread("/home/whe/PPI.csv")[AGE_GROUP > 4, c(49, 50, 25:46, 8, 47, 48, 18, 16, 21, 23)]
data.H2RA <- fread("/home/whe/H2RA.csv")[AGE_GROUP > 4, c(49, 50, 25:46, 8, 47, 48, 18, 16, 21, 23)]


var.factor <- names(data.PPI)

data.PPI[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
data.H2RA[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]




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


label[variable == "EXPCON", `:=`(var_label = "Group", val_label = c("Non-user", "User"))]
