library(openxlsx)
library(data.table)
library(plyr)

db<-read.xlsx(xlsxFile = "data/final_pop_update_may2017.xlsx", sheet = 3)

write.csv(db,file="data/final_pop_update_map2017.csv",fileEncoding = "UTF-8")

#add column
db$pop_diff <- as.numeric(db$Pop.Data.Update.May.2017 - db$Pop_estimates_HNO.2017)

#table for Aleppo
db_aleppo <- db[db$Admin1Name_En =="Aleppo",]


#sum population by governorate
## tapply(Summary Variable, Group Variable, Function)
db_pop_gov <- data.frame(tapply(db$Pop_estimates_HNO.2017,db$Admin1Name_En,sum,na.rm=TRUE))

# renaming column name by index
names(db_pop_gov)[0] <- "gov"
names(db_pop_gov)[1] <- "sum_pop"
db_pop_gov <- rename(db_pop_gov,c("sum_pop"="sum_pop_gov"))
#renaming alternate
colnames(db_pop_gov)[which(colnames(db_pop_gov) == 'sum_pop_gov')] <- "sum_pop_hno2017"

#---if else---------
db$change = ''
col_index_diff = which(colnames(db) == 'pop_diff')
col_index_change = which(colnames(db) == 'change')

db[,13]<-ifelse(db[,12]>0,"increased",ifelse(db[,12]<0,"decreased",db[,13]))

for (i in 1:nrow(db)){
  if (!is.na(db$pop_diff[i])){
      if (db$pop_diff[i] > 0){
        db$change[i]<- "increased"
      } else if (db$pop_diff[i] < 0){
        db$change[i]<- "decreased"
      } else if (db$pop_diff[i]==0) {
        db$change[i] <- "same"
      }
  }
}


