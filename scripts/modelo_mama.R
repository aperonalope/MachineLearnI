library(tidyverse)
library(dplyr)
library(stringr)

moda=function(vector){
  return(as.numeric(names(which.max(table(unique(vector))))))
}

factores=function(vector){
  return(names(table(vector)[table(vector)>50]))
}


limpiar2=function(df){
  for (k in 2:length(colnames(df))){
    if (is.numeric(df[,k])==TRUE){
      print("hola")
      for (r in 1:length(row.names(df))){
        if (is.na(df[r,k])==1){
          print("na")
          df[r,k] <-round(mean(df[,k][is.na(df[,k])==0]))
        }else if(df[r,k]>10){
          print("mayor")
          df[r,k] <- round(df[r,k]/10)
        }
      }
    }else if(is.factor(df[,k])==TRUE){
      levl=factores(df[,k])
      df[,k] <- as.numeric(as.character(df[,k]))
      print(df[,k])
      
      print(levl)
      for (r in 1:length(row.names(df))){
        if (is.na(df[r,k])==1){
          print(1)
          df[r,k] <-moda(df[,k][is.na(df[,k])==0])
        }else if(df[r,k]>10){
          print(2)
          df[r,k] <- round(df[r,k]/10)
        }else if (!(df[r,k] %in% levl)){
          print(3)
          df[r,k] = moda(df[,k][is.na(df[,k])==0])
        }
      }
      df[,k] <- as.factor(df[,k])
      print(df[,k])
      levels(df[,k]) <- levl
    }
    
  }
    
  return(df)
}

raw_train=read.csv("./data/Breast_Cancer_train.data",sep="_",header = F)
raw_test <- read.csv("./data/Breast_Cancer_test_completo.csv")


train_complete <- raw_train %>% mutate_all(~ str_replace_all(., "h", ""))
train_complete  <- as.data.frame(lapply(train_complete, as.numeric))
train_complete[,12] <- as.factor(train_complete[,12])
colnames(train_complete)=c("ID","clump_thickness","unif_cell_size","unif_cell_shape","Marg_adhes","Epith_cell_size","Bare_nucl","Bland_chrom","Normal_nucleoli","Mitoses","Group","class")
train_complete <- limpiar2(train_complete)

test_complete <- as.data.frame(lapply(raw_test,as.numeric))
test_complete[,12] <- as.factor(test_complete[,12])
colnames(test_complete)=c("ID","clump_thickness","unif_cell_size","unif_cell_shape","Marg_adhes","Epith_cell_size","Bare_nucl","Bland_chrom","Normal_nucleoli","Mitoses","Group","class")
test_complete <- limpiar2(test_complete)

## Ya tenemos los datos bien limpios


levels(train_complete$class) <- c("Benign","Malign")
levels(test_complete$class) <- c("Benign","Malign")



## Ya hemos preparado los datos
train_complete <- train_complete[sample(1:length(row_number(train_complete))),]

tercio <- round(length(row_number(train_complete))/3)

train_group1 <- train_complete[1:tercio,]
train_group2 <- train_complete[(tercio+1):(tercio*2),]
train_group3 <- train_complete[((tercio*2)+1):length(row_number(train_complete)),]

formula="class~"
for (name in 2:(length(colnames(train_complete))-1)){
  if (name==2){
    formula=paste0(formula,colnames(train_complete)[name])
  }else{
    formula=paste(formula,colnames(train_complete)[name],sep="+")
  }
  
}

for (step in 1:10){
  print(step)
  modelo=glm(formula,train_group1,family=(binomial(link="logit")))
  resumen=summary(modelo)
  menos_influyente=rownames(resumen$coefficients)[resumen$coefficients[,4]==max(resumen$coefficients[,4])]
  primero_en_formula=unlist(str_split(str_remove(formula,"class~"),pattern = "\\+"))[1]
  if(resumen$coefficients[,4][resumen$coefficients[,4]==max(resumen$coefficients[,4])]<0.05){
    print("ya ha terminado el stepwise")
    break
  }
  if (menos_influyente==primero_en_formula){
    formula=str_remove(formula,paste0(primero_en_formula,"\\+"))
  }else{
    formula=str_remove(formula,paste0("\\+",menos_influyente))
  }
  
  
}
  



brea
