library(tidyverse)

moda=function(vector){
  return(as.numeric(names(which.max(table(unique(vector))))))
}

factores=function(vector){
  return(names(table(vector)[table(vector)>50]))
}

limpiar=function(df){
  i=0
  df <- as.data.frame(lapply(df, as.numeric))
  for (k in 2:length(colnames(df))){
    for (r in 1:length(row.names(df))){
      if (is.na(df[r,k])==1){
        df[r,k] <-moda(df[,k])
      }else if(df[r,k]>10){
        df[r,k] <- round(df[r,k]/10)
      }else if ((k==length(colnames(df))) && (df[r,k] %in% c(3,1,0))){
        print(df[r,k])
        df[r,k] = moda(df[,k])
        print(names(as.numeric(names(which.max(table(unique(df[,k])))))))
        i=0
      }
      print(!(df[r,k] %in% c(2,4,"NA")))
      i=i+1
    }
  }
  print(i)

  return(df)
}



limpiar2=function(df){
  for (k in 2:length(colnames(df))){
    print(k)
    print(df[,k])
    if (is.numeric(df[,k]==TRUE)){
      for (r in 1:length(row.names(df))){
        if (is.na(df[r,k])==1){
          df[r,k] <-round(mean(df[,k][is.na(df[,k])==0]))
        }else if(df[r,k]>10){
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
        }else if ((k==length(colnames(df))) && !(df[r,k] %in% levl)){
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

raw_train=read.csv("./Data/Breast_Cancer_train.data",sep="_",header = F)
raw_test <- read.csv("./Data/Breast_Cancer_test_completo.csv")


train_complete <- raw_train %>% mutate_all(~ str_replace_all(., "h", ""))
train_complete  <- as.data.frame(lapply(train_complete, as.numeric))
train_complete[,12] <- as.factor(train_complete[,12])
colnames(train_complete)=c("ID","clump_thickness","unif_cell_size","unif_cell_shape","Marg_adhes","Epith_cell_size","Bare_nucl","Bland_chrom","Normal_nucleoli","Mitoses","Group","class")
train_complete <- limpiar2(train_complete)

test_complete <- limpiar(raw_test)

colnames(test_complete)=c("ID","clump_thickness","unif_cell_size","unif_cell_shape","Marg_adhes","Epith_cell_size","Bare_nucl","Bland_chrom","Normal_nucleoli","Mitoses","Group","class")
colnames(train_complete)=c("ID","clump_thickness","unif_cell_size","unif_cell_shape","Marg_adhes","Epith_cell_size","Bare_nucl","Bland_chrom","Normal_nucleoli","Mitoses","Group","class")
test_complete$class <- (test_complete$class/2)-1
train_complete$class <- (train_complete$class/2)-1

train_complete$class <- round(train_complete$class)
test_complete$class <- round(test_complete$class)

test_complete$class <- as.factor(test_complete$class)
levels(test_complete$class) <- c("Benign","Malign")
train_complete$class <- as.factor(train_complete$class)
levels(train_complete$class) <- c("Benign","Malign")



## Ya hemos preparado los datos
