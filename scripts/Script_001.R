library(tidyverse)
h=read.csv("Data/heart_mod_2023-02-08.csv")
data=as.data.frame(read.csv("Data/heart_mod_2023-02-08.csv",sep=c("p")))

data$target[data$target==11]=1
data$target[data$target=="O"]=0
data$age[data$age>100]=data$age[data$age>100]/10
data$sex <- as.factor(data$sex)
data$trestbps[which(is.na(data$trestbps))]=mean(na.omit(data$trestbps))
data$trestbps[data$trestbps>300]=data$trestbps[data$trestbps>300]/10
r=table(data$ca)
r=r/sum(r)


data$cp <- as.factor(data$cp) # Chest pain type (4 types)

data$fbs <- as.factor(data$fbs) # Whether the level of sugar in the blood is higher than 120 mg/dl or not. 

data$restecg <- as.factor(data$restecg) # Results of the electrocardiogram on rest (3 types)

data$exang <- as.factor(data$exang) # Whether the patient had angina during exercise

data$slope <- as.factor(data$slope) # Slope of the ST segment during the most demanding part of the exercise (3 types)

data$thal <- as.factor(data$thal) # Results of the blood flow observed via the radioactive dye. (3 types)

data$target <- as.factor(data$target) # heart attack y/n

# Give a better name to the factor values for the graphs

levels(data$sex) <- c("Female", "Male")

levels(data$cp) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")

levels(data$fbs) <- c("No", "Yes")

levels(data$restecg) <- c("Hypertrophy", "Normal", "Abnormalities")

levels(data$exang) <- c("No", "Yes")

levels(data$slope) <- c("Descending", "Flat", "Ascending")

levels(data$thal) <- c("Fixed defect", "Normal flow", "Reversible defect")

levels(data$target) <- c("Yes", "No")

analisis=function(datafr,n){
  if(is.numeric(datafr[,n])==TRUE){
    boxplot(datafr[,n],main=colnames(datafr)[n])
    readline(prompt="Press [enter] to continue")
    hist(datafr[,n],main=colnames(datafr)[n])
  } else{
    barplot(table(datafr[,n]))
  }
}

analisis2=function(datafr,n){
  if(is.numeric(datafr[,n])==TRUE){
    print(datafr[,n])
    print(ggplot(datafr,aes(x=datafr[,n],y=target))+geom_boxplot()+ggtitle(colnames(datafr)[n]))
    readline(prompt="Press [enter] to continue")
    hist(datafr[,n],main=colnames(datafr)[n])
  } else{
   print(ggplot(datafr,aes(x=datafr[,n],color=target))+geom_bar()+ggtitle(colnames(datafr)[n]))
  }
}

c=vector()

##for (i in 1:length(colnames(data))){
  #analisis2(data,i)
  #readline(prompt="Press [enter] to continue")
#}


c=vector()
#for (i in 1:length(colnames(data))){
  #if (is.numeric(data[,i])==TRUE){
  #s=shapiro.test(data[,i])
  #c=append(c,s$p.value)
  #}else{
    #c=append(c,0)
 # }
  
#}


##centrar y estandarizar
##Para ello hay que restarle la media al dato y dividirlo por la desviacion estnadar
##De esta manera todos seran comparable
library(ggplot2)


cuantitativas= select(data,age,chol,thalach,oldpeak)
cualitativas=select(data,!colnames(cuantitativas))

seq_test=seq(from=1,to=round(303))
seq_train=sort(sample(seq_test,round(303*0.8)))
seq_test=seq_test[!(seq_test %in% seq_train)]

test=data[seq_test,]
train=data[seq_train,]


nombre=vector()
estim=vector()
pval=vector()
n=colnames(data)[colnames(data)!="target"]

for (col in n){
  formula1=as.formula(paste0("target~",col))
  glm=glm(formula1,data,family=(binomial(link="logit")))
  estim=append(estim,as.vector(glm$coefficients[2]))
  pval=append(pval,summary(glm)$coefficients[2,4])
  print(summary(glm)$coefficients[2,4])
}

datos_glm=rbind(estim,pval)
colnames(datos_glm)=n


datos_glm=as.data.frame(t(datos_glm))
datos_glm=cbind(row.names(datos_glm),datos_glm)
datos_glm=arrange(datos_glm,... =desc(pval))
datos_glm_ordenados=datos_glm
datos_glm_ordenados$pval=-log(datos_glm_ordenados$pval)
datos_glm_ordenados=arrange(datos_glm_ordenados,... =desc(pval))
ggplot(datos_glm_ordenados,aes(x=reorder(`row.names(datos_glm)`,-pval),y=pval))+geom_bar(stat = "identity",)+coord_flip()


data_filtrado=data[,colnames(data) %in% row.names(datos_glm[datos_glm$pval < 0.25,])]


formula2=as.formula("target~age+sex")

#formula1=as.formula(paste0("target~age"))
#formula2=as.formula("target~age+sex")


#modelo_glm <- glm(formula1,data,family=(binomial(link="logit")))
#summary(modelo_glm)
#El estimate de age es -0.053, lo que significa que por cada unidad que aumenta age, target disminuye 0.053 unidades
#El AIC cuanto mas pequeño sea mejor
  


#modelo_glm2 <- glm(formula2,data,family=(binomial(link="logit")))
#summary(modelo_glm2)
#El estimate de age es -0.053, lo que significa que por cada unidad que aumenta age, target disminuye 0.053 unidades
#El AIC cuanto mas pequeño sea mejor
#modelo_glma[1]

##STEPWISE forwad: metemso todas las variables y vamos quitando
formula2=as.formula("target~sex+cp+trestbps+chol+thalach+exang+ca")
glm_stepwise=glm(formula2,data,family=(binomial(link="logit")))
resumen=summary(glm_stepwise)
resumen=as.data.frame(resumen$coefficients)





nombre=vector()
estim=vector()
pval=vector()
n=colnames(train)[colnames(train)!="target"]

for (col in n){
  formula1=as.formula(paste0("target~",col))
  glm=glm(formula1,train,family=(binomial(link="logit")))
  estim=append(estim,as.vector(glm$coefficients[2]))
  pval=append(pval,summary(glm)$coefficients[2,4])
  print(summary(glm)$coefficients[2,4])
}

datos_glm=rbind(estim,pval)
colnames(datos_glm)=n


datos_glm=as.data.frame(t(datos_glm))
datos_glm=cbind(row.names(datos_glm),datos_glm)
datos_glm=arrange(datos_glm,... =desc(pval))
datos_glm_ordenados=datos_glm
datos_glm_ordenados$pval=-log(datos_glm_ordenados$pval)
datos_glm_ordenados=arrange(datos_glm_ordenados,... =desc(pval))
ggplot(datos_glm_ordenados,aes(x=reorder(`row.names(datos_glm)`,-pval),y=pval))+geom_bar(stat = "identity",)+coord_flip()


data_filtrado=data[,colnames(data) %in% row.names(datos_glm[datos_glm$pval < 0.25,])]


formula2=as.formula("target~age+sex")

#formula1=as.formula(paste0("target~age"))
#formula2=as.formula("target~age+sex")


#modelo_glm <- glm(formula1,data,family=(binomial(link="logit")))
#summary(modelo_glm)
#El estimate de age es -0.053, lo que significa que por cada unidad que aumenta age, target disminuye 0.053 unidades
#El AIC cuanto mas pequeño sea mejor



#modelo_glm2 <- glm(formula2,data,family=(binomial(link="logit")))
#summary(modelo_glm2)
#El estimate de age es -0.053, lo que significa que por cada unidad que aumenta age, target disminuye 0.053 unidades
#El AIC cuanto mas pequeño sea mejor
#modelo_glma[1]

##STEPWISE forwad: metemso todas las variables y vamos quitando
formula2=as.formula("target~sex+cp+trestbps+chol+thalach+exang+ca")
glm_stepwise=glm(formula2,train,family=(binomial(link="logit")))
resumen=summary(glm_stepwise)
resumen=as.data.frame(resumen$coefficients)

r=predict(glm_stepwise,test,type="response")

test3=cbind(test,as.vector(r))

table(test3$target,round(test3$`as.vector(r)`))

test3$`as.vector(r)`


estad=function(data,cutoff){
  data$`as.vector(r)`=ifelse(data$`as.vector(r)`<=cutoff,0,1)
  print(table(data$target,data$`as.vector(r)`))
  tabla=table(data$target,data$`as.vector(r)`)
  especificidad=tabla[2,2]/sum(tabla[2,])
  sensibilidad=tabla[1,1]/sum(tabla[1,])
  acuracy=(tabla[1,1]+tabla[2,2])/sum(tabla)
  precision=(tabla[1,1]/(tabla[1,1]+tabla[2,1]))
  negative_predicitve_value=(tabla[2,2]/(tabla[2,2]+tabla[1,2]))
  return=list(especificidad,sensibilidad,acuracy,precision,negative_predicitve_value)
}

especi=vector()
sensi=vector()
acura=vector()
precion=vector()
nega=vector()
for (i in seq(0.1,9,0.1)){
  print(i)
  data=estad(test3,i)
  especi=append(especi,data[[1]])
  sensi=append(sensi,data[[2]])
  acura=append(acura,data[[3]])
  precion=append(precion,data[[4]])
  nega=append(nega,data[[5]])
}

plot(seq(0.1,0.9,0.1),especi,type="l",col="blue")
lines(seq(0.1,0.9,0.1),sensi,col="red")
lines(seq(0.1,0.9,0.1),acura,col="green")
lines(seq(0.1,0.9,0.1),precion,col="black")
lines(seq(0.1,0.9,0.1),nega,colr="orange")

