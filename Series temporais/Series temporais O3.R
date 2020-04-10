##### Times series a6_poluica
https://cran.rstudio.com/web/packages/sweep/vignettes/SW01_Forecasting_Time_Series_Groups.html
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
# Carregando dados
data<- readr::read_csv("a6_poluicao.csv")


# Visualização
data %>% gather(Var,Valor,-DATA) %>%  ggplot(aes(x = DATA,y =Valor, color = Var)) + 
  geom_line() + 
  facet_wrap(~Var, scales = "free_y") 


# Filtrando uma categoria 
data_o3<- data %>% select(data=DATA,Qtd_o3 = o3)

#Séries A6 (Poluição): Emissão diária de poluentes na cidade de São Paulo, de 1 de janeiro de 31 de dezembro de 1997.
#(a) CO - gás carbônico

# Descrevendo mais a categoria 
Os dados foram coletados diariamente, por tanto teremos 365 registros para a quantidade de O3 na cidade de São Paulo
data_o3 %>% summary()

# Divisão de treinamento e teste
Como o objetivo de comparar modelos para uma melhor previsão do o3, vamos dividir a base de dados em treinamento e teste.

data_o3_train<-
  data_o3 %>%
    mutate(week = wday(data,label = T)# %>% as.factor()
                             ,Month = month(data,label = T) #%>% as.factor()
                             , Day = day(data)) %>%
      filter(Month <'set')
data_o3_test<-
  data_o3 %>%
    mutate(week = wday(data,label = T)# %>% as.factor()
                                 ,Month = month(data,label = T) #%>% as.factor()
                                 , Day = day(data)) %>%
      filter(Month >='set')
data_future<- expand.grid(data = max(data_o3$data)+1:90) %>% as.data.frame() %>% 
  mutate(week = wday(data,label = T),
         Month = month(data,label = T), #%>% as.factor()
         Day = day(data)) 

# Descritiva
#Analise por semana----
Analise por semana
data_o3_train %>% ggplot(aes(x=week,Qtd_o3)) + geom_boxplot()
data_o3_train %>% 
  group_by(week) %>% 
  summarise(Media = mean(Qtd_o3)
            ,Maximo = max(Qtd_o3) 
            ,Minimo = min(Qtd_o3) 
            ,Desvio = sd(Qtd_o3)
            ,N = n()
            ,ic2 = Media - 1.96*Desvio/N
            ,ic1 = Media + 1.96*Desvio/N) %>% select(week,ic1,ic2) %>% 
  gather(Ics,Var,-week) %>% ggplot(aes(week,Var,col = week))+geom_point()


#Analise por mes---- 
Analise por mes
data_o3_train %>% ggplot(aes(x=Month,Qtd_o3)) + geom_boxplot()

data_o3_train %>% 
  group_by(Month) %>% 
  summarise(Media = mean(Qtd_o3)
            ,Maximo = max(Qtd_o3) 
            ,Minimo = min(Qtd_o3) 
            ,Desvio = sd(Qtd_o3)
            ,N = n()
            ,ic2 = Media - 1.96*Desvio/N
            ,ic1 = Media + 1.96*Desvio/N) %>% select(Month,ic1,ic2) %>% 
  gather(Ics,Var,-Month) %>% ggplot(aes(Month,Var,col = Month))+geom_point()

#-----
Analise por Mes e semana
data_o3_train %>% ggplot(aes(x=week,Qtd_o3)) + geom_boxplot() + facet_wrap(~Month)

data_o3_train %>% 
  group_by(Month,week) %>% 
    summarise(Media = mean(Qtd_o3)
             ,Maximo = max(Qtd_o3) 
             ,Minimo = min(Qtd_o3) 
             ,Desvio = sd(Qtd_o3)
             ,N = n()
             ,ic2 = Media - 1.96*Desvio/N
             ,ic1 = Media + 1.96*Desvio/N) %>%  
              View()
#Modelo---- 
library(caret)
# train the GBM model
set.seed(7)
modelGbm <- train(Qtd_o3~., data=data_o3_train %>% select(-data), method="gbm",tuneLength = 1, verbose=FALSE)
# train the SVM model
set.seed(7)
modelSvm <- train(Qtd_o3~., data=data_o3_train %>% select(-data), method="logicBag",tuneLength = 1)
# train the rf model
set.seed(7)
modelo_rf <- train(Qtd_o3 ~ .,data=data_o3_train %>% select(-data) ,method = "rf",tuneLength = 10)
# train the ml model
set.seed(7)
modelo_ml <- train(Qtd_o3 ~ .,data=data_o3_train %>% select(-data) ,method = "mlpWeightDecayML",tuneLength = 10)
# train the tree model
set.seed(7)
modelo_tree <- train(Qtd_o3 ~ .,data=data_o3_train %>% select(-data) ,method = "rpart",tuneLength = 10)
# train the xgb model
set.seed(7)
modelo_xgblin <- train(Qtd_o3 ~ .,data=data_o3_train %>% select(-data) ,method = "xgbLinear",tuneLength = 1)

# lm
modelo_lm<-  train(Qtd_o3 ~ .,data=data_o3_train %>% select(-data) ,method = "lm")


# collect resamples
results <- resamples(list(GBM=modelGbm, SVM=modelSvm, RF = modelo_rf, ML=modelo_ml, TREE=modelo_tree, XGB=modelo_xgblin,Lm = modelo_lm ))
results %>% summary()
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

#---- Plots previsao
Forecast = lapply(list(modelo_xgblin,
                       modelGbm,
                       modelSvm,
                       modelo_rf,
                       modelo_ml,
                       modelo_tree
                       ),
                  predict, data_o3_test %>% select(-data,-Qtd_o3)) %>%
          data.frame();
colnames(Forecast)<-c('modelo_xgblin',
                      'modelGbm',
                      'modelSvm','modelo_rf',
                      'modelo_ml','modelo_tree')
Data_pred<- data_o3_test %>% select(data,Qtd_o3) %>% 
            cbind(Forecast) %>% gather(Var,Valor,-data)

Data_pred %>% ggplot(aes(data,Valor, col=Var))+geom_line()+facet_wrap(~Var)
#---- Previsao 
forecast_2<- predict(modelo_xgblin,data_future) %>% data.frame()
colnames(forecast_2)<-c("Qtd_o3")
Data_pred<-
  data_future %>%
  cbind(forecast_2) %>%
  select(data,Qtd_o3)%>% 
  rbind(data_o3) %>% 
  mutate(Flag = if_else(data>max(data_o3$data),"Previsão","Presente"))



Data_pred %>% ggplot(aes(data,Qtd_o3, col=Flag))+geom_line()
