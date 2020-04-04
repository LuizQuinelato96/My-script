## Leitura dos dados 
library(readr)
library(dplyr)
data_train <- readr::read_csv("train.csv")
data_train<- data_train %>% select(NU_INSCRICAO,CO_UF_RESIDENCIA,SG_UF_RESIDENCIA,NU_IDADE,TP_SEXO,
                                   TP_COR_RACA,TP_NACIONALIDADE,TP_ST_CONCLUSAO,TP_ANO_CONCLUIU,TP_ESCOLA,
                                   TP_ENSINO,IN_TREINEIRO,TP_DEPENDENCIA_ADM_ESC,IN_BAIXA_VISAO,IN_CEGUEIRA,IN_SURDEZ,IN_DISLEXIA,
                                   IN_DISCALCULIA,IN_SABATISTA,IN_GESTANTE,IN_IDOSO,TP_PRESENCA_CN,TP_PRESENCA_CH,TP_PRESENCA_LC,CO_PROVA_CN,
                                   CO_PROVA_CH,CO_PROVA_LC,CO_PROVA_MT,NU_NOTA_CN,NU_NOTA_CH,NU_NOTA_LC,TP_LINGUA,TP_STATUS_REDACAO,NU_NOTA_COMP1,
                                   NU_NOTA_COMP2,NU_NOTA_COMP3,NU_NOTA_COMP4,NU_NOTA_COMP5,NU_NOTA_REDACAO,Q001,Q002,Q006,Q024,Q025,Q026,Q027,Q047,NU_NOTA_MT) %>% 
                        
  select(-c( NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4, NU_NOTA_COMP5,
             "CO_PROVA_CN", "CO_PROVA_CH", "CO_PROVA_LC", "CO_PROVA_MT",NU_INSCRICAO,CO_UF_RESIDENCIA,
             IN_SURDEZ, IN_DISLEXIA, IN_DISCALCULIA, IN_SABATISTA, IN_GESTANTE, IN_IDOSO,  TP_PRESENCA_CN,
             TP_PRESENCA_CH, TP_PRESENCA_LC,IN_BAIXA_VISAO,IN_CEGUEIRA,TP_STATUS_REDACAO)) %>% as.data.frame()
  

# Alterando as types
data_train  %>% glimpse

data_train  %>% str()

# Tratando NA
data_train<-data_train %>%  mutate_at(vars(starts_with("NU_NOTA")),list(~tidyr::replace_na(.,0))) %>% 
  mutate_if(is.integer, list(~tidyr::replace_na(.,"Em branco"))) %>% 
  mutate_if(is.character, list(~tidyr::replace_na(.,"Em branco")))
 
# Data type
 data_train<-data_train %>% 
  mutate_at(vars(starts_with("TP_")),funs(factor)) %>%
  mutate_at(vars(starts_with("IN_")),funs(factor)) %>% 
  mutate(SG_UF_RESIDENCIA =as.factor(SG_UF_RESIDENCIA),
         NU_IDADE = as.numeric(NU_IDADE),
         "Q001" = as.factor(Q001)
         ,"Q002"  = as.factor(Q002)
         ,"Q006" = as.factor(Q006)
         ,"Q024" = as.factor(Q024)
         ,"Q025"  = as.factor(Q025)
         ,"Q026"  = as.factor(Q026)
         ,"Q027" = as.factor(Q027)
         ,"Q047"=as.factor(Q047)) #%>%   glimpse()


data_train %>% summary()

# Descritiva IDADE
data_train$NU_IDADE %>% table()
fit<-rpart(NU_NOTA_MT~
             NU_NOTA_CN+NU_NOTA_CH+NU_IDADE ,data = data_train,method="anova" )
plot(fit)


#Modelagem----
# MODELOS
library(caret)
fitControl <-  trainControl(method = "cv", number =3 , repeats = 2)
modelo_lm <- train(NU_NOTA_MT ~ .,
                      data=data_train ,
                      method = "lm",
                      #trControl = fitControl,
                      tuneLength = 4
                      )
#modelo_knn <- train(NU_NOTA_MT ~ .,
#                   data=data_train ,
#                   method = "knn",
#                   trControl = fitControl,
#                   tuneLength = 10
#)
modelo_rf <- train(NU_NOTA_MT ~ .,
                   data=data_train ,
                   method = "rf",
                  # trControl = fitControl,
                   tuneLength = 1
)
modelo_ml <- train(NU_NOTA_MT ~ .,
                   data=data_train ,
                   method = "mlpWeightDecayML",
                   #trControl = fitControl,
                   tuneLength = 2
)
modelo_tree <- train(NU_NOTA_MT ~ .,
                   data=data_train ,
                   method = "rpart",
                   trControl = fitControl,
                   tuneLength = 3
)
modelo_xgblin <- train(NU_NOTA_MT ~ .,
                        data=data_train ,
                        method = "xgbLinear",
                        trControl = fitControl,
                        tuneLength = 4
)



#Predicao ----


PRIMEIRA REGRA
 APENAS MODELAR OS CASOS QUE TP_PRESENCA_LC == 1 O RESTO A NOTA DE MATEMATICA SERA 0
  data_test <- readr::read_csv("test.csv")
dim(data_test)
# filtrando as alunos que  foram na prova de matematica e dando zero para as que não foram 
#data_test %>% group_by(TP_PRESENCA_LC) %>% count()
pred_1 = data_test %>% filter(TP_PRESENCA_LC != 1) %>% mutate(NU_NOTA_MT = 0) %>% select(NU_INSCRICAO,NU_NOTA_MT)
NU_ins<-data_test %>% filter(TP_PRESENCA_LC == 1) %>% select(NU_INSCRICAO)
data_test<-data_test %>% filter(TP_PRESENCA_LC == 1) %>%
  select(-c( NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4, NU_NOTA_COMP5,
            "CO_PROVA_CN", "CO_PROVA_CH", "CO_PROVA_LC", "CO_PROVA_MT",NU_INSCRICAO,CO_UF_RESIDENCIA,
            IN_SURDEZ, IN_DISLEXIA, IN_DISCALCULIA, IN_SABATISTA, IN_GESTANTE, IN_IDOSO,  TP_PRESENCA_CN,
            TP_PRESENCA_CH,TP_PRESENCA_LC,IN_BAIXA_VISAO,IN_CEGUEIRA,TP_STATUS_REDACAO))

#
data_test %>% glimpse()
data_test<-
  data_test %>%  mutate_at(vars(starts_with("NU_NOTA")),list(~tidyr::replace_na(.,0))) %>% 
  mutate_if(is.integer, list(~tidyr::replace_na(.,"Em branco"))) %>% 
  mutate_if(is.character, list(~tidyr::replace_na(.,"Em branco"))) %>% 
  mutate_at(vars(starts_with("TP_")),funs(factor)) %>%
  mutate_at(vars(starts_with("IN_")),funs(factor)) %>% 
  mutate_at(vars(starts_with("Q0_")),funs(factor)) %>% 
  mutate(SG_UF_RESIDENCIA =as.factor(SG_UF_RESIDENCIA),
         "Q001" = as.factor(Q001)
         ,"Q002"  = as.factor(Q002)
         ,"Q006" = as.factor(Q006)
         ,"Q024" = as.factor(Q024)
         ,"Q025"  = as.factor(Q025)
         ,"Q026"  = as.factor(Q026)
         ,"Q027" = as.factor(Q027)
         ,"Q047"=as.factor(Q047)
         ,NU_IDADE = as.numeric(NU_IDADE))# %>% glimpse
pred <- predict(modelo_xgblin, data_test)
pred<- pred %>% cbind(NU_ins) %>% data.frame()
colnames(pred)<-c("NU_NOTA_MT","NU_INSCRICAO")
pred_final<- pred %>% select(NU_INSCRICAO,NU_NOTA_MT) %>% rbind(pred_1)
pred_final %>% head()
write.csv(pred,file = "C:/Users/User/Desktop/Luiz/R/Desafios codenation/answer.csv",row.names = FALSE)
pred<-read.csv2("answer.csv")
pred$NU_INSCRICAO<-as.character(pred$NU_INSCRICAO)
pred$NU_NOTA_MT<-as.double(pred$NU_NOTA_MT)

pred %>% str()
data_train %>% select(NU_NOTA_MT,NU_INSCRICAO) %>% glimpse()
