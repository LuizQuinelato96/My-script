# APLICACOA INICIACAO CIENTIFICA, DADOS NÃO PODEM SER DISPONIBILIZADOS 
# Modelos
library(caret)
library(readr)
library(dplyr)
dados.bruto <-  read_csv("C:/Users/User/Desktop/Luiz/adelmo-IC-TCC/Trabalho/Codigos/dados.csv", 
                         col_types = cols(agravaids = col_factor(levels = c()), 
                                          agravalcoo = col_factor(levels = c()), 
                                          agravdiabe = col_factor(levels = c()), 
                                          agravdoenc = col_factor(levels = c()), 
                                          agravoutra = col_factor(levels = c()), 
                                          bacilosc_e = col_factor(levels = c()), 
                                          benef_gov = col_factor(levels = c()), 
                                          cs_escol_n = col_factor(levels = c()), 
                                          cs_raca = col_factor(levels = c()), 
                                          cs_sexo = col_factor(levels = c()), 
                                          cultura_es = col_factor(levels = c()), 
                                          forma = col_factor(levels = c()), 
                                          hiv = col_factor(levels = c()), nu_comu_ex = col_factor(levels = c()), 
                                          pop_imig = col_factor(levels = c()), 
                                          pop_liber = col_factor(levels = c()), 
                                          pop_rua = col_factor(levels = c()), 
                                          pop_saude = col_factor(levels = c()), 
                                          raiox_tora = col_factor(levels = c()), 
                                          situa_ence = col_factor(levels = c()), 
                                          tratamento = col_factor(levels = c()), 
                                          tratsup_at = col_factor(levels = c())))
  #
#filter Es
dados.bruto<- dados.bruto %>% filter(sg_uf_not==32) 

dados.bruto<-dados.bruto[,-c(1,2,14,26,27)]


dados<-dados.bruto;remove(dados.bruto)
str(dados)
#View(dados) 
#####

##### Treino e teste
##### 
# Teste
set.seed(2019)
teste<-sample(1:dim(dados)[1],round(0.2*dim(dados)[1]))
Teste<-dados[teste,] # banco teste
dados<-dados[-teste,]
# Divis?o entre treinamento e teste
library(caTools)
set.seed(2019)
divisao = sample.split(dados$situa_ence, SplitRatio = 0.7)
dados.treino = subset(dados, divisao == TRUE) # banco treino
dados.validacao = subset(dados, divisao == FALSE) # banco treino
#####
##### Modelo SVM
######
TrainingParameters <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
SVModel <- train(make.names(situa_ence) ~ ., data = dados.treino,
                 method = 'svmLinear',
                 trControl= TrainingParameters,
                 tuneGrid = expand.grid(C=3**(-7:7)),metric="Kappa",
                 preProcess = c("pca","scale","center"),
                 na.action = na.omit
)

SVMPredictions <-predict(SVModel, dados.validacao,type = "prob")
myroc = pROC::roc(dados.validacao$situa_ence, as.vector(SVMPredictions[,2]))
plot(myroc, print.thres = "best")

library(ROCR)
perf_svm <-   prediction(SVMPredictions[,"2"],dados.validacao$situa_ence) %>% performance(measure='tpr', x.measure='fpr')


##adjust optimal cut-off threshold for class probabilities
ponto<-cbind(myroc[[4]],myroc[[3]],myroc[[2]]); colnames(ponto)<-c("Corte","Especificidade","Sensibilidade")
ponto<-ponto %>% data.frame %>% mutate(Dif=abs(Especificidade-Sensibilidade))
bestcut<-ponto[which.min(ponto$Dif),][[1]]
predCut = factor( ifelse(SVMPredictions[,2] > bestcut, 2, 1) )
resposta<-as.data.frame(dados.validacao[,17])
tabela<-table(predCut,resposta$situa_ence)
confusionMatrix(tabela)
# Create confusion matrix
cmSVM <-confusionMatrix(SVMPredictions, dados.validacao$situa_ence)
print(cmSVM)
importance <- varImp(SVModel, scale=FALSE)
plot(importance)
########




########## Arvore 
#######
TrainingParameters <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
model_dt <- train(make.names(situa_ence) ~ ., 
                  data=dados.treino,
                  trControl = TrainingParameters,
                  method = "rpart",metric="ROC",
                  tuneLength = 15)#,,metric="ROC")
pred_arvore <- predict(model_dt, dados.validacao,type="prob")
myroc = pROC::roc(dados.validacao$situa_ence, as.vector(pred_arvore[,2]))
plot(myroc, print.thres = "best")
library(ROCR)
perf_arvore<-   prediction(pred_arvore[,"X2"],dados.validacao$situa_ence) %>% performance(measure='tpr', x.measure='fpr')


##adjust optimal cut-off threshold for class probabilities
ponto<-cbind(myroc[[4]],myroc[[3]],myroc[[2]]); colnames(ponto)<-c("Corte","Especificidade","Sensibilidade")
ponto<-ponto %>% data.frame %>% mutate(Dif=abs(Especificidade-Sensibilidade))
bestcut<-ponto[which.min(ponto$Dif),][[1]]
predCut = factor( ifelse(pred_arvore[,2] > bestcut, 2, 1) )
resposta<-as.data.frame(dados.validacao[,17])
tabela<-table(predCut,resposta$situa_ence)
confusionMatrix(tabela)


#########


########## Random forest
#######
cvCtrl = trainControl(method = "cv",number = 4, classProbs = TRUE, summaryFunction = twoClassSummary)
tunegrid <- expand.grid(.mtry = (1:10))
 classifierRandomForest = train(make.names(situa_ence) ~ .,
                                data = dados.treino,
                                trControl = cvCtrl,
                                method = "rf",
                                metric="Kappa",
                                ntree=20,
                                tuneGrid = tunegrid)
 curClassifier = classifierRandomForest

 predRandom = predict(curClassifier, dados.validacao, type = "prob")
 myroc = pROC::roc(dados.validacao$situa_ence, as.vector(predRandom[,2]))
 plot(myroc, print.thres = "best")
 library(ROCR)
 perf_rf <-   prediction(predRandom[,"X2"],dados.validacao$situa_ence) %>% performance(measure='tpr', x.measure='fpr')
 
 ##adjust optimal cut-off threshold for class probabilities
 ponto<-cbind(myroc[[4]],myroc[[3]],myroc[[2]]); colnames(ponto)<-c("Corte","Especificidade","Sensibilidade")
ponto<-ponto %>% data.frame %>% mutate(Dif=abs(Especificidade-Sensibilidade))
  bestcut<-ponto[which.min(ponto$Dif),][[1]]
   predCut = factor( ifelse(predRandom$X2 > bestcut, 2, 1) )
resposta<-as.data.frame(dados.validacao[,17])
tabela<-table(predCut,resposta$situa_ence)
 confusionMatrix(tabela)
########

 
 ############## NEURAL NET ## so testei
 #######
 TrainingParameters <- trainControl(method = "repeatedcv", number = 5, repeats=5)
 
 NNModel <- train(make.names(situa_ence) ~ .,
                  data = dados.treino,
                  method = "nnet",
                  trControl= TrainingParameters,
                  preProcess=c("scale","center"),
                  na.action = na.omit
 )
 
 pred_nnmodel <- predict(NNModel, dados.validacao,type="prob")
 myroc <- pROC::roc(dados.validacao$situa_ence, as.vector(pred_nnmodel[,2]))
 plot(myroc, print.thres = "best")
 
 ##adjust optimal cut-off threshold for class probabilities
 ponto<-cbind(myroc[[4]],myroc[[3]],myroc[[2]]); colnames(ponto)<-c("Corte","Especificidade","Sensibilidade")
 ponto<-ponto %>% data.frame %>% mutate(Dif=Especificidade-Sensibilidade)
 bestcut<-ponto[which.min(ponto$Dif),][[1]]
 predCut = factor( ifelse(pred_arvore[,2] > bestcut, 2, 1) )
 resposta<-as.data.frame(dados.validacao[,17])
 tabela<-table(predCut,resposta$situa_ence)
 confusionMatrix(tabela)
 
      
 
 ####### NEURAL NET ELM  # so testei
 #######

 TrainingParameters <- trainControl(method = "cv", number = 5)
 NNModel_ran <- train(make.names(situa_ence) ~ .,
                  data = dados.treino,
                  method = 'avNNet', # multinom, ORFridge, 
                  trControl= TrainingParameters,
                  na.action = na.omit,
                  tuneLength=20 # tuneGrid = data.frame(nhid = 5, actfun = "sin") 
 )
 
 pred_nnmodel <- predict(NNModel_ran, dados.validacao,type="prob")
 myroc <- pROC::roc(dados.validacao$situa_ence, as.vector(pred_nnmodel[,2]))
 plot(myroc, print.thres = "best")
 
 ##adjust optimal cut-off threshold for class probabilities
 ponto<-cbind(myroc[[4]],myroc[[3]],myroc[[2]]); colnames(ponto)<-c("Corte","Especificidade","Sensibilidade")
 ponto<-ponto %>% data.frame %>% mutate(Dif=abs(Especificidade-Sensibilidade))
 bestcut<-ponto[which.min(ponto$Dif),][[1]]
 predCut = factor( ifelse(pred_nnmodel[,2] > bestcut, 2, 1) ) # 0.315
 resposta<-as.data.frame(dados.validacao[,17])
 tabela<-table(predCut,resposta$situa_ence)
 confusionMatrix(tabela)
 #######

  
 ###### KNN    k=5
 ###### 
 fitControl <- trainControl(## 10-fold CV
   method = "cv",
   number = 5)
 my_knn_model <- train(situa_ence ~ .,
                        data=dados.treino,
                       method = "knn",
                       trControl = fitControl,
                       preProcess = c("center","scale"),metric="Kappa",
                       tuneLength = 10  )#  or tunegrid=expand.grid(k=c(1,2,3,4))
 pred_knn <- predict(my_knn_model, dados.validacao,type="prob")
 myroc <- pROC::roc(dados.validacao$situa_ence, as.vector(pred_nnmodel[,2]))
 plot(myroc, print.thres = "best")
 library(ROCR)
 perf_knn <-   prediction(pred_knn[,"2"],dados.validacao$situa_ence) %>% performance(measure='tpr', x.measure='fpr')
 
 
 ##adjust optimal cut-off threshold for class probabilities
 ponto<-cbind(myroc[[4]],myroc[[3]],myroc[[2]]); colnames(ponto)<-c("Corte","Especificidade","Sensibilidade")
 ponto<-ponto %>% data.frame %>% mutate(Dif=abs(Especificidade-Sensibilidade))
 bestcut<-ponto[which.min(ponto$Dif),][[1]]
 predCut = factor( ifelse(pred_knn[,2] > bestcut, 2, 1) )
 resposta<-as.data.frame(dados.validacao[,17])
 tabela<-table(predCut,resposta$situa_ence)
 confusionMatrix(tabela)
 #####
 
 
 ##### Naive 
 #####
 fitControl <- trainControl(## 10-fold CV
   method = "cv",
   number = 3)
 search_grid <- expand.grid(
     usekernel = c(TRUE, FALSE),
     fL = 0:5,
     adjust = seq(0, 5, by = 1)
   )
 modelo_naive <- train(situa_ence ~ .,
                           data=dados.treino,
                           method = "nb",
                           trControl = fitControl,metric="Kappa",
                           #preProc = c("BoxCox", "center", "scale", "pca"),
                       tuneGrid = search_grid)# or tunegrid=expand.grid(k=c(1,2,3,4))
 pred_naive <- predict(modelo_naive, dados.validacao,type="prob")
 myroc <- pROC::roc(dados.validacao$situa_ence, as.vector(pred_naive[,2]))
 plot(myroc, print.thres = "best")
 library(ROCR)
 perf_naive <-   prediction(pred_naive[,"2"],dados.validacao$situa_ence) %>% performance(measure='tpr', x.measure='fpr')
 
 ponto<-cbind(myroc[[4]],myroc[[3]],myroc[[2]]); colnames(ponto)<-c("Corte","Especificidade","Sensibilidade")
 ponto<-ponto %>% data.frame %>% mutate(Dif=abs(Especificidade-Sensibilidade))
 bestcut<-ponto[which.min(ponto$Dif),][[1]]
 predCut = factor( ifelse(pred_naive[,2] > bestcut, 2, 1) )
 resposta<-as.data.frame(dados.validacao[,17])
 tabela<-table(predCut,resposta$situa_ence)
 confusionMatrix(tabela)
 
 
 data.frame(predicted=pred_naive[,2], actual=dados.validacao$situa_ence)  %>%
   ggplot(data=., aes(x=pred_naive[,2])) +
   geom_density(aes(fill=dados.validacao$situa_ence), alpha=0.5) +
   xlab('Probabilidade de ser classe 1') +ylab("Densidade")+
   scale_fill_discrete(name="Classes") + theme_economist()
 theme(legend.position=c(0.8,0.8))
 
 
 
 
##### 
##### Logistico
#####
 library(parallel)
 library(parallelMap) 
 parallelStartSocket(cpus = detectCores())
 
 fitControl <- trainControl(## 10-fold CV
   method = "cv",
   number = 3)
  modelo_logistico <- train(situa_ence ~ .,
                       data=dados.treino,
                       method = "plr",
                       trControl = fitControl,metric='Kappa',
                       tuneLength = 10)# or tunegrid=expand.grid(k=c(1,2,3,4))
   pred_logistico <- predict(modelo_logistico, dados.validacao,type="prob")
 myroc <- pROC::roc(dados.validacao$situa_ence, as.vector(pred_logistico[,2]))
 plot(myroc, print.thres = "best")
 library(ROCR)
 perf_logi <-   prediction(pred_logistico[,"2"],dados.validacao$situa_ence) %>% performance(measure='tpr', x.measure='fpr')

 
 
 ponto<-cbind(myroc[[4]],myroc[[3]],myroc[[2]]); colnames(ponto)<-c("Corte","Especificidade","Sensibilidade")
 ponto<-ponto %>% data.frame %>% mutate(Dif=abs(Especificidade-Sensibilidade))
 bestcut<-ponto[which.min(ponto$Dif),][[1]]
 predCut = factor( ifelse(pred_logistico[,2] > bestcut, 2, 1) )
  resposta<-as.data.frame(dados.validacao[,17])
 tabela<-table(predCut,resposta$situa_ence)
 confusionMatrix(tabela)
 
 ##### Plot Densidade
  
 data.frame(predicted=pred_logistico[,2], actual=dados.validacao$situa_ence)  %>%
   ggplot(data=., aes(x=pred_logistico[,2])) +
   geom_density(aes(fill=dados.validacao$situa_ence), alpha=0.5) +
   xlab('Probabilidade de ser classe 2') +ylab("Densidade")+
   scale_fill_discrete(name="Classes") 
 theme(legend.position=c(0.8,0.8))
 # classe 1 , probabilidade de pertencer a classe sabendo que ? 1
  #####
 
 
 
 
 ## ROC CURVE COM TODAS OS MODELOS (fazendo...))
 roc_nb <- data.frame(fpr=unlist(perf_naive@x.values), tpr=unlist(perf_naive@y.values))
 roc_nb$method <- "Naive bayes"
 
 roc_lr <- data.frame(fpr=unlist(perf_logi@x.values), tpr=unlist(perf_logi@y.values))
 roc_lr$method <- "Logistic regression"
 
 perf_arvore <- data.frame(fpr=unlist(perf_arvore@x.values), tpr=unlist(perf_arvore@y.values))
 perf_arvore$method <- "Decision tree"
 
 perf_knn <- data.frame(fpr=unlist(perf_knn@x.values), tpr=unlist(perf_knn@y.values))
 perf_knn$method <- "Knn"
 
 perf_rf <- data.frame(fpr=unlist(perf_rf@x.values), tpr=unlist(perf_rf@y.values))
 perf_rf$method <- "Random forest"
 
 perf_xgb <- data.frame(fpr=unlist(perf_xgb@x.values), tpr=unlist(perf_xgb@y.values))
 perf_xgb$method <- "Xgboost"
 
 
 perf_xgb
 
 
 rbind(roc_lr,roc_nb,perf_arvore,perf_knn,perf_rf,perf_xgb) %>%
   ggplot(data=., aes(x=fpr, y=tpr, linetype=method, color=method)) + 
   geom_line() +
   geom_abline(a=1, b=0, linetype=2) +
   scale_x_continuous(limits=c(0,1), labels = scales::percent) +
   scale_y_continuous(limits=c(0,1), labels = scales::percent) +
   theme(legend.position=c(0.8,0.3), legend.title=element_blank())+labs(y="Sensibility",x = "1 - Specificity ")
 
 
##########
 ################### XGBOOST
 library(xgboost)
 library(mltools)
 library(data.table)
 
 
 gnbust
######
##### x treino e y treino 
 x_train <- 
   dados.treino %>%
   select(-situa_ence) %>% 
   #mutate(situa_ence=as.numeric(situa_ence))%>%
   data.table() %>% one_hot() %>%
   as.matrix()
 y_train<-dados.treino$situa_ence
# y_train <- dados.treino %>% select(situa_ence) %>%mutate(situa_ence=as.factor(situa_ence)) # %>% data.frame()# %>% mutate(situa_ence=(if_else(situa_ence=='1',0,1))) 
# x_label<-label[x_train]
   x_train<-xgb.DMatrix(x_train)
 
   
##########   
   x_validacao <- 
     dados.validacao %>%
     select(-situa_ence) %>% 
     #mutate(situa_ence=as.numeric(situa_ence))%>%
     data.table() %>% one_hot() %>%
     as.matrix()
   y_validacao <- dados.validacao %>% select(situa_ence)# %>% mutate(situa_ence=(if_else(situa_ence=='1',0,1))) %>% as.matrix
   # x_label<-label[x_train]
  # x_validacao<-xgb.DMatrix(x_validacao)
   
   
   
   ##########   TESTE 
   x_teste <- 
     Teste %>%
     select(-situa_ence) %>% 
     #mutate(situa_ence=as.numeric(situa_ence))%>%
     data.table() %>% one_hot() %>%
     as.matrix()
   y_teste <- Teste %>% select(situa_ence)# %>% mutate(situa_ence=(if_else(situa_ence=='1',0,1))) %>% as.matrix
   # x_label<-label[x_train]
   # x_validacao<-xgb.DMatrix(x_validacao)
  
   xgb_trcontrol = trainControl(
     method = "cv",
     number = 5,  
     allowParallel = TRUE,
     verboseIter = FALSE,
     returnData = FALSE
   )
   xgbGrid <- expand.grid(nrounds = c(50,100,150,200,250),  # this is n_estimators in the python code above
                          max_depth = c(10, 15, 20, 25,30,40,80),
                          colsample_bytree = seq(0.2, 0.9, length.out = 2),
                          ## The values below are default values in the sklearn-api. 
                          eta = 0.1,
                          gamma=0,
                          min_child_weight = 1,
                          subsample = 1  )
   
   xgbGrid <- expand.grid(nrounds = c(50),  # this is n_estimators in the python code above
                          max_depth = c(10),
                          colsample_bytree = 0.2,
                          ## The values below are default values in the sklearn-api. 
                          eta = 0.1,
                          gamma=0,
                          min_child_weight = 1,
                          subsample = 1  )
   set.seed(0) 
   xgb_model = train(
     x_train, y_train,  
     trControl = xgb_trcontrol,
     tuneGrid = xgbGrid,
     method = "xgbTree"
   )
   
   The final values used for the model were nrounds = 100, max_depth = 40, eta = 0.1, gamma =
     0, colsample_bytree = 0.2, min_child_weight = 1 and subsample = 1.
   
   
   #### es 
   e nrounds = 50, max_depth = 10, eta
   = 0.1, gamma = 0, colsample_bytree = 0.2, min_child_weight = 1 and
   subsample = 1.
   80         0.9               200      0.8496923  0.3116344
 
     pred_xgb <- predict(xgb_model, x_validacao,type="prob")
   myroc <- pROC::roc(Teste$situa_ence, as.vector(pred_xgb[,2]))
   plot(myroc, print.thres = "best")
   library(ROCR)
   perf_xgb <-   prediction(pred_xgb[2],dados.validacao$situa_ence) %>% performance(measure='tpr', x.measure='fpr')
   
   ponto<-cbind(myroc[[4]],myroc[[3]],myroc[[2]]); colnames(ponto)<-c("Corte","Especificidade","Sensibilidade")
   ponto<-ponto %>% data.frame %>% mutate(Dif=abs(Especificidade-Sensibilidade))
   bestcut<-ponto[which.min(ponto$Dif),][[1]]
   predCut = factor( ifelse(pred_xgb[,2] > bestcut, 2, 1) )
   resposta<-as.data.frame(Teste[,17])
   tabela<-table(predCut,resposta$situa_ence)
   confusionMatrix(tabela)
   
   
   
   data.frame(predicted=pred_xgb[,2], actual=dados.validacao$situa_ence)  %>%
     ggplot(data=., aes(x=pred_xgb[,2])) +
     geom_density(aes(fill=dados.validacao$situa_ence), alpha=0.5) +
     xlab('Probabilidade de ser classe 2') +ylab("Densidade")+
     scale_fill_discrete(name="Classes") 
   theme(legend.position=c(0.8,0.8))
   
   
   
   