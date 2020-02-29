funcao_rend <- function()
{
  tempo_mes = readline(prompt="Quantos anos investindo? "); tempo_mes = as.numeric(tempo_mes)*12
  aplicacao = as.numeric(readline(prompt = "Valor de aplicação mensal? "))
  taxa = as.numeric(readline(prompt = "Taxa de dividendos mensal em %: "))/100
  Total_investido = 0
  valor = 0
for (i in 2:tempo_mes)
{valor[i] <- (aplicacao + valor[i - 1]) * taxa + aplicacao + valor[i - 1]
Total_investido[i]<-Total_investido[i-1]+aplicacao

}
  rendimento_mes <- valor * taxa
  ano <- round(seq(1:tempo_mes) / 12, 2)
 dif<-round(valor,0)-Total_investido
 minha_idade<-ano+23
data<-cbind(Total_investido,round(valor,0),dif, round(rendimento_mes, 0), ano,minha_idade) 
colnames(data) <- c("Total investido",'Acumulado_Total','Lucro dos dividendos', "Rendimento por Mes", "Ano","minha_idade")
View(data)
}
funcao_rend()


