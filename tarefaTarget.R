tarefa <- function(numero)
{
  fibonacci <- c(0, 1)
  i <-3
  while(5==5)
  {
    if(!is.numeric(numero)) return("por favor inserir apenas numeros  ") 
    novo <- fibonacci[i - 1] + fibonacci[i - 2]
    fibonacci <- c(fibonacci, novo)
    if(fibonacci[i] == numero ) return("o numero faz parte da sequencia de fibonacci ")
    if(fibonacci[i] > numero ) return("o numero não faz parte da sequencia de fibonacci " )
    i <-i +1      
  }
}  


tarefa()

sp <- 67836.43
rj <- 36678.66
mg <- 29229.88
es <- 27165.48
outros <- 19849.53
total <- sp + rj + mg + es + outros 

Porcentagem <-function(parte , total)
{
  return(parte/(total/100)  )
}

spPorcentagem <- Porcentagem(sp , total)
rjPorcentagem <- Porcentagem(RJ , total)
mgPorcentagem <-Porcentagem(MG , total)
esPorcentagem <-Porcentagem(ES , total)
oustrosPorcentagem <- Porcentagem(outros , total)
listaPorcentagens <- c(spPorcentagem,rjPorcentagem,mgPorcentagem,esPorcentagem,oustrosPorcentagem)
nomes <-c("sp","rj","mg","es","outros")
pie(listaPorcentagens,
    labels = paste(nomes," (",round(listaPorcentagens,2) ,"%",")",sep=""),
    main ="Faturamento mensal por estado "  )


Tarefa5 <- function(palavra)
{
  library(stringr)
  
  i = str_length(palavra)
  palavraInvertida <-""
  while(i > 0) 
  {
    palavraInvertida <-paste(palavraInvertida , str_sub(palavra,i,i), sep = "")
    i <- i -1
  }
  
  return(palavraInvertida)
}

install.packages("jsonlite")
library("jsonlite")
dados<- fromJSON("dados.json", flatten=TRUE)

min <- dados$valor[1] 
max <- dados$valor[1]
tamanho <- length(dados$dia)
media <- sum(dados$valor)/tamanho
i <-2 
diasAcimaDaMedia <- 0
while(i < tamanho)
{
  if(dados$valor[i] != 0)
  {
    
    if(dados$valor[i] > max) max <-  dados$valor[i]
    if(dados$valor[i] < min) min <-  dados$valor[i]
    if(dados$valor[i] < media) diasAcimaDaMedia <-  diasAcimaDaMedia + 1
    
  }
  i <- i + 1
} 

retdiasAcimaDaMedia <- paste(", Dias no mês em que o valor de faturamento diário foi superior à média mensal: ",diasAcimaDaMedia)
rerMax <- paste(", O maior valor de faturamento ocorrido em um dia do mês: ",max)
retMin <- paste("O menor valor de faturamento ocorrido em um dia do mês: ",min)

print(paste(retMin, rerMax,retdiasAcimaDaMedia))

