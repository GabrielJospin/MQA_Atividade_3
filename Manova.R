data <- read.csv("./Database/DataFrame.csv")
FTable <- read.csv("./Database/tabelaF.5per.csv")

#Analise das variaveis e divisão dos pontos
n <- length(data$Local)
GeralMean <- mean(data$Computador)
pValorMaximo <- FTable$X2[n-3]

InvestimentoD1 <- quantile(data$Investimento, probs = 1/3)
InvestimentoD2 <- quantile(data$Investimento, probs = 2/3)

PesquisaD1 <- quantile(data$Pesquisa, probs = 1/3)
PesquisaD2 <- quantile(data$Pesquisa, probs = 2/3)

#Variaveis usadas
Computador <- data$Computador
Investimento <- data$Investimento
Pesquisa <- data$Pesquisa

#Divizão das Variaveis
PoucoInvestimento <- data[data$Investimento < InvestimentoD1,]
lenPI <- length(PoucoInvestimento$Local)

AltoInvestimento <- data[data$Investimento > InvestimentoD2,]
lenAI <- length(AltoInvestimento$Local)

InvestimentoModerado <- data[data$Investimento > InvestimentoD1 & data$Investimento < InvestimentoD2, ]
lenIM <- length(InvestimentoModerado$Local)

##Erros Gerais

SQE <- sum((data$Computador - GeralMean)^2)
QME <- SQE/(9 * (n-1))
print(sprintf("SQE: %.3f", SQE))
print(sprintf("QME: %.3f", QME))

#VariaveisPrinciapias
meanPI <- mean(PoucoInvestimento$Computador)
meanAI <- mean(AltoInvestimento$Computador)
meanIM <- mean(InvestimentoModerado$Computador)

#SQTrat

SQTPI <- lenPI * (meanPI - GeralMean)^2
SQTAI <- lenAI * (meanAI - GeralMean)^2
SQTIM <- lenIM * (meanIM - GeralMean)^2
SQI <- SQTAI + SQTIM + SQTAI
QMI <- SQI/2
FI <- QMI/QME

print(sprintf("SQI: %.3f", SQI))
print(sprintf("QMI: %.3f", QMI))
print(sprintf("FI: %.3f", FI))

PoucaPesquisa <- data[data$Pesquisa < PesquisaD1,]
lenPP <- length(PoucaPesquisa$Local)

MuitaPesquisa <- data[data$Pesquisa > PesquisaD2,]
lenMP <- length(MuitaPesquisa$Local)

PesquisaModerada <- data[data$Pesquisa > PesquisaD1 & data$Pesquisa < PesquisaD2, ]
lenPM <- length(PesquisaModerada$Local)

#VariaveisPrinciapias
meanPP <- mean(PoucaPesquisa$Computador)
meanMP <- mean(MuitaPesquisa$Computador)
meanPM <- mean(PesquisaModerada$Computador)

#SQTrat

SQTPP <- lenPP * (meanPP - GeralMean)^2
SQTPM <- lenPM * (meanPM - GeralMean)^2
SQTMP <- lenMP * (meanMP - GeralMean)^2
SQP <- SQTPP + SQTMP + SQTMP
QMP <- SQP/2
FP <- QMP/QME

print(sprintf("SQP: %.3f", SQP))
print(sprintf("QMP: %.3f", QMP))
print(sprintf("FP: %.3f", FP))


PoucoInvestimentoXPoucaPesquisa <- filter(PoucoInvestimento, PoucoInvestimento$Local %in% PoucaPesquisa$Local)
PoucoInvestimentoXPesquisaModerada <- filter(PoucoInvestimento, PoucoInvestimento$Local %in% PesquisaModerada$Local)
PoucoInvestimentoXMuitaPesquisa <- filter(PoucoInvestimento, PoucoInvestimento$Local %in% MuitaPesquisa$Local)

AltoInvestimentoXPoucaPesquisa <- filter(AltoInvestimento, AltoInvestimento$Local %in% PoucaPesquisa$Local)
AltoInvestimentoXPesquisaModerada <- filter(AltoInvestimento, AltoInvestimento$Local %in% PesquisaModerada$Local)
AltoInvestimentoXMuitaPesquisa <- filter(AltoInvestimento, AltoInvestimento$Local %in% MuitaPesquisa$Local)

InvestimentoModeradoXPoucaPesquisa <- filter(InvestimentoModerado, InvestimentoModerado$Local %in% PoucaPesquisa$Local)
InvestimentoModeradoXPesquisaModerada <- filter(InvestimentoModerado, InvestimentoModerado$Local %in% PesquisaModerada$Local)
InvestimentoModeradoXMuitaPesquisa <- filter(InvestimentoModerado, InvestimentoModerado$Local %in% MuitaPesquisa$Local)

#VariaveisPrinciapias
meanPIPP <- mean(PoucoInvestimentoXPoucaPesquisa$Computador)
lenPIPP <- length(PoucoInvestimentoXPoucaPesquisa$Local)
meanPIPM <- mean(PoucoInvestimentoXPesquisaModerada$Computador)
lenPIPM <- length(PoucoInvestimentoXPesquisaModerada$Local)
meanPIMP <- mean(PoucoInvestimentoXMuitaPesquisa$Computador)
lenPIMP <- length(PoucoInvestimentoXMuitaPesquisa$Local)

meanAIPP <- mean(AltoInvestimentoXPoucaPesquisa$Computador)
lenAIPP <- length(AltoInvestimentoXPoucaPesquisa$Local)
meanAIPM <- mean(AltoInvestimentoXPesquisaModerada$Computador)
lenAIPM <- length(AltoInvestimentoXPesquisaModerada$Local)
meanAIMP <- mean(AltoInvestimentoXMuitaPesquisa$Computador)
lenAIMP <- length(AltoInvestimentoXMuitaPesquisa$Local)

meanIMPP <- mean(InvestimentoModeradoXPoucaPesquisa$Computador)
lenIMPP <- length(InvestimentoModeradoXPoucaPesquisa$Local)
meanIMPM <- mean(InvestimentoModeradoXPesquisaModerada$Computador)
lenIMPM <- length(InvestimentoModeradoXPesquisaModerada$Local)
meanIMMP <- mean(InvestimentoModeradoXMuitaPesquisa$Computador)
lenIMMP <- length(InvestimentoModeradoXMuitaPesquisa$Local)

#SQTrat

SQTPIPP <- lenPIPP * (meanPIPP - GeralMean)^2
SQTPIPM <- lenPIPM * (meanPIPM - GeralMean)^2
SQTPIMP <- lenPIMP * (meanPIMP - GeralMean)^2

SQTAIPP <- lenAIPP * (meanAIPP - GeralMean)^2
SQTAIPM <- lenAIPM * (meanAIPM - GeralMean)^2
SQTAIMP <- lenAIMP * (meanAIMP - GeralMean)^2

SQTIMPP <- lenIMPP * (meanIMPP - GeralMean)^2
SQTIMPM <- lenIMPM * (meanIMPM - GeralMean)^2
SQTIMMP <- lenIMMP * (meanIMMP - GeralMean)^2

SQPI <- SQTPIPP + SQTPIPM + SQTPIMP + SQTAIPP + SQTAIPM + SQTAIMP + SQTIMPP + SQTIMPM + SQTIMMP
QMPI <- SQPI/(2*(n-1))
FPI <- QMPI/QME

print(sprintf("SQPI: %.3f", SQPI))
print(sprintf("QMPI: %.3f", QMPI))
print(sprintf("FPI: %.3f", FPI))

pValorMaximoG <- FTable$X2[n-6]
print(sprintf("Valor máximo de F aceitavel: %.3f", pValorMaximoG))

if(FPI < pValorMaximoG){
  print(sprintf("%.3f < %.3f", FPI, pValorMaximoG))
  print("logo Não existe interação entre os Fatores Investimento e Pesquisa")

  if(FI > pValorMaximo){
    print(sprintf("%.3f > %.3f", FI, pValorMaximo))
    print("logo existe interação entre os Fatores Investimento e Computadores")

  }else{
    print(sprintf("%.3f < %.3f", FI, pValorMaximo))
    print("logo existe não interação entre os Fatores Investimento e Computadores")
  }

  if(FP > pValorMaximo){
    print(sprintf("%.3f > %.3f", FP, pValorMaximo))
    print("logo existe interação entre os Fatores Pesquisa e Computadores")
  }else{
    print(sprintf("%.3f < %.3f", FP, pValorMaximo))
    print("logo existe não interação entre os Fatores Pesquisa e Computadores")
  }

  }else{
    print(sprintf("%.3f > %.3f",FPI, pValorMaximoG))
    print("logo existe interação entre os Fatores Investimento e Pesquisa")
  }