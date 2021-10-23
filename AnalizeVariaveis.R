data <- read.csv("./Database/DataFrame.csv")

InvestimentoD1 <- quantile(data$Investimento, probs = 1/3)
InvestimentoD2 <- quantile(data$Investimento, probs = 2/3)

PesquisaD1 <- quantile(data$Pesquisa, probs = 1/3)
PesquisaD2 <- quantile(data$Pesquisa, probs = 2/3)

linInvCom <- lm(formula = data$Computador ~data$Investimento)
plot(data$Investimento, data$Computador, type = 'n', main = "Computeres por Casa vs. Investimento em Pesquisa",
     xlab="Investimento no Setor de TI",ylab="Computadores Por Casa")
points(data$Investimento, data$Computador, col='blue')

linPesCom <- lm(formula = data$Computador ~data$Pesquisa)
plot(data$Pesquisa, data$Computador, type = 'n', main = "Computadores por Casa vs. Investimento em Pesquisa",
     xlab="Investimento em Pesquisa", ylab="Quantidade de Computadores por Casa")
points(data$Pesquisa, data$Computador, col='red')

linInvPes <- lm(formula = data$Computador ~ data$Investimento)
plot(data$Investimento, data$Pesquisa, type = 'n', main = "Computer by Home vs. Investiment",
     xlab="Invastimento em Pesquisa", ylab="Investimento no Setor de TI")
points(data$Investimento, data$Pesquisa, col='green')