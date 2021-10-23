data <- read.csv("./Database/DataFrame.csv")

Computador <- data$Computador
Investimento <- data$Investimento
Pesquisa <- data$Pesquisa

linCBH <- lm(formula=Computador ~ Investimento)
rstudent(linCBH)
summary(linCBH)

plot(Investimento, Computador, type = 'n', main = "Computer by Home vs. Investiment")
points(Investimento, Computador, col='blue')
abline(coef(linCBH)[1], coef(linCBH)[2], col='red')

linCBH <- lm(formula=Computador ~ Pesquisa)
rstudent(linCBH)
summary(linCBH)

plot(Pesquisa, Computador, type = 'n', main = "Computer by Home vs. Investiment")
points(Pesquisa, Computador, col='blue')
abline(coef(linCBH)[1], coef(linCBH)[2], col='red')

linCBH <- lm(formula=Investimento ~ Pesquisa)
rstudent(linCBH)
summary(linCBH)

plot(Pesquisa, Investimento, type = 'n', main = "Computer by Home vs. Investiment")
points(Pesquisa, Investimento, col='blue')
abline(coef(linCBH)[1], coef(linCBH)[2], col='red')
