## carregando os pacotes necessários

library(ggplot2)
library(plyr)
library(ROCR)
library(caret)

## abrindo a planilha

dados = read.csv("wage_train.csv", stringsAsFactors = TRUE)
colnames(dados)

sapply(dados, class)


##problema de classificação, a variavel yearly_wage é categorica

## simplificando os dados

d1 = dados[,-c(4,7,12,13)]
d1

summary(d1)

## limpando os NA

d1 = d1[rowSums(d1 == "?")==0, -c(7,10) , drop = FALSE]

## analises idade por salário

ggplot(d1, aes(x=yearly_wage, y=age, fill = yearly_wage)) + 
  geom_boxplot()

##analises para workclass

d1$workclass = as.factor(as.character(d1$workclass))
levels(d1$workclass)

plot(
  jitter(as.numeric(d1$workclass),0.5) ~ jitter(as.numeric(d1$yearly_wage), 0.5),
  data = d1,
  xlab = "salario",
  ylab = "workclass",
  pch = 19, 
  cex = 1, 
  bty = "n",
  col = rgb(180,0,180,30, maxColorValue = 255)
)

## grafico de porcentagens

#unindo categorias

count <- table(dados[adult$workclass == 'Government',]$yearly_wage)["<=50K"]
count <- c(count, table(dados[adult$workclass == 'Government',]$yearly_wage)[">50K"])
count <- c(count, table(dados[adult$workclass == 'Other/Unknown',]$yearly_wage)["<=50K"])
count <- c(count, table(dados[adult$workclass == 'Other/Unknown',]$yearly_wage)[">50K"])
count <- c(count, table(dados[adult$workclass == 'Private',]$yearly_wage)["<=50K"])
count <- c(count, table(dados[adult$workclass == 'Private',]$yearly_wage)[">50K"])
count <- c(count, table(dados[adult$workclass == 'Self-Employed',]$yearly_wage)["<=50K"])
count <- c(count, table(dados[adult$workclass == 'Self-Employed',]$yearly_wage)[">50K"])
count <- as.numeric(count)

# criando um data frame 
industry <- rep(levels(dados$workclass), each = 2)
income <- rep(c('<=50K', '>50K'), 4)
df <- data.frame(industry, yearly_wage, count)
df

df <- ddply(df, .(industry), transform, percent = count/sum(count) * 100)

df <- ddply(df, .(industry), transform, pos = (cumsum(count) - 0.5 * count))
df$label <- paste0(sprintf("%.0f", df$percent), "%")

ggplot(df, aes(x = industry, y = count, fill = yearly_wage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income by Industry')


## analise de salário por nível educacional

dados$education = as.factor(as.character(dados$education))

levels(dados$education)

plot(
  jitter(as.numeric(education),0.5) ~ jitter(as.numeric(yearly_wage), 0.5),
  data = dados,
  xlab = "salario",
  ylab = "escolaridade",
  pch = 19, 
  cex = 1, 
  bty = "n",
  col = rgb(180,0,180,30, maxColorValue = 255)
)

## grafico de porcentagens

# calculando as porcentagens

df1 <- ddply(df1, .(education_num), transform, percent = count/sum(count) * 100)

# formatando as legendas

df1 <- ddply(df1, .(education_num), transform, pos = (cumsum(count) - 0.5 * count))
df1$label <- paste0(sprintf("%.0f", df1$percent), "%")

df1$label[which(df1$percent < 5)] <- NA

# barplot dos grupos

ggplot(df1, aes(x = education_num, y = count, fill = yearly_wage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level with Years of Education')

## analise de ocupações

d1$occupation = as.factor(as.character(d1$occupation))

prop.table(table(d1$occupation, d1$yearly_wage)) * 100

## analise do sexo

dados$sex = as.factor(as.character(dados$sex))

levels(dados$sex)

plot(
  jitter(as.numeric(sex),0.5) ~ jitter(as.numeric(yearly_wage), 0.5),
  data = dados,
  xlab = "Salario",
  ylab = "Sexo",
  pch = 19, 
  cex = 1, 
  bty = "n",
  col = rgb(180,0,180,30, maxColorValue = 255)
)

## analise das horas trabalhadas

ggplot(dados, aes(x=yearly_wage, y=hours_per_week, fill = yearly_wage)) + 
  geom_boxplot()

##começando a modelagem
##convertendo as categorias <=50k e >50k em 0 e 1 para ajustar a distribuição binomial

d1$income_binary = ifelse(d1$yearly_wage == "<=50K", 0, 1)

smp <- floor(0.80 * nrow(d1))
train_ind <- sample(seq_len(nrow(d1)), size = smp)
train_dados <- d1[train_ind, ]
test_d1 <- d1[-train_ind, ]

## analise por etnia]

# calando as porcentagens 
df2 <- ddply(df2, .(race), transform, percent = count/sum(count) * 100)

# formatando as legendas
df2 <- ddply(df2, .(race), transform, pos = (cumsum(count) - 0.5 * count))
df2$label <- paste0(sprintf("%.0f", df2$percent), "%")

df2$label[df2$race == 'Other'] <- NA
df2$label[df2$race == 'Amer-Indian-Eskimo'] <- NA

# barplot 
ggplot(df2, aes(x = race, y = count, fill = yearly_wage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level by Race')



### criando modelo de regressão logaritimica

m1 <- glm(income_binary~., 
          data = train_dados[,-c(9)], family = binomial)

coeff =  data.frame(m1$coefficients)

summary(m1)


#### analisando a precisão do modelo

logit_model <- glm(income_binary~., 
                   data = train_dados [,-c(9)], family = binomial)

coeff =  data.frame(logit_model$coefficients)

summary(logit_model)

test_d1$income_logistic_predicted <- ifelse(predict(logit_model, test_d1,
                                                    type = "response") > 0.2, ">50K", "<=50K")

confusion_matrix = confusionMatrix(table(Predicted = test_d1$income_logistic_predicted, 
                                         Actual = test_d1$yearly_wage))

confusion_matrix