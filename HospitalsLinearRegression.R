### Data and packages import
library(pacman)
pacman:::p_load("tidyverse", "gridExtra", "knitr","openxlsx","readr","lmtest","leaps",
                "car")


setwd("C:/Users/bruno/Desktop/5 Semestre/Regressão/Remoto/Trabalho/pqp/Modelo rMarkdown")
dados <-  readxl::read_xlsx("Dados_trabalho_20201_.xlsx")
amostra1 <- readr::read_rds("amostra1.xls")
amostra2 <- readr::read_rds("amostra2.xls")


### Qualitative variables transformation
dados$X7 <- as.factor(dados$X7)
dados$X8 <- as.factor(dados$X8)
amostra1$X7 <- as.factor(amostra1$X7)
amostra1$X8 <- as.factor(amostra1$X8)
amostra2$X7 <- as.factor(amostra2$X7)
amostra2$X8 <- as.factor(amostra2$X8)

dados1 <- dados %>% 
  mutate(X7 = fct_recode(X7,
                         "Sim" = "1",
                         "Não" = "2"),
         X8 = fct_recode(X8,
                         "NE" = "1",
                         "NC" = "2",
                         "S" = "3",
                         "W" = "4"))

amostra1 <- amostra1 %>% 
  mutate(X7 = fct_recode(X7,
                         "Sim" = "1",
                         "Não" = "2"),
         X8 = fct_recode(X8,
                         "NE" = "1",
                         "NC" = "2",
                         "S" = "3",
                         "W" = "4"))

amostra2 <- amostra2 %>% 
  mutate(X7 = fct_recode(X7,
                         "Sim" = "1",
                         "Não" = "2"),
         X8 = fct_recode(X8,
                         "NE" = "1",
                         "NC" = "2",
                         "S" = "3",
                         "W" = "4"))


### First model Train split
banco.modelo1 <- amostra1 %>% 
  select(X10,X11,X8)


names(banco.modelo1) <- c("Y","X1","X2")
banco.modelo1$X2 <- as.character(banco.modelo1$X2)

a <- banco.modelo1$X2
a <- as.character(a)
a[-c(which(a == "NE"))] = 0
a[c(which(a == "NE"))] = 1

banco.modelo1$X21 <- as.numeric(a)    

a <- banco.modelo1$X2
a <- as.character(a)
a[-c(which(a == "NC"))] = 0
a[c(which(a == "NC"))] = 1

banco.modelo1$X22 <- as.numeric(a)    

a <- banco.modelo1$X2
a <- as.character(a)
a[-c(which(a == "S"))] = 0
a[c(which(a == "S"))] = 1
banco.modelo1$X23 <- as.numeric(a) 

banco.modelo1 <- banco.modelo1[,-3]

### First model Test split
banco.validacao1 <- amostra2 %>% 
  select(X10,X11,X8)


names(banco.validacao1) <- c("Y","X1","X2")
banco.validacao1$X2 <- as.character(banco.validacao1$X2)

a <- banco.validacao1$X2
a <- as.character(a)
a[-c(which(a == "NE"))] = 0
a[c(which(a == "NE"))] = 1

banco.validacao1$X21 <- as.numeric(a)    

a <- banco.validacao1$X2
a <- as.character(a)
a[-c(which(a == "NC"))] = 0
a[c(which(a == "NC"))] = 1

banco.validacao1$X22 <- as.numeric(a)    

a <- banco.validacao1$X2
a <- as.character(a)
a[-c(which(a == "S"))] = 0
a[c(which(a == "S"))] = 1
banco.validacao1$X23 <- as.numeric(a) 

banco.validacao1 <- banco.validacao1[,-3]

### Second model Train split
banco.modelo2 <- amostra1 %>% 
  select(X1,X2,X3,X6,X9,X10,X7,X8)

names(banco.modelo2) <- c("Y","X1","X2","X3","X4","X5","X6","X7")
banco.modelo2$X7 <- as.character(banco.modelo2$X7)

a <- banco.modelo2$X6
a <- as.character(a)
a[-c(which(a == "Sim"))] = 0
a[c(which(a == "Sim"))] = 1

banco.modelo2$X61 <- as.factor(a) 


a <- banco.modelo2$X7
a <- as.character(a)
a[-c(which(a == "NE"))] = 0
a[c(which(a == "NE"))] = 1

banco.modelo2$X71 <- as.factor(a)    

a <- banco.modelo2$X7
a <- as.character(a)
a[-c(which(a == "NC"))] = 0
a[c(which(a == "NC"))] = 1

banco.modelo2$X72 <- as.factor(a)    

a <- banco.modelo2$X7
a <- as.character(a)
a[-c(which(a == "S"))] = 0
a[c(which(a == "S"))] = 1
banco.modelo2$X73 <- as.factor(a) 

banco.modelo2 <- banco.modelo2[,c(-7,-8)]


### Second model Test split
banco.validacao2 <- amostra2 %>% 
  select(X1,X2,X3,X6,X9,X10,X7,X8)

names(banco.validacao2) <- c("Y","X1","X2","X3","X4","X5","X6","X7")
banco.validacao2$X7 <- as.character(banco.validacao2$X7)

a <- banco.validacao2$X6
a <- as.character(a)
a[-c(which(a == "Sim"))] = 0
a[c(which(a == "Sim"))] = 1

banco.validacao2$X61 <- as.factor(a) 


a <- banco.validacao2$X7
a <- as.character(a)
a[-c(which(a == "NE"))] = 0
a[c(which(a == "NE"))] = 1

banco.validacao2$X71 <- as.factor(a)    

a <- banco.validacao2$X7
a <- as.character(a)
a[-c(which(a == "NC"))] = 0
a[c(which(a == "NC"))] = 1

banco.validacao2$X72 <- as.factor(a)    

a <- banco.validacao2$X7
a <- as.character(a)
a[-c(which(a == "S"))] = 0
a[c(which(a == "S"))] = 1
banco.validacao2$X73 <- as.factor(a) 

banco.validacao2 <- banco.validacao2[,c(-7,-8)]


### Description table
codigo <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11")
nome <- c("Número de Identificação","Duração da Internação","Idade","Risco 
          de Infecção","Proporção de Culturas de Rotina","Proporção de Raio-X 
          de Tórax de Rotina","Número de leitos","Filiação a Escola de 
          Medicina","Região","Média diária de pacientes","Número de 
          enfermeiro(s) ","Facilidades e serviços disponíveis")

descricao <- c("1-113","Duração média da internação de todos os pacientes no 
hospital (em dias)","Idade média dos pacientes","Probabilidade média estimada 
               de adquirir infecção no hospital (em %)","Razão do número de
               culturas realizadas com relação ao número de pacientes sem 
               sinais ou sintomas de infeção adquirida no hospital, 
               vezes 100.","Razão do número de Raio-X de Tórax realizados com
               relação ao número
               de pacientes sem sinais ou sintomas de pneumonia, vezes 100."
               ,"Número médio de leitos no hospital durante o período de 
               estudo","1 – sim 2 – não","Região Geográfica, onde:
               1 – NE 2- NC 3 – S e 4 – W","Número médio de pacientes 
               no hospital por dia durante o período do estudo","Número
               médio de enfermeiros(as) de tempo-integral ou equivalente
               registrados e licenciados durante o período de estudo 
               ( número de tempos integrais+metade do número de tempo
               parcial)","% de 35 potenciais facilidades e serviços que são 
               fornecidos pelo hospital")

table <- data.frame(codigo,nome,descricao)
kable(table, col.names = c("Código       ","   Nome   ","Descrição"))


### Exploratory Analysis

## Variable X1 - Internation length

# Table

meanX1 = mean(dados$X1)
medianX1= median(dados$X1)
q1X1 = quantile(dados$X1, names=FALSE)[2]
q2X1 = quantile(dados$X1, names=FALSE)[3]
q3X1 = quantile(dados$X1, names=FALSE)[4]
q4X1 = quantile(dados$X1, names=FALSE)[5]
maxX1 = max(dados$X1)
minX1 = min(dados$X1)

varX1 = var(dados$X1)
sdX1 = sd(dados$X1)

d1X1 = quantile(dados$X1, seq(0, 1, 0.1), names=FALSE)[2]
d9X1 = quantile(dados$X1, seq(0, 1, 0.1), names=FALSE)[10]

skewX1 = 3*(meanX1 - medianX1)/sdX1
kurtX1 = (q3X1 - q1X1)/(2*(d9X1 - d1X1))
coef.varX1 = (sdX1/meanX1)*100

mX1 <- c(round(c(meanX1,minX1,q1X1,medianX1,q3X1,maxX1,varX1,sdX1,
                 skewX1,kurtX1,coef.varX1),2))
nX1 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo",
         "Variância","Desvio Padrão","Assimetria","Curtose",
         "Coeficiente de Variação(%)")
variX1 <- data.frame(Medidas = nX1, Valores = mX1)
kable(variX1,caption = "Medidas de posição, variabilidade, 
      assimetria e curtose")

# Freq table

freq_salarios <- table(cut(dados$X1,breaks = seq(6,24,4)))
kable(freq_salarios,col.names = c("Duração da Internação","Frequência"),
      caption = "Distribuição de frequência para Duração de Internação")

# Histogram

graf11 <- ggplot(dados,aes(x=X1))+
  geom_histogram(colour="white",fill="darkslategray",binwidth=1)+
  labs(x="Duração da Internação", y="Frequência") +
  theme_bw ()+
  theme(axis.title.y=element_text(colour="black",size=10),
        axis.title.x=element_text(colour="black",size=10),
        axis.text=element_text(colour="black",size=9.5) ,
        panel.border=element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(size=10,hjust=0.3,colour="black"))

# Boxplot
graf12 <- ggplot(dados,aes(x=factor(""), y=X1))+
  geom_boxplot(fill=c("darkslategray"),width=0.5)+
  stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")+
  labs(x="",y="Duração da Internação")+
  theme_bw()+
  theme (axis.title.y=element_text(colour="black",size=10),
         axis.title.x=element_text(colour="black",size=10),
         axis.text=element_text(colour="black",size=9.5),
         panel.border=element_blank(),
         axis.line.y=element_line(colour="black"),
         plot.title = element_text(size=10,hjust=0.3,colour="black"))

grid.arrange(graf11, graf12,ncol=2)

# QQ Plot
ggplot(dados, aes(sample = X1)) +
  stat_qq(colour = "darkslategray") +
  labs(x="",y="Duração da Internação")+
  stat_qq_line()+
  theme_bw()+
  theme (axis.title.y=element_text(colour="black",size=10),
         axis.title.x=element_text(colour="black",size=12),
         axis.text=element_text(colour="black",size=9.5),
         panel.border=element_blank(),
         axis.line.y=element_line(colour="black"),
         plot.title = element_text(size=10,hjust=0.3,colour="black"))

# Shapiros test

shapiro.test(dados$X1)


### Variable X2 - Age

# Table

meanX2 = mean(dados$X2)
medianX2= median(dados$X2)
q1X2 = quantile(dados$X2, names=FALSE)[2]
q2X2 = quantile(dados$X2, names=FALSE)[3]
q3X2 = quantile(dados$X2, names=FALSE)[4]
q4X2 = quantile(dados$X2, names=FALSE)[5]
maxX2 = max(dados$X2)
minX2 = min(dados$X2)

varX2 = var(dados$X2)
sdX2 = sd(dados$X2)

d1X2 = quantile(dados$X2, seq(0, 1, 0.1), names=FALSE)[2]
d9X2 = quantile(dados$X2, seq(0, 1, 0.1), names=FALSE)[10]

skewX2 = 3*(meanX2 - medianX2)/sdX2
kurtX2 = (q3X2 - q1X2)/(2*(d9X2 - d1X2))
coef.varX2 = (sdX2/meanX2)*100

mX2 <- c(round(c(meanX2,minX2,q1X2,medianX2,q3X2,maxX2,varX2,
                 sdX2,skewX2,kurtX2,coef.varX2),2))
nX2 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo",
         "Variância","Desvio Padrão","Assimetria","Curtose",
         "Coeficiente de Variação(%)")
variX2 <- data.frame(Medidas = nX2, Valores = mX2)
kable(variX2,caption = "Medidas de posição, variabilidade, 
      assimetria e curtose")

# Freq table

freq_salarios <- table(cut(dados$X2,breaks = seq(35,70,7)))
kable(freq_salarios,col.names = c("Idade","Frequência"),
      caption = "Distribuição de frequência para Idade")

# Histogram

graf21 <- (ggplot(dados,aes(x=X2))+
             geom_histogram(colour="white",fill="darkslategray",
                            binwidth=5)+
             labs(x="Idade", y="Frequência") +
             theme_bw ()+
             theme(axis.title.y=element_text(colour="black",size=10),
                   axis.title.x=element_text(colour="black",size=10),
                   axis.text=element_text(colour="black",size=9.5) ,
                   panel.border=element_blank(),
                   axis.line=element_line(colour="black"),
                   plot.title = element_text(size=10,hjust=0.3,colour="black")))

# Boxplot
graf22 <- (ggplot(dados,aes(x=factor(""), y=X2))+
             geom_boxplot(fill=c("darkslategray"),width=0.5)+
             stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")+
             labs(x="",y="Idade")+
             theme_bw()+
             theme (axis.title.y=element_text(colour="black",size=10),
                    axis.title.x=element_text(colour="black",size=10),
                    axis.text=element_text(colour="black",size=9.5),
                    panel.border=element_blank(),
                    axis.line.y=element_line(colour="black"),
                    plot.title = element_text(size=10,hjust=0.3,colour="black")))

grid.arrange(graf21, graf22,ncol=2)

### Variable X3 - Infection risk

# Table

meanX3 = mean(dados$X3)
medianX3= median(dados$X3)
q1X3 = quantile(dados$X3, names=FALSE)[2]
q2X3 = quantile(dados$X3, names=FALSE)[3]
q3X3 = quantile(dados$X3, names=FALSE)[4]
q4X3 = quantile(dados$X3, names=FALSE)[5]
maxX3 = max(dados$X3)
minX3 = min(dados$X3)

varX3 = var(dados$X3)
sdX3 = sd(dados$X3)

d1X3 = quantile(dados$X3, seq(0, 1, 0.1), names=FALSE)[2]
d9X3 = quantile(dados$X3, seq(0, 1, 0.1), names=FALSE)[10]

skewX3 = 3*(meanX3 - medianX3)/sdX3
kurtX3 = (q3X3 - q1X3)/(2*(d9X3 - d1X3))
coef.varX3 = (sdX3/meanX3)*100

mX3 <- c(round(c(meanX3,minX3,q1X3,medianX3,q3X3,maxX3,
                 varX3,sdX3,skewX3,kurtX3,coef.varX3),2))
nX3 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo",
         "Variância","Desvio Padrão","Assimetria","Curtose","Coeficiente de Variação(%)")
variX3 <- data.frame(Medidas = nX3, Valores = mX3)
kable(variX3,caption = "Medidas de posição, variabilidade, 
      assimetria e curtose")

# Freq table

freq_salarios <- table(cut(dados$X3,breaks = seq(1,10,2)))
kable(freq_salarios,col.names = c("Risco de Infecção","Frequência"),
      caption = "Distribuição de frequência para Risco de Infecção")

# Histogram

graf31 <- (ggplot(dados,aes(x=X3))+
             geom_histogram(colour="white",fill="darkslategray",binwidth=1)+
             labs(x="Risco de Infecção", y="Frequência") +
             theme_bw ()+
             theme(axis.title.y=element_text(colour="black",size=10),
                   axis.title.x=element_text(colour="black",size=10),
                   axis.text=element_text(colour="black",size=9.5) ,
                   panel.border=element_blank(),
                   axis.line=element_line(colour="black"),
                   plot.title = element_text(size=10,hjust=0.3,colour="black")))

# Boxplot
graf32 <- (ggplot(dados,aes(x=factor(""), y=X3))+
             geom_boxplot(fill=c("darkslategray"),width=0.5)+
             stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")+
             labs(x="",y="Risco de Infecção")+
             theme_bw()+
             theme (axis.title.y=element_text(colour="black",size=10),
                    axis.title.x=element_text(colour="black",size=10),
                    axis.text=element_text(colour="black",size=9.5),
                    panel.border=element_blank(),
                    axis.line.y=element_line(colour="black"),
                    plot.title = element_text(size=10,hjust=0.3,colour="black")))

grid.arrange(graf31, graf32,ncol=2)

### Variable X4 - Hemoculture proportion

# Table

meanX4 = mean(dados$X4)
medianX4= median(dados$X4)
q1X4 = quantile(dados$X4, names=FALSE)[2]
q2X4 = quantile(dados$X4, names=FALSE)[3]
q3X4 = quantile(dados$X4, names=FALSE)[4]
q4X4 = quantile(dados$X4, names=FALSE)[5]
maxX4 = max(dados$X4)
minX4 = min(dados$X4)

varX4 = var(dados$X4)
sdX4 = sd(dados$X4)

d1X4 = quantile(dados$X4, seq(0, 1, 0.1), names=FALSE)[2]
d9X4 = quantile(dados$X4, seq(0, 1, 0.1), names=FALSE)[10]

skewX4 = 3*(meanX4 - medianX4)/sdX4
kurtX4 = (q3X4 - q1X4)/(2*(d9X4 - d1X4))
coef.varX4 = (sdX4/meanX4)*100

mX4 <- c(round(c(meanX4,minX4,q1X4,medianX4,q3X4,maxX4,
                 varX4,sdX4,skewX4,kurtX4,coef.varX4),2))
nX4 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo",
         "Variância","Desvio Padrão","Assimetria","Curtose",
         "Coeficiente de Variação(%)")
variX4 <- data.frame(Medidas = nX4, Valores = mX4)
kable(variX4,caption = "Medidas de posição, variabilidade,
      assimetria e curtose")

# Freq table

freq_salarios <- table(cut(dados$X4,breaks = seq(1,70,10)))
kable(freq_salarios,col.names = c("Proporção de 
                                  Culturas de Rotina","Frequência"),
      caption = "Distribuição de frequência para Risco da 
      Proporção de Culturas de Rotina")

# Histogram

graf41 <- (ggplot(dados,aes(x=X4))+
             geom_histogram(colour="white",fill="darkslategray",binwidth=5)+
             labs(x="Proporção de Culturas de Rotina", y="Frequência") +
             theme_bw ()+
             theme(axis.title.y=element_text(colour="black",size=10),
                   axis.title.x=element_text(colour="black",size=10),
                   axis.text=element_text(colour="black",size=9.5) ,
                   panel.border=element_blank(),
                   axis.line=element_line(colour="black"),
                   plot.title = element_text(size=10,hjust=0.3,colour="black")))

# Boxplot
graf42 <- (ggplot(dados,aes(x=factor(""), y=X4))+
             geom_boxplot(fill=c("darkslategray"),width=0.5)+
             stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")+
             labs(x="",y="Proporção de Culturas de Rotina")+
             theme_bw()+
             theme (axis.title.y=element_text(colour="black",size=10),
                    axis.title.x=element_text(colour="black",size=10),
                    axis.text=element_text(colour="black",size=9.5),
                    panel.border=element_blank(),
                    axis.line.y=element_line(colour="black"),
                    plot.title = element_text(size=10,hjust=0.3,colour="black")))

grid.arrange(graf41, graf42,ncol=2)

### Variable X5 - Thorax X-rays proportion

# Table

meanX5 = mean(dados$X5)
medianX5= median(dados$X5)
q1X5 = quantile(dados$X5, names=FALSE)[2]
q2X5 = quantile(dados$X5, names=FALSE)[3]
q3X5 = quantile(dados$X5, names=FALSE)[4]
q4X5 = quantile(dados$X5, names=FALSE)[5]
maxX5 = max(dados$X5)
minX5 = min(dados$X5)

varX5 = var(dados$X5)
sdX5 = sd(dados$X5)

d1X5 = quantile(dados$X5, seq(0, 1, 0.1), names=FALSE)[2]
d9X5 = quantile(dados$X5, seq(0, 1, 0.1), names=FALSE)[10]

skewX5 = 3*(meanX5 - medianX5)/sdX5
kurtX5 = (q3X5 - q1X5)/(2*(d9X5 - d1X5))
coef.varX5 = (sdX5/meanX5)*100

mX5 <- c(round(c(meanX5,minX5,q1X5,medianX5,q3X5,maxX5,
                 varX5,sdX5,skewX5,kurtX5,coef.varX5),2))
nX5 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo",
         "Variância","Desvio Padrão","Assimetria",
         "Curtose","Coeficiente de Variação(%)")
variX5 <- data.frame(Medidas = nX5, Valores = mX5)
kable(variX5,caption = "Medidas de posição, variabilidade, 
      assimetria e curtose")

# Freq table

freq_salarios <- table(cut(dados$X5,breaks = seq(35,140,15)))
kable(freq_salarios,
      col.names = c("Proporção de Raio - X de Tórax de Rotina",
                    "Frequência"),
      caption = "Distribuição de frequência para Proporção de Raio - X 
      de Tórax de Rotina")

# Histogram

graf51 <- (ggplot(dados,aes(x=X5))+
             geom_histogram(colour="white",fill="darkslategray",binwidth=14)+
             labs(x="Proporção de Raio - X de Tórax de Rotina", y="Frequência") +
             theme_bw ()+
             theme(axis.title.y=element_text(colour="black",size=10),
                   axis.title.x=element_text(colour="black",size=10),
                   axis.text=element_text(colour="black",size=9.5) ,
                   panel.border=element_blank(),
                   axis.line=element_line(colour="black"),
                   plot.title = element_text(size=10,hjust=0.3,colour="black")))


# Boxplot
graf52 <- (ggplot(dados,aes(x=factor(""), y=X5))+
             geom_boxplot(fill=c("darkslategray"),width=0.5)+
             stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")+
             labs(x="",y="Proporção de Raio - X de Tórax de Rotina")+
             theme_bw()+
             theme (axis.title.y=element_text(colour="black",size=10),
                    axis.title.x=element_text(colour="black",size=10),
                    axis.text=element_text(colour="black",size=9.5),
                    panel.border=element_blank(),
                    axis.line.y=element_line(colour="black"),
                    plot.title = element_text(size=10,hjust=0.3,colour="black")))

grid.arrange(graf51, graf52,ncol=2)


### Variable X6 - Hospital beds

# Table

meanX6 = mean(dados$X6)
medianX6= median(dados$X6)
q1X6 = quantile(dados$X6, names=FALSE)[2]
q2X6 = quantile(dados$X6, names=FALSE)[3]
q3X6 = quantile(dados$X6, names=FALSE)[4]
q4X6 = quantile(dados$X6, names=FALSE)[5]
maxX6 = max(dados$X6)
minX6 = min(dados$X6)

varX6 = var(dados$X6)
sdX6 = sd(dados$X6)

d1X6 = quantile(dados$X6, seq(0, 1, 0.1), names=FALSE)[2]
d9X6 = quantile(dados$X6, seq(0, 1, 0.1), names=FALSE)[10]

skewX6 = 3*(meanX6 - medianX6)/sdX6
kurtX6 = (q3X6 - q1X6)/(2*(d9X6 - d1X6))
coef.varX6 = (sdX6/meanX6)*100

mX6 <- c(round(c(meanX6,minX6,q1X6,medianX6,q3X6,maxX6,
                 varX6,sdX6,skewX6,kurtX6,coef.varX6),2))
nX6 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo",
         "Variância","Desvio Padrão","Assimetria","Curtose",
         "Coeficiente de Variação(%)")
variX6 <- data.frame(Medidas = nX6, Valores = mX6)
kable(variX6,caption = "Medidas de posição, variabilidade, 
      assimetria e curtose")

# Freq table

freq_salarios <- table(cut(dados$X6,breaks = seq(25,900,125)))
kable(freq_salarios,col.names = c("Proporção de Número de leitos",
                                  "Frequência"),
      caption = "Distribuição de frequência para número de leitos")

# Histogram

graf61 <- (ggplot(dados,aes(x=X6))+
             geom_histogram(colour="white",
                            fill="darkslategray",binwidth=80)+
             labs(x="Proporção de número de leitos", y="Frequência") +
             theme_bw ()+
             theme(axis.title.y=element_text(colour="black",size=10),
                   axis.title.x=element_text(colour="black",size=10),
                   axis.text=element_text(colour="black",size=9.5) ,
                   panel.border=element_blank(),
                   axis.line=element_line(colour="black"),
                   plot.title = element_text(size=10,hjust=0.3,colour="black")))


# Boxplot
graf62 <- (ggplot(dados,aes(x=factor(""), y=X6))+
             geom_boxplot(fill=c("darkslategray"),width=0.5)+
             stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")+
             labs(x="",y="Proporção de número de leitos")+
             theme_bw()+
             theme (axis.title.y=element_text(colour="black",size=10),
                    axis.title.x=element_text(colour="black",size=10),
                    axis.text=element_text(colour="black",size=9.5),
                    panel.border=element_blank(),
                    axis.line.y=element_line(colour="black"),
                    plot.title = element_text(size=10,hjust=0.3,colour="black")))

grid.arrange(graf61, graf62,ncol=2)

### Variable X7 - Filiação a Escola de Medicina

# Freq table

freq1 <- sum(dados1$X7 == "Sim")
freq2 <- sum(dados1$X7 == "Não")
perc1 <- round(freq1/length(dados1$X7),2)
perc2 <- round(freq2/length(dados1$X7),2)

freq.X7 <- c(freq1,freq2)
porc.X7 <- c(perc1,perc2)
x7 <- c("Sim", "Não")
tabelax7 <- data.frame(x7,freq.X7,porc.X7)
kable(tabelax7, col.names = c("X7", "Frequência","Porcentagem"))

# Bar plot 
ggplot(dados1, aes(x = X7)) + 
  geom_bar( fill=c("#69d171","#d96464")) + 
  labs(x="Filiação a Escola de Medicina" , y="Frequência") + 
  theme_bw() +
  theme( axis.title.y=element_text(colour="black" , size=12) , 
         axis.title.x = element_text(colour="black" , size=12) , 
         axis.text = element_text(colour = "black" , size=9.5) , 
         panel.border = element_blank () , 
         axis.line = element_line(colour = "black"),
         plot.title = element_text(size=12,hjust=0.3,
                                   colour="black"))

### Variable X8 - Region

# Freq table

freq11 <- sum(dados1$X8 == "NE");perc11 <- round(freq11/length(dados1$X8),2)
freq22 <- sum(dados1$X8 == "NC");perc22 <- round(freq22/length(dados1$X8),2)
freq33 <- sum(dados1$X8 == "S");perc33 <- round(freq33/length(dados1$X8),2)
freq44 <- sum(dados1$X8 == "W");perc44 <- round(freq44/length(dados1$X8),2)


freq.X8 <- c(freq11,freq22,freq33,freq44)
porc.X8 <- c(perc11,perc22,perc33,perc44)
x8 <- c("NE", "NC","S","W")
tabelax8 <- data.frame(x8,freq.X8,porc.X8)

kable(tabelax8, col.names = c("X8", "Frequência","Porcentagem"))

# Bar plot 
ggplot(dados1, aes(x = X8)) + 
  geom_bar( fill=c("#69d171","#d96464","#99aae8","#d09ff5")) + 
  labs(x="Região" , y="Frequência") + 
  theme_bw() +
  theme( axis.title.y=element_text(colour="black" , size=12) , 
         axis.title.x = element_text(colour="black" , size=12) , 
         axis.text = element_text(colour = "black" , size=9.5) , 
         panel.border = element_blank () , 
         axis.line = element_line(colour = "black"),
         plot.title = element_text(size=12,hjust=0.3,
                                   colour="black"))

### Variable X9 - Daily patient mean

# Table

meanX9 = mean(dados$X9)
medianX9= median(dados$X9)
q1X9 = quantile(dados$X9, names=FALSE)[2]
q2X9 = quantile(dados$X9, names=FALSE)[3]
q3X9 = quantile(dados$X9, names=FALSE)[4]
q4X9 = quantile(dados$X9, names=FALSE)[5]
maxX9 = max(dados$X9)
minX9 = min(dados$X9)

varX9 = var(dados$X9)
sdX9 = sd(dados$X9)

d1X9 = quantile(dados$X9, seq(0, 1, 0.1), names=FALSE)[2]
d9X9 = quantile(dados$X9, seq(0, 1, 0.1), names=FALSE)[10]

skewX9 = 3*(meanX9 - medianX9)/sdX9
kurtX9 = (q3X9 - q1X9)/(2*(d9X9 - d1X9))
coef.varX9 = (sdX9/meanX9)*100

mX9 <- c(round(c(meanX9,minX9,q1X9,medianX9,q3X9,maxX9,
                 varX9,sdX9,skewX9,kurtX9,coef.varX9),2))
nX9 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo",
         "Variância","Desvio Padrão","Assimetria","Curtose",
         "Coeficiente de Variação(%)")
variX9 <- data.frame(Medidas = nX9, Valores = mX9)
kable(variX9,caption = "Medidas de posição, variabilidade, 
      assimetria e curtose")

# Freq table

freq_salarios <- table(cut(dados$X9,breaks = seq(20,820,100)))
kable(freq_salarios,col.names = c("Proporção de Média diárias 
                                  de pacientes","Frequência"),
      caption = "Distribuição de frequência para média diárias
      de pacientes")

# Histogram

graf91 <- (ggplot(dados,aes(x=X9))+
             geom_histogram(colour="white",fill="darkslategray",binwidth=100)+
             labs(x="Proporção de média diárias de pacientes", y="Frequência") +
             theme_bw ()+
             theme(axis.title.y=element_text(colour="black",size=10),
                   axis.title.x=element_text(colour="black",size=10),
                   axis.text=element_text(colour="black",size=9.5) ,
                   panel.border=element_blank(),
                   axis.line=element_line(colour="black"),
                   plot.title = element_text(size=10,hjust=0.3,colour="black")))


# Boxplot
graf92 <- (ggplot(dados,aes(x=factor(""), y=X9))+
             geom_boxplot(fill=c("darkslategray"),width=0.5)+
             stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")+
             labs(x="",y="Proporção de média diárias de pacientes")+
             theme_bw()+
             theme (axis.title.y=element_text(colour="black",size=10),
                    axis.title.x=element_text(colour="black",size=10),
                    axis.text=element_text(colour="black",size=9.5),
                    panel.border=element_blank(),
                    axis.line.y=element_line(colour="black"),
                    plot.title = element_text(size=10,hjust=0.3,colour="black")))

grid.arrange(graf91, graf92,ncol=2)

### Variable X10 - Number of nurses

# Table

meanX10 = mean(dados$X10)
medianX10= median(dados$X10)
q1X10 = quantile(dados$X10, names=FALSE)[2]
q2X10 = quantile(dados$X10, names=FALSE)[3]
q3X10 = quantile(dados$X10, names=FALSE)[4]
q4X10 = quantile(dados$X10, names=FALSE)[5]
maxX10 = max(dados$X10)
minX10 = min(dados$X10)

varX10 = var(dados$X10)
sdX10 = sd(dados$X10)

d1X10 = quantile(dados$X10, seq(0, 1, 0.1), names=FALSE)[2]
d9X10 = quantile(dados$X10, seq(0, 1, 0.1), names=FALSE)[10]

skewX10 = 3*(meanX10 - medianX10)/sdX10
kurtX10 = (q3X10 - q1X10)/(2*(d9X10 - d1X10))
coef.varX10 = (sdX10/meanX10)*100

mX10 <- c(round(c(meanX10,minX10,q1X10,medianX10,q3X10,maxX10,
                  varX10,sdX10,skewX10,kurtX10,coef.varX10),2))
nX10 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo",
          "Variância","Desvio Padrão","Assimetria","Curtose",
          "Coeficiente de Variação(%)")
variX10 <- data.frame(Medidas = nX10, Valores = mX10)
kable(variX10,caption = "Medidas de posição, variabilidade, 
      assimetria e curtose")

# Freq table

freq_salarios <- table(cut(dados$X10,breaks = seq(10,710,100)))
kable(freq_salarios,col.names = c("Proporção de Número de 
                                  enfermeiro(s)","Frequência"),
      caption = "Distribuição de frequência para número de enfermeiro(s)")
# Histogram

graf101 <- (ggplot(dados,aes(x=X10))+
              geom_histogram(colour="white",fill="darkslategray",
                             binwidth=100)+
              labs(x="Proporção de número de enfermeiro(s)", y="Frequência") +
              theme_bw ()+
              theme(axis.title.y=element_text(colour="black",size=10),
                    axis.title.x=element_text(colour="black",size=10),
                    axis.text=element_text(colour="black",size=9.5) ,
                    panel.border=element_blank(),
                    axis.line=element_line(colour="black"),
                    plot.title = element_text(size=10,hjust=0.3,colour="black")))

# Boxplot
graf102 <- (ggplot(dados,aes(x=factor(""), y=X10))+
              geom_boxplot(fill=c("darkslategray"),width=0.5)+
              stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")+
              labs(x="",y="Proporção de número de enfermeiro(s)")+
              theme_bw()+
              theme (axis.title.y=element_text(colour="black",size=10),
                     axis.title.x=element_text(colour="black",size=10),
                     axis.text=element_text(colour="black",size=9.5),
                     panel.border=element_blank(),
                     axis.line.y=element_line(colour="black"),
                     plot.title = element_text(size=10,hjust=0.3,colour="black")))

grid.arrange(graf101, graf102,ncol=2)

# QQ Plot
ggplot(dados, aes(sample = X10)) +
  stat_qq(colour = "darkslategray") +
  labs(x="",y="Número de enfermeiro(s)")+
  stat_qq_line()+
  theme_bw()+
  theme (axis.title.y=element_text(colour="black",size=10),
         axis.title.x=element_text(colour="black",size=12),
         axis.text=element_text(colour="black",size=9.5),
         panel.border=element_blank(),
         axis.line.y=element_line(colour="black"),
         plot.title = element_text(size=10,hjust=0.3,colour="black"))

# Shapiros test

shapiro.test(dados$X10)

### Variable X11 - Available services and resources

# Table

meanX11 = mean(dados$X11)
medianX11= median(dados$X11)
q1X11 = quantile(dados$X11, names=FALSE)[2]
q2X11 = quantile(dados$X11, names=FALSE)[3]
q3X11 = quantile(dados$X11, names=FALSE)[4]
q4X11 = quantile(dados$X11, names=FALSE)[5]
maxX11 = max(dados$X11)
minX11 = min(dados$X11)

varX11 = var(dados$X11)
sdX11 = sd(dados$X11)

d1X11 = quantile(dados$X11, seq(0, 1, 0.1), names=FALSE)[2]
d9X11 = quantile(dados$X11, seq(0, 1, 0.1), names=FALSE)[10]

skewX11 = 3*(meanX11 - medianX11)/sdX11
kurtX11 = (q3X11 - q1X11)/(2*(d9X11 - d1X11))
coef.varX11 = (sdX11/meanX11)*100

mX11 <- c(round(c(meanX11,minX11,q1X11,medianX11,q3X11,maxX11,
                  varX11,sdX11,skewX11,kurtX11,coef.varX11),2))
nX11 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo",
          "Variância","Desvio Padrão","Assimetria","Curtose",
          "Coeficiente de Variação(%)")
variX11 <- data.frame(Medidas = nX11, Valores = mX11)
kable(variX11,caption = "Medidas de posição, variabilidade, 
      assimetria e curtose")

# Freq table

freq_salarios <- table(cut(dados$X11,breaks = seq(5,85,10)))
kable(freq_salarios,col.names = c("Proporção de facilidades e serviços 
                                  disponíveis","Frequência"),
      caption = "Distribuição de frequência para facilidades 
      e serviços disponíveis")

# Histogram

graf111 <- (ggplot(dados,aes(x=X11))+
              geom_histogram(colour="white",fill="darkslategray",
                             binwidth=10)+
              labs(x="Proporção de facilidades e serviços disponíveis", y="Frequência")+
              theme_bw ()+
              theme(axis.title.y=element_text(colour="black",size=10),
                    axis.title.x=element_text(colour="black",size=9),
                    axis.text=element_text(colour="black",size=9.5) ,
                    panel.border=element_blank(),
                    axis.line=element_line(colour="black"),
                    plot.title = element_text(size=10,hjust=0.3,colour="black")))


# Boxplot
graf112 <- (ggplot(dados,aes(x=factor(""), y=X11))+
              geom_boxplot(fill=c("darkslategray"),width=0.5)+
              stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")+
              labs(x="",y="Proporção de facilidades e serviços disponíveis")+
              theme_bw()+
              theme (axis.title.y=element_text(colour="black",size=9),
                     axis.title.x=element_text(colour="black",size=10),
                     axis.text=element_text(colour="black",size=9.5),
                     panel.border=element_blank(),
                     axis.line.y=element_line(colour="black"),
                     plot.title = element_text(size=10,hjust=0.3,colour="black")))

grid.arrange(graf111, graf112,ncol=2)

## Multivariate

## Model 1

# Region x Nurses

# Boxplot
ggplot(dados, aes(x = X8, group = X8, y = X10))+
  geom_boxplot(fill = c("#69d171","#d96464","#99aae8","#d09ff5"))+
  theme_minimal()+
  xlab("Região")+
  ylab("Número de Enfermeiros")

# Freq table

x <- dados %>%
  group_by(X8) %>%
  summarise("Freq. Observada" = sum(X10)) %>%
  mutate("Freq. Esperada" = sum(dados$X10)*0.25) %>%
  mutate(Região = fct_recode(X8, "NE" = "1", "NC" = "2",
                             "S" = "3", "W" = "4"))

x <- data.frame(x$Região, x$`Freq. Observada`, x$`Freq. Esperada`)
names(x) <- c("Região", "Freq. Observada", "Freq. Esperada")

kable(x, caption = "Tabela de Freq. do Número de Enfermeiros por Região")

# Services x Nurses

# Scatterplot

ggplot(dados, aes(X11, X10))+
  geom_point(color = "darkslategray")+
  geom_smooth(se = F, color = "black")+
  theme_minimal()+
  xlab("Facilidades e Serviços disponíveis (%)")+
  ylab("Número de Enfermeiros")

# Table

x <- data.frame(round(cor(dados$X11, dados$X10, 
                          method = "spearman"), 3),
                round(cor(amostra1$X11, amostra1$X10,
                          method = "spearman"), 3),
                round(cor(amostra2$X11, amostra2$X10, 
                          method = "spearman"), 3))
names(x) <- c("Dados Completos", "Amostra 1", "Amostra 2")

kable(x)


## Model 2

# Internation length X Age

# Scatterplot

ggplot(dados, aes(X2,X1))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  geom_smooth(method = "lm", se = F, color = "black")+
  labs(y = "Duração da internação(X1)", x = "Idade(X2)")


# Table

knitr::kable(cbind(round(cor(dados$X2,dados$X1), 3),
                   round(cor(amostra1$X2,amostra1$X1), 3),
                   round(cor(amostra2$X2,amostra2$X1), 3)),
             caption = "Correlação linear de Pearson", col.names = 
               c("Dados Completos", "Amostra 1", "Amostra 2"))

# Internation length X Infection Risk

# Scatterplot

ggplot(dados, aes(X3,X1))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  geom_smooth(method = "lm", se = F, color = "black")+
  labs(y = "Duração da internação(X1)", x = "Risco de infecção(X3)")


# Table

knitr::kable(cbind(round(cor(dados$X3,dados$X1), 3),
                   round(cor(amostra1$X3,amostra1$X1), 3),
                   round(cor(amostra2$X3,amostra2$X1), 3)),
             caption = "Correlação linear de Pearson", col.names = 
               c("Dados Completos", "Amostra 1", "Amostra 2"))

# Internation Length X Hospital Beds

# Scatterplot

ggplot(dados, aes(X6,X1))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  geom_smooth(method = "lm", se = F, color = "black")+
  labs(y = "Duração da internação(X1)", x = "Número de leitos(X6)")


# Table

knitr::kable(cbind(round(cor(dados$X6,dados$X1), 3),
                   round(cor(amostra1$X6,amostra1$X1), 3),
                   round(cor(amostra2$X6,amostra2$X1), 3)),
             caption = "Correlação linear de Pearson", col.names = 
               c("Dados Completos", "Amostra 1", "Amostra 2"))

# Internation Length X Medical School Afilliation

# Boxplot

ggplot(dados, aes(X7,X1))+
  geom_boxplot(fill = c("#69d171","#d96464"))+
  theme_minimal()+
  labs(y = "Duração da internação(X1)", 
       x = "Filiação a Escola de Medicina(X7)")+
  scale_x_discrete(labels=c("1" = "Sim", "2" = "Não"))


## Histogram

ggplot(dados, aes(X1))+
  geom_histogram(aes(), color = "black", fill = "darkslategrey")+
  theme_minimal()+
  facet_wrap(~X7, labeller = as_labeller(c("1" = "Sim", "2" = "Não")))+
  theme(strip.background =element_rect(fill="skyblue"))+
  labs( x = "Duração da internação(X1)", y = "Contagem",
        title = "Distribuição dos dias de internação por filiação")

# Data transformation

x1 <- amostra1 %>%
  group_by(X7) %>%
  summarise("Freq. absoluta amostra1" = n(),
            "Freq. relativa amostra1" = n()/57) %>%
  mutate(`Filiação amostra1`  = fct_recode(X7,
                                           "Sim" = "1",
                                           "Não" = "2")) %>% 
  select(4,2,3)

x2 <- amostra2 %>%
  group_by(X7) %>%
  summarise("Freq. absoluta amostra2" = n(),
            "Freq. relativa amostra2" = n()/56) %>%
  mutate(`Filiação amostra2`  = fct_recode(X7,
                                           "Sim" = "1",
                                           "Não" = "2")) %>% 
  select(4,2,3)

x3 <- dados %>%
  group_by(X7) %>%
  summarise("Freq. absoluta dados" = n(),
            "Freq. relativa dados" = n()/113) %>%
  mutate(`Filiação dados` = fct_recode(X7,
                                       "Sim" = "1",
                                       "Não" = "2")) %>% 
  select(4,2,3)


x <- list(x1,x2,x3)

# Table 

knitr::kable(x, caption = "Tabela da Freq. dos dias de 
             internação por filiação")

# Internation Length X Region

# Boxplot

ggplot(dados, aes(X8,X1))+
  geom_boxplot(fill = c("#69d171","#d96464","#99aae8","#d09ff5"))+
  theme_minimal()+
  labs(y = "Duração da internação(X1)", x = "Região(X8)")+
  scale_x_discrete(labels=c("1" = "NE", "2"= "NC", "3" = "S",  
                            "4" = "W"))


# Histogram

ggplot(dados, aes(X1))+
  geom_histogram(aes(), color = "black", fill = "darkslategrey")+
  theme_minimal()+
  facet_wrap(~X8, labeller = as_labeller(c("1" = "NE", "2"= "NC", 
                                           "3" = "S", 
                                           "4" = "W")))+
  theme(strip.background =element_rect(fill="skyblue"))+
  labs( x = "Duração da internação(X1)", y = "Contagem",
        title = "Distribuição dos dias de internação por região")

# Data transform

x11 <- amostra1 %>%
  group_by(X8) %>%
  summarise("Freq. absoluta" = n(),"Freq. relativa" = n()/57) %>%
  mutate(Região  = fct_recode(X8,
                              "NE" ="1",  "NC" ="2",  "S" ="3" ,  
                              "W" ="4")) %>% 
  select(4,2,3)

x22 <- amostra2 %>%
  group_by(X8) %>%
  summarise("Freq. absoluta" = n(),"Freq. relativa" = n()/56) %>%
  mutate(Região  = fct_recode(X8,
                              "NE" ="1",  "NC" ="2",  "S" ="3" ,  
                              "W" ="4")) %>% 
  select(4,2,3)


x33 <- dados %>%
  group_by(X8) %>%
  summarise("Freq. absoluta" = n(),"Freq. relativa" = n()/113) %>%
  mutate(Região  = fct_recode(X8,
                              "NE" ="1",  "NC" ="2",  "S" ="3" ,   
                              "W" ="4")) %>% 
  select(4,2,3)

# Table 

knitr::kable(list(x11,x22,x33), 
             caption = "Tabela da Freq. dos dias de internação 
             por região")

# Internation Length X Number of patients mean

# Scatterplot

ggplot(dados, aes(X9,X1))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  geom_smooth(method = "lm", se = F, color = "black")+
  labs(y = "Duração da internação(X1)", 
       x = "Média diária de pacientes(X9)")

# Table
knitr::kable(cbind(round(cor(dados$X9,dados$X1), 3),
                   round(cor(amostra1$X9,amostra1$X1), 3),
                   round(cor(amostra2$X9,amostra2$X1), 3)),
             caption = "Correlação linear de Pearson",
             col.names = c("Dados Completos", "Amostra 1",
                           "Amostra 2"))

# Internation Length X Number of nurses

# Scatterplot

ggplot(dados, aes(X10,X1))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  geom_smooth(method = "lm", se = F, color = "black")+
  labs(y = "Duração da internação(X1)", x = "Número de infermeiros(X10)")

# Table
knitr::kable(cbind(round(cor(dados$X10,dados$X1), 3),
                   round(cor(amostra1$X10,amostra1$X1), 3),
                   round(cor(amostra2$X10,amostra2$X1), 3)),
             caption = "Correlação linear de Pearson", 
             col.names = c("Dados Completos", "Amostra 1", "Amostra 2"))


### Model 1
## Complete Model

xi1 <- banco.modelo1$X1 - mean(banco.modelo1$X1)

modelo1.completo <- lm(banco.modelo1$Y~xi1 + 
                         I(xi1^2)+banco.modelo1$X21+
                         banco.modelo1$X22+ banco.modelo1$X23)

#summary(modelo1.completo)

## Tables
# Table 1
nomes <- c("Intercepto","$x_{i1}$","$x_{i1}^2$","$X_{21}$",
           "$X_{22}$","$X_{23}$")

kable(cbind(nomes,summary(modelo1.completo)[["coefficients"]]),
      col.names = c("Variáveis","Estimativas",
                    "Erro Padrão","Estatística","P-valor"),
      row.names = FALSE)

# Table 2
df <- data.frame(F = 45.64,
                 glnum = 5,
                 gldem = 51,
                 pvalor = "<0.0001")
kable(df, col.names = c("Estatística F", "Graus de Liberdade Numerador",
                        "Graus de Liberdade Denominador","P-valor"))

# Table 3
kable(data.frame("1" = c(70.14),
                 "2" = c(51)),
      col.names = c("Erro padrão do resíduo", "Graus de Liberdade"))

# Table 4

kable(cbind(0.8174, 0.7994), col.names = c("$R^2$", "$R^2_{ajustado}$"))

## Residual Analysis

a <- ggplot(modelo1.completo, 
            aes(modelo1.completo$fitted.values,
                modelo1.completo$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Valores Ajustados")+
  ylab("Resíduos")


b <- ggplot(banco.modelo1, aes(X1,modelo1.completo$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Facilidades e serviços disponíveis")+
  ylab("Resíduos")

c <- ggplot(banco.modelo1, aes(sample = modelo1.completo$residuals)) +
  stat_qq(colour = "darkslategray") +
  labs(x="Quantis teóricos",y="Quantis dos Resíduos")+
  stat_qq_line()+
  theme_bw()


grid.arrange(a,b,c,nrow=2,ncol=2)

# Hypothesis tests

s <- shapiro.test(modelo1.completo$residuals)
bp <- bptest(lm(banco.modelo1$Y~xi1 + I(xi1^2)+banco.modelo1$X21+
                  banco.modelo1$X22+ banco.modelo1$X23))

teste <- c("Breusch-Pagan","Shapiro-Wilk")

est <- c(bp$statistic,s$statistic)

pv <- c(bp$p.value,s$p.value)

tabela <- data.frame(teste, est,pv)

kable(tabela, col.names = c("Teste", "Estatísca do Teste", "P-valor"))

## Transformed model

modelo1.trans <- lm(log(banco.modelo1$Y)~xi1 + 
                      I(xi1^2)+banco.modelo1$X21+ 
                      banco.modelo1$X22+ banco.modelo1$X23)

#summary(modelo1.trans)

## Tables 

# Table 1
nomes <- c("Intercepto","$x_{i1}$","$x_{i1}^2$","$X_{21}$",
           "$X_{22}$","$X_{23}$")

kable(cbind(nomes,summary(modelo1.trans)[["coefficients"]]),
      col.names = c("Variáveis","Estimativas",
                    "Erro Padrão","Estatística","P-valor"),
      row.names = FALSE)

# Table 2
df <- data.frame(F = 45.03,
                 glnum = 5,
                 gldem = 51,
                 pvalor = "<0.0001")
kable(df, col.names = c("Estatística F", "Graus de Liberdade Numerador",
                        "Graus de Liberdade Denominador","P-valor"))

# Table 3

kable(data.frame("1" = c(0.4094),
                 "2" = c(51)),
      col.names = c("Erro padrão do resíduo", "Graus de Liberdade"))

# Table 4

kable(cbind(0.8153, 0.7972), col.names = c("$R^2$", "$R^2_{ajustado}$"))

## Residual Analysis

a <- ggplot(modelo1.trans, aes(modelo1.trans$fitted.values,
                               modelo1.trans$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Valores Ajustados")+
  ylab("Resíduos")


b <- ggplot(banco.modelo1, aes(X1,modelo1.trans$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Facilidades e serviços disponíveis")+
  ylab("Resíduos")

c <- ggplot(banco.modelo1, aes(sample = modelo1.trans$residuals)) +
  stat_qq(colour = "darkslategray") +
  labs(x="Quantis teóricos",y="Quantis dos Resíduos")+
  stat_qq_line()+
  theme_bw()


grid.arrange(a,b,c,nrow=2,ncol=2)

# Hypothesis tests

s <- shapiro.test(modelo1.trans$residuals)
bp <- bptest(lm(log(banco.modelo1$Y)~xi1 + 
                  I(xi1^2)+banco.modelo1$X21+ banco.modelo1$X22+
                  banco.modelo1$X23))

teste <- c("Breusch-Pagan","Shapiro-Wilk")

est <- c(bp$statistic,s$statistic)

pv <- c(bp$p.value,s$p.value)

tabela <- data.frame(teste, est,pv)

kable(tabela, col.names = c("Teste", "Estatísca do Teste", "P-valor"))

## Backwards Model

modelo1.tbm <- lm(log(Y)~X1, data=banco.modelo1)

# Criteria Selection

selecao <- regsubsets(log(Y) ~ ., data = banco.modelo1, nbest = 5)

num.var <- as.numeric(rownames(summary(selecao)$which))

df <- data.frame(num.var, r2 = summary(selecao)$rsq, 
                 r2adj = summary(selecao)$adjr2, bic = summary(selecao)$bic,
                 cm = summary(selecao)$cp)

a <- ggplot(df, aes(x=num.var, y=bic)) + 
  geom_point(colour="darkslategray", size=2, alpha = 0.65) +
  labs(x="Número de variáveis explicativas", y="BIC") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=10),
        axis.title.x = element_text(colour="black", size=10),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)

b <- ggplot(df, aes(x=num.var, y=cm)) + 
  geom_point(colour="darkslategray", size=2, alpha = 0.65) +
  labs(x="Número de variáveis explicativas", y="CM") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=10),
        axis.title.x = element_text(colour="black", size=10),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)


c <- ggplot(df, aes(x=num.var, y=r2)) + 
  geom_point(colour="darkslategray", size=2, alpha = 0.65) +
  labs(x="Número de variáveis explicativas", y="R²") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=10),
        axis.title.x = element_text(colour="black", size=10),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)

d <- ggplot(df, aes(x=num.var, y= r2adj)) + 
  geom_point(colour="darkslategray", size=2, alpha = 0.65) +
  labs(x="Número de variáveis explicativas", y="R² ajustado") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=10),
        axis.title.x = element_text(colour="black", size=10),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)

grid.arrange(a,b,c,d,ncol=2)

## Criteria Selection models

# 1 Variable
modelo11 <- lm(log(Y)~X1,data = banco.modelo1)
#summary(modelo11)

# 2 Variables
modelo12 <- lm(log(banco.modelo1$Y)~banco.modelo1$X1 + banco.modelo1$X22)
#summary(modelo12)

# Comparison tables

mod11 <-  c( 0.8116140357,	0.80463678,	-83.018804,	1.65778088)
mod12 <-  c(	0.8103392395,	0.80689086,	-86.677439,	0.01411284)
df <- rbind(mod11,mod12)
kable(df, col.names = c("$R^2$","$R^2_{Ajustado}$","BIC","CM" ))

## Final Model

# Residual Analysis

a <- ggplot(modelo11, aes(modelo11$fitted.values,modelo11$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Valores Ajustados")+
  ylab("Resíduos")



b <- ggplot(banco.modelo1, aes(X1,modelo11$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Facilidades e serviços disponíveis")+
  ylab("Resíduos")


c <- ggplot(banco.modelo1, aes(sample = modelo11$residuals)) +
  stat_qq(colour = "darkslategray") +
  labs(x="Quantis teóricos",y="Quantis dos Resíduos")+
  stat_qq_line()+
  theme_bw()

grid.arrange(a,b,c,ncol=2)

# Hypothesis tests

s <- shapiro.test(modelo11$residuals)
bp <- bptest(lm(log(banco.modelo1$Y)~banco.modelo1$X1))


teste <- c("Breusch-Pagan","Shapiro-Wilk")

est <- c(bp$statistic,s$statistic)

pv <- c(bp$p.value,s$p.value)

tabela <- data.frame(teste, est,pv)

kable(tabela, col.names = c("Teste", "Estatísca do Teste", "P-valor"), 
      row.names = F)

# Influence

medidas <- as.data.frame(influence.measures(modelo11)[[1]])


a <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dffit)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFFIT")

b <- ggplot(medidas, aes(x = c(1:57), y = medidas$cook.d))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("Distância de Cook")

c <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dfb.X1)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFBETA - xi1")



grid.arrange(a, b, c, ncol = 2)
#banco.modelo22 <- banco.modelo1[-48,]
#summary(lm(log(banco.modelo22$Y) ~ xi1[-48]))
#banco.modelo22 <- banco.modelo1[-49,]
#summary(lm(log(banco.modelo22$Y) ~ xi1[-49]))

## Validation

num.enf.chapeu <- predict(modelo11, banco.validacao1)
MSPR <- mean((log(banco.validacao1[,1]) - num.enf.chapeu)^2)
kable(cbind(anova(modelo11)["Residuals","Mean Sq"],MSPR),
      col.names = c("MSE", "MSPR"))

# Parameters and estimatives

betas <- c("Beta_0","Beta_1")
kable(cbind(betas,summary(modelo11)[["coefficients"]]) ,
      col.names = c("Parâmetros","Estimativas",
                    "Erro Padrão","Estatística","P-valor"), row.names = F)

### Model 2

## Complete Model

banco.mod <- amostra1

banco.mod$X7 <- as.character(banco.mod$X7)

a <- banco.mod$X7
a <- as.character(a)
a[-c(which(a == "Sim"))] = 0
a[c(which(a == "Sim"))] = 1

banco.mod$X61 <- as.factor(a) 


a <- banco.mod$X8
a <- as.character(a)
a[-c(which(a == "NE"))] = 0
a[c(which(a == "NE"))] = 1

banco.mod$X71 <- as.factor(a)    

a <- banco.mod$X8
a <- as.character(a)
a[-c(which(a == "NC"))] = 0
a[c(which(a == "NC"))] = 1

banco.mod$X72 <- as.factor(a)    

a <- banco.mod$X8
a <- as.character(a)
a[-c(which(a == "S"))] = 0
a[c(which(a == "S"))] = 1
banco.mod$X73 <- as.factor(a) 

banco.mod <- banco.mod[,c(-1,-8,-9)]

mod <- lm(X1~X2+X3+X4+X5+X6+X9+X10+X11+X61+X71+X72+X73, data = banco.mod)

# Residual Analysis

a <- ggplot(mod, aes(mod$fitted.values,mod$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Valores Ajustados")+
  ylab("Resíduos")

b <- ggplot(banco.mod, aes(sample = mod$residuals)) +
  stat_qq(colour = "darkslategray") +
  labs(x="Quantis teóricos",y="Quantis dos Resíduos")+
  stat_qq_line()+
  theme_bw()

grid.arrange(a,b,ncol=2)

# Hypothesis tests

s <- shapiro.test(mod$residuals)
bp <- bptest(mod)

teste <- c("Breusch-Pagan","Shapiro-Wilk")

est <- c(4.8763,0.98845)

pv <- c(0.962,0.8618)

tabela <- data.frame(teste, est,pv)

kable(tabela, col.names = c("Teste", "Estatísca do Teste", "P-valor"))

kable(summary(mod)[["coefficients"]],
      col.names = c("Estimativas","Erro Padrão","Estatística","P-valor"))

## Complete Model

modelo2.completo <- lm(Y~X1+X2+X3+X4+X5+X61+X71+X72+X73,data=banco.modelo2)

## Tables 

# Table 1
df <- data.frame("Variable" = c("Intercepto", "X1", "X2", "X3", "X4",
                                "X5", "X61", "X71", "X72", "X73"),
                 "Estimativas"= c(3.307, 0.0609, 0.4092, -0.0088, 0.0214,
                                  -0.0073, 0.3179, 1.4706, 0.7521, 0.2579),
                 "Erro Padrão"=c(2.0034,0.0348,0.1170,0.0046,
                                 0.0053,0.0027,0.5809,0.5042,0.4767,0.4704),
                 "Estatística" = c(1.663,1.751,3.497,-1.894,
                                   4.020,-2.674,0.547,2.917,1.578,0.548),
                 "P-valor" = c(0.1031,0.0865,0.0010,0.0644,0.0002,
                               0.0103,0.5868,0.0054,0.1213,0.5861))
names(df) <- c("Variable","Estimativas","Erro Padrão",
               "Estatística","P-valor")
kable(df)

# Table 2
df <- data.frame(F = 14.16,
                 glnum = 9,
                 gldem = 47,
                 pvalor = "<0.0001")
kable(df, col.names = c("Estatística F", "Graus de Liberdade Numerador",
                        "Graus de Liberdade Denominador","P-valor"))

# Table 3

kable(data.frame("1" = c(1.087),
                 "2" = c(47)),
      col.names = c("Erro padrão do resíduo", "Graus de Liberdade"))

# Table 4

kable(cbind(0.7306, 0.679), col.names = c("$R^2$", "$R^2_{ajustado}$"))

## Residual Analysis

a <- ggplot(modelo2.completo, aes(modelo2.completo$fitted.values,
                                  modelo2.completo$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Valores Ajustados")+
  ylab("Resíduos")


b <- ggplot(banco.modelo2, aes(X1,modelo2.completo$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Idade")+
  ylab("Resíduos")

c <- ggplot(banco.modelo2, aes(X2,modelo2.completo$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Risco de Infecção")+
  ylab("Resíduos")


d <- ggplot(banco.modelo2, aes(X3,modelo2.completo$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Número de leitos")+
  ylab("Resíduos")


e <- ggplot(banco.modelo2, aes(X4,modelo2.completo$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Número Médio de pacientes")+
  ylab("Resíduos")

f <- ggplot(banco.modelo2, aes(X5,modelo2.completo$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Número médio de infermeiros(as)")+
  ylab("Resíduos")

grid.arrange(a,b,c,d,e,f,ncol=3)

# QQ Plot

ggplot(banco.modelo2, aes(sample = modelo2.completo$residuals)) +
  stat_qq(colour = "darkslategray") +
  labs(x="Quantis teóricos",y="Quantis dos Resíduos")+
  stat_qq_line()+
  theme_bw()

# Tests table

s <- shapiro.test(modelo2.completo$residuals)
bp <- bptest(Y~X1+X2+X3+X4+X5+X61+X71+X72+X73,data=banco.modelo2)

teste <- c("Breusch-Pagan","Shapiro-Wilk")

est <- c(4.8412,0.99143)

pv <- c(0.8479,0.9581)

tabela <- data.frame(teste, est,pv)

kable(tabela, col.names = c("Teste", "Estatísca do Teste", "P-valor"))

### Variables selection

## Backwards Model
modelo2.reduzido <- lm(Y~X2+X3+X4+X5+X71,data=banco.modelo2)

# Criteria Selection

# Plots

selecao <- regsubsets(Y ~ ., data = banco.modelo2, nbest = 8)

num.var <- as.numeric(rownames(summary(selecao)$which))

df <- data.frame(num.var, r2 = summary(selecao)$rsq, 
                 r2adj = summary(selecao)$adjr2, bic = summary(selecao)$bic, 
                 cm = summary(selecao)$cp)

a <- ggplot(df, aes(x=num.var, y=bic)) + 
  geom_point(colour="darkslategray", size=2, alpha = 0.65) +
  labs(x="Número de variáveis explicativas", y="BIC") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=10),
        axis.title.x = element_text(colour="black", size=10),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)

b <- ggplot(df, aes(x=num.var, y=cm)) + 
  geom_point(colour="darkslategray", size=2, alpha = 0.65) +
  labs(x="Número de variáveis explicativas", y="CM") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=10),
        axis.title.x = element_text(colour="black", size=10),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)


c <- ggplot(df, aes(x=num.var, y=r2)) + 
  geom_point(colour="darkslategray", size=2, alpha = 0.65) +
  labs(x="Número de variáveis explicativas", y="R²") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=10),
        axis.title.x = element_text(colour="black", size=10),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)

d <- ggplot(df, aes(x=num.var, y= r2adj)) 
+ geom_point(colour="darkslategray", size=2, alpha = 0.65) +
  labs(x="Número de variáveis explicativas", y="R² ajustado") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=10),
        axis.title.x = element_text(colour="black", size=10),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)

grid.arrange(a,b,c,d,ncol=2)

# Model 4 variables

modelo24 <- lm(Y~X2+X4+X5+X71,data=banco.modelo2)
#summary(modelo24)

# Model 5 variables

modelo25 <- lm(Y~X2+X3+X4+X5+X71,data=banco.modelo2)
#summary(modelo25)

# Model 6 variables

modelo26 <- lm(Y~X1+X2+X3+X4+X5+X71,data=banco.modelo2)
#summary(modelo26)

# Model comparison table

mod1 <-  c(4, 0.6660346,	0.6403450,	-42.29767,	11.27045)
mod2 <-  c(5,	0.6949882,	0.6650851,	-43.42376,	8.218618)
mod3 <-  c(6,	0.7096083,	0.6747613,	-42.18055,	7.667686)
df <- rbind(mod1,mod2,mod3)
kable(df, col.names = c("Modelo","$R^2$","$R^2_{Ajustado}$","BIC","CM" ))


## Final Model 2

# Residual Analysis

a <- ggplot(modelo24, aes(modelo24$fitted.values,modelo24$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Valores Ajustados")+
  ylab("Resíduos")



b <- ggplot(banco.modelo2, aes(X2,modelo24$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Risco de Infecção")+
  ylab("Resíduos")



c <- ggplot(banco.modelo2, aes(X4,modelo24$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Número Médio de pacientes")+
  ylab("Resíduos")

d <- ggplot(banco.modelo2, aes(X5,modelo24$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Número médio de infermeiros(as)")+
  ylab("Resíduos")

e <- ggplot(banco.modelo2, aes(sample = modelo24$residuals)) +
  stat_qq(colour = "darkslategray") +
  labs(x="Quantis teóricos",y="Quantis dos Resíduos")+
  stat_qq_line()+
  theme_bw()

grid.arrange(a,b,c,d,e,ncol=3)

# Tests and table

s <- shapiro.test(modelo24$residuals)
bp <- bptest(Y~X2+X4+X5+X71,data=banco.modelo2)


teste <- c("Breusch-Pagan","Shapiro-Wilk")

est <- c(bp$statistic,s$statistic)

pv <- c(bp$p.value,s$p.value)

tabela <- data.frame(teste, est,pv)

kable(tabela, col.names = c("Teste", "Estatísca do Teste", 
                            "P-valor"), row.names = F)

# Multicollinearity

vi<-vif(modelo24)
kable(vi, col.names = "VIF")

# Influence

medidas <- as.data.frame(influence.measures(modelo24)[[1]])


a <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dffit)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFFIT")

b <- ggplot(medidas, aes(x = c(1:57), y = medidas$cook.d))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("Distância de Cook")

c <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dfb.X2)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFBETA - Risco de Infecção")

d <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dfb.X4)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFBETA - Média Diária de Pacientes")

e <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dfb.X5)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFBETA - Número de Enfermeiros")

f <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dfb.X711)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFBETA - Região NE")

grid.arrange(a, b, c, d, e, f, ncol = 3)

#summary(lm(Y ~ X2+X4+X5+X71, data = banco.modelo2))
#summary(lm(Y ~ X2+X4+X5+X71, data =banco.modelo2[-56,]))
#summary(lm(Y ~ X2+X4+X5+X71, data =banco.modelo2[-28,]))

## Final Model 2 Validation

num.enf.chapeu <- predict(modelo24, banco.validacao2)
MSPR <- mean((banco.validacao2[,1] - num.enf.chapeu)^2)
kable(cbind(anova(modelo24)["Residuals","Mean Sq"],MSPR),
      col.names = c("MSE", "MSPR"))


## Transformed Final model

modelo24.trans <- lm(log(Y)~X2+X4+X5+X71,data=banco.modelo2)

# Residual Analysis

a <- ggplot(modelo24.trans, aes(modelo24.trans$fitted.values,modelo24.trans$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Valores Ajustados")+
  ylab("Resíduos")



b <- ggplot(banco.modelo2, aes(X2,modelo24.trans$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Risco de Infecção")+
  ylab("Resíduos")



c <- ggplot(banco.modelo2, aes(X4,modelo24.trans$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Número Médio de pacientes")+
  ylab("Resíduos")

d <- ggplot(banco.modelo2, aes(X5,modelo24.trans$residuals))+
  geom_point(color = "darkslategray")+
  theme_minimal()+
  xlab("Número médio de infermeiros(as)")+
  ylab("Resíduos")

e <- ggplot(banco.modelo2, aes(sample = modelo24.trans$residuals)) +
  stat_qq(colour = "darkslategray") +
  labs(x="Quantis teóricos",y="Quantis dos Resíduos")+
  stat_qq_line()+
  theme_bw()

grid.arrange(a,b,c,d,e,ncol=3)

# Tests and Table

s <- shapiro.test(modelo24.trans$residuals)
bp <- bptest(log(Y)~X2+X4+X5+X71,data=banco.modelo2)


teste <- c("Breusch-Pagan","Shapiro-Wilk")

est <- c(bp$statistic,s$statistic)

pv <- c(bp$p.value,s$p.value)

tabela <- data.frame(teste, est,pv)

kable(tabela, col.names = c("Teste", "Estatísca do Teste", "P-valor"),
      row.names = F)

# Multicollinearity

vi<-vif(modelo24.trans)
kable(vi, col.names = "VIF")

# Influence

medidas <- as.data.frame(influence.measures(modelo24.trans)[[1]])


a <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dffit)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFFIT")

b <- ggplot(medidas, aes(x = c(1:57), y = medidas$cook.d))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("Distância de Cook")

c <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dfb.X2)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFBETA - Risco de Infecção")

d <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dfb.X4)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFBETA - Média Diária de Pacientes")

e <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dfb.X5)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFBETA - Número de Enfermeiros")

f <- ggplot(medidas, aes(x = c(1:57), y = abs(medidas$dfb.X711)))+
  geom_point(color = "darkslategrey")+
  theme_minimal()+
  xlab("Índice")+
  ylab("DFBETA - Região NE")

grid.arrange(a, b, c, d, e, f, ncol = 3)

#summary(lm(Y ~ X2+X4+X5+X71, data = banco.modelo2))
#summary(lm(Y ~ X2+X4+X5+X71, data =banco.modelo2[-56,]))
#summary(lm(Y ~ X2+X4+X5+X71, data =banco.modelo2[-28,]))

## Transformed Final Model 2 validation

num.enf.chapeu <- predict(modelo24.trans, banco.validacao2)
MSPR <- mean((log(banco.validacao2[,1]) - num.enf.chapeu)^2)
kable(cbind(anova(modelo24.trans)["Residuals","Mean Sq"],MSPR),
      col.names = c("MSE", "MSPR"))

# Estimation and Parameters

betas <- c("Beta_0","Beta_2","Beta_4","Beta_5","Beta_71")
kable(cbind(betas,summary(modelo24.trans)[["coefficients"]]) ,
      col.names = c("Parâmetros","Estimativas",
                    "Erro Padrão","Estatística","P-valor"), 
      row.names = F)