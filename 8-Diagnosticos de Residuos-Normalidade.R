                   #Aula 8 - Diagnósticos de Resíduos

                      # Instalando e carregando pacotes - Executar apenas uma vez
install.packages("normtest")                                          #Instala o pacote normtest
install.packages("agricolae")                                         #Instala o pacote agrocolae
library(agricolae)                                                    #Carrega o pacote agricolae
library(normtest)                                                     #Carrega o pacote normtest
library(readxl)                                                        #Carrega o pacote readxl

                      #Carregando o arquivo xls
variacao_PIB <- read.table("c:/Econometria/variacao.xls", header = T)  #Lê o arquivo variacao.xls na pasta c:/Econometria
var_PIB <- ts(variacao_PIB$variacao_PIB, start =1951, frequency = 1 )  #Cria a serie temporal var_PIB

                      #Realizando teste de normalidade dos resíduos
AR2 <- arima(var_PIB,c(2,0,0))        #Estima um modelo AR2                      
residuosAR2 <- AR2$residuals          #Extrai os resíduos do modelo AR2

                      #Histogramas dos Resíduos
hist(residuosAR2)                                               #Cria histograma padrão
hist(residuosAR2, main = "Histograma dos Residuos")             #Cria histograma com título
hist(residuosAR2, main = "Histograma dos Residuos", col="Gray") #Cria histograma com título e cor cinza
hist(residuosAR2, main = "Histograma dos Residuos", col="Gray", breaks=20) #Cria histograma com título, cor cinza e aumenta número de intervalos para 20
                     
                      #Personalizando ainda mais o Histograma (digitar no script)
hist(residuosAR2, main = "Histograma dos Residuos", col="Gray",breaks = 20,             
               xlab="Residuos",                                              #Altera o nome do eixo x para Residuos
               ylab = "Desnsidade",                                          #Altera o nome do eixo x para Densidade
               ylim = c(0,15),                                               #Altera a amplitude do eixo y: tamanho de 0 a 25
               xlim =c(-0.15,0.15)  )                                        #Altera a amplitude do eixo x: tamanho de -0.2 a 0.2

    #Incluindo Linha de Suavização segundo Kernel, esses comandos devem ser realizados logo após a criação dos histogramas
lines(density(residuosAR2, bw=0.03),col="Blue")                            #Linha azul e e largura da janela(bw)=0.03
lines(density(residuosAR2, bw=0.03, kernel = "gaussian"),col="Blue")       #Linha azul e e largura da janela(bw)=0.03, Kernel Gaussiano (padrão)
lines(density(residuosAR2, bw=0.03, kernel = "triangular"),col="Red")      #Linha vermelha: Kernel Triangular
lines(density(residuosAR2, bw=0.03, kernel = "epanechnikov"),col="Yellow") #Linha amarela kernel Epanechnikov
lines(density(residuosAR2, bw=0.03, kernel = "biweight"),col="Green")      #Linha verde kernel Biponderado

           #Resetando para o Gráfico Original
hist(residuosAR2, main = "Histograma dos Residuos", col="Gray",breaks = 20,             
     xlab="Residuos",                                              
     ylab = "Desnsidade",                                          
     ylim = c(0,20),                                               
     xlim =c(-0.15,0.15)  )  

           #Incluindo larguras de janelas diversas
lines(density(residuosAR2, bw=0.02),col="Blue")    #h=0.02
lines(density(residuosAR2, bw=0.01),col="Red")     #h=0.01
lines(density(residuosAR2, bw=0.005),col="Yellow") #h=0.005

           #Teste Jarque Bera para Normalidade
jb.norm.test(residuosAR2)   #Estatística JB sob hipóte nula de normalidade
skewness(residuosAR2)       #Retorna o valor da assimetria
kurtosis(residuosAR2)       #Retorna valor da curtose
