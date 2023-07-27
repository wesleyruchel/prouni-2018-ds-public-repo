## URL::
## https://brasil.io/dataset/cursos-prouni/cursos/


# Inicializar ambiente e tratamentos --------------------------------------

library(dplyr)
library(ggplot2)
library(forcats)
library(stats)
library(class)

options(dplyr.print_max = Inf)

## diretório padrão
setwd("D:/Dev/prouni-2018-ds")


## dados
df <- read.csv("dataset/cursos-prouni.csv")


## remove variáveis não significativas ou duplas
exclude <- c("curso_id","cidade_filtro","campus_id", "curso_busca")
df <- df[,!(names(df)%in% exclude)]


## ajusta nome das variáveis
colnames(df)[8:12] <- c('cidade','uf','universidade','campus','curso')


# funções 

## normaliza valores numéricos
normalizar = function(x) { 
  (x - min(x)) / (max(x) - min(x))
}

## cálculo da acurácia do modelo
calcularacuracia = function(x) {
  (sum(diag(x)) / sum(rowSums(x))) * 100
}

# Visualizações -----------------------------------------------------------


## frequência dos cursos no dataframe
## top 10 cursos com maiores quantidades
data <- df %>% 
  group_by(curso) %>% 
  summarize(qnt=n()) %>% 
  arrange(desc(qnt)) %>% 
  top_n(10, qnt) %>%
  as.data.frame()

ggplot(data, aes(x = fct_reorder(curso, qnt), y = qnt)) +
  geom_col(fill="steelblue") +
  geom_text(aes(label=qnt), vjust=1.6, color="white", size=3.5)+
  labs(x = "cursos", y = "quantidade")


## frequência dos cursos no dataframe
## top 10 cursos com menores quantidades (com quantidade distintas)
data <- df %>% 
  group_by(curso) %>% 
  summarize(qnt=n()) %>% 
  arrange(desc(qnt)) %>% 
  distinct(qnt, .keep_all= TRUE) %>%
  top_n(-10, qnt) %>%
  as.data.frame()

ggplot(data, aes(x = fct_reorder(curso, qnt), y = qnt)) +
  geom_col(fill="steelblue") +
  geom_text(aes(label=qnt), vjust=1.6, color="white", size=3.5)+
  labs(x = "cursos", y = "quantidade")


## quantidade de cursos no dataframe por estado
## top 10 estados com maiores quantidades
data <- df %>% 
  group_by(uf) %>% 
  summarize(qnt=n()) %>% 
  arrange(desc(qnt)) %>% 
  top_n(10, qnt) %>%
  as.data.frame()

ggplot(data, aes(x = fct_reorder(uf, qnt), y = qnt)) +
  geom_col(fill="steelblue") +
  geom_text(aes(label=qnt), vjust=1.6, color="white", size=3.5)+
  labs(x = "UF", y = "quantidade")


## quantidade de cursos no dataframe por estado
## todos os estados
data <- df %>% 
  group_by(uf) %>% 
  summarize(qnt=n()) %>% 
  arrange(desc(qnt)) %>%
  as.data.frame()

ggplot(data, aes(x = fct_reorder(uf, qnt), y = qnt)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  geom_text(aes(label=qnt), vjust=-0.1, size=3.5)+
  labs(x = "UF", y = "quantidade")


## top mensalidades mais caras
df %>% 
  arrange(desc(mensalidade)) %>%
  distinct(mensalidade, .keep_all= TRUE) %>%
  slice_max(mensalidade, n=10) %>%
  select(mensalidade, curso, uf, universidade) %>%


## top mensalidades mais baratas
df %>%
  arrange(desc(mensalidade)) %>%
  distinct(mensalidade, .keep_all= TRUE) %>%
  slice_min(mensalidade, n=10) %>%
  select(mensalidade, curso, uf, universidade) %>%


## maior mensalidade por estado
data <- df %>% 
  group_by(uf) %>% 
  arrange(desc(mensalidade)) %>% 
  distinct(uf, .keep_all= TRUE) %>%
  select(uf, mensalidade, curso, universidade) %>%
  as.data.frame()

ggplot(data, aes(x = fct_reorder(uf, mensalidade), y = mensalidade)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  geom_text(aes(label=mensalidade), vjust=-0.1, size=3.5)+
  labs(x = "UF", y = "mensalidade")


## maiores mensalidade por curso
df %>% 
  arrange(desc(mensalidade)) %>% 
  distinct(curso, .keep_all= TRUE) %>%
  select(uf, mensalidade, curso, universidade)


## top 10 mensalidade por curso
data <- df %>% 
  arrange(desc(mensalidade)) %>% 
  distinct(curso, .keep_all= TRUE) %>%
  slice_max(mensalidade, n=10) %>%
  select(uf, mensalidade, curso, universidade) %>%
  as.data.frame()


ggplot(data, aes(x = fct_reorder(curso, mensalidade), y = mensalidade)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  geom_text(aes(label=mensalidade), vjust=-0.1, size=3.5)+
  labs(x = "curso", y = "mensalidade")


## maior nota para curso integral
df %>%
  arrange(desc(nota_integral_ampla)) %>%
  distinct(nota_integral_ampla, .keep_all= TRUE) %>%
  slice_max(nota_integral_ampla, n=5) %>%
  select(mensalidade, nota_integral_ampla, curso, uf, universidade)


## menor nota para curso integral
df %>%
  arrange(desc(nota_integral_ampla)) %>%
  distinct(nota_integral_ampla, .keep_all= TRUE) %>%
  slice_min(nota_integral_ampla, n=5) %>%
  select(mensalidade, nota_integral_ampla, curso, uf, universidade)


## Algoritmo de agrupamento - kmeans()
## dois grupos, nota integral ampas e mensalidade
data <- df %>%
  select(mensalidade, nota_integral_ampla) %>%
  as.data.frame()

data <- data[complete.cases(data),]

grupos <- kmeans(data, 2, iter.max = 5)

grupos$cluster <- as.factor(grupos$cluster)

ggplot(data, aes(mensalidade, nota_integral_ampla, color = grupos$cluster)) +
  geom_point(show.legend = FALSE)


## Algoritmo de classificação - knn()
## com mensalidade e nota integral, determinar qual curso, limitando a cursos
## específicos

data <- df %>%
  select(mensalidade, nota_integral_ampla, curso) %>%
  filter(curso %in% c("Direito", "Medicina", "Administração")) %>%
  as.data.frame()


## remove NAs
data <- data[complete.cases(data),]


## normaliza elementos numéricos
## mensalidade e nota integral ampla
m_lista_normalizada = lapply(data[,c(1:2)], normalizar)


## joga para um frame
m_df_normalizado = as.data.frame(m_lista_normalizada)


## amostra com valores aleatórios 70%
amostra = sample(x = 1:nrow(data), size = 0.70 * nrow(data))


## amostra p/ treino
df_treino = m_df_normalizado[amostra,]

## qnt amostra de treino
nrow(df_treino)


# amostra p/ teste
df_teste = m_df_normalizado[-amostra,]

## qnt amostra de teste
nrow(df_teste)


## adiciona a variável curso nos frames
df_curso_treino = data[amostra,"curso"]
df_curso_teste = data[-amostra,"curso"]


## KNN()
previsoes = knn(train=df_treino, test=df_teste, cl=df_curso_treino, k=7)


# matriz de confusão
matrizconfusao = table(previsoes, df_curso_teste)
matrizconfusao


# acurácia do modelo
acuracia = calcularacuracia(matrizconfusao)
acuracia

