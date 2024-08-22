
# Instalando os pacotes necessários:

#install.packages(c('dplyr', 'magrittr', 'ggplot2', 'readr', 'forcats'))

# Importando os pacotes necessários:

library(dplyr)
library(magrittr)
library(ggplot2)
library(readr)
library(forcats)

# Escolhendo o diretório para leitura do banco de dados (BD)

setwd('C:/Users/Filipe/Desktop/Preditiva')

# Lendo o BD

# Obs.: Antes de importar os dados, alterei, no excel, o nome das variáveis para, 
# respectivamente: 'COMPROU', 'IDADE', 'PROFISSAO', 'ESTADO_CIVIL', 'FORMACAO', 
# 'CLIENTE_DEVEDOR', 'SALDO_CC', 'POSSUI_HIPOTECA', 'POSSUI_EMPRESTIMO', 
# 'QTD_LIGACOES_RELIZADAS'

dados <- read_csv('bank_marketing.csv', col_types = 'fnffffnffn')

# Alterando os caracteres de algumas variáveis para UTF-8 e posteriormente para 
# fatores

dados <- dados %>%
  mutate(across(.cols = c(COMPROU, PROFISSAO, FORMACAO, CLIENTE_DEVEDOR,
                          POSSUI_HIPOTECA, POSSUI_EMPRESTIMO),
                .fns = iconv, from = 'latin1', to = 'UTF-8'),
         across(.cols = c(COMPROU, PROFISSAO, FORMACAO, CLIENTE_DEVEDOR,
                          POSSUI_HIPOTECA, POSSUI_EMPRESTIMO),
                .fns = as.factor))

# Criando uma função para formatar números. Faz com que o número não fique na
# forma científica, fique com o ponto como separador de milhar e fique com a
# vírgula como separador de decimal

formato_numerico <- function(x) {
  format(x, big.mark = '.', decimal.mark = ',', scientific = FALSE)
}

###############################
# Análises variáveis numéricas
###############################

# Tabela de frequências da variável IDADE:

dados %>%
  filter(COMPROU == 'Sim') %>%
  mutate(Intervalo = cut(IDADE, 
                         breaks = seq(0, 100, by = 10),
                         right = FALSE,
                         include.lowest = TRUE)) %>%
  count(Intervalo)


# Tabela de frequências da variável SALDO_CC:

dados %>%
  filter(COMPROU == 'Sim') %>%
  filter(!is.na(SALDO_CC)) %>%
  mutate(Intervalo = cut(SALDO_CC, 
                         breaks = seq(min(SALDO_CC), max(SALDO_CC) + 3000, by = 3000),
                         right = FALSE,
                         include.lowest = TRUE,
                         labels = FALSE)) %>%
  mutate(Intervalo = ifelse(is.na(Intervalo), 'Fora do Intervalo',
                            paste0(
                              formato_numerico(seq(min(SALDO_CC), max(SALDO_CC) + 3000, by = 3000)[Intervalo]),
                              ' - ',
                              formato_numerico(seq(min(SALDO_CC), max(SALDO_CC) + 3000, by = 3000)[Intervalo + 1] - 1)
                            ))) %>%
  count(Intervalo)

# Box-Plot da variável IDADE pela indicadora de compra do título

dados %>%
  ggplot() + 
  geom_boxplot(aes(x = COMPROU, y = IDADE, group = COMPROU, color = COMPROU)) +
  geom_point(aes(x = COMPROU, y = mean(IDADE, na.rm = T), group = COMPROU, 
                 color = COMPROU), size = 3, shape = 4) + 
  ggtitle(label = 'Box-Plot da variável IDADE pela indicadora de compra do título') + 
  theme_minimal()

# Box-Plot da variável IDADE pela indicadora de compra do título

dados %>%
  ggplot() + 
  geom_boxplot(aes(x = COMPROU, y = SALDO_CC, group = COMPROU, 
                   color = COMPROU)) +
  geom_point(aes(x = COMPROU, y = mean(SALDO_CC, na.rm = T), group = COMPROU, 
                 color = COMPROU), size = 3, shape = 4) + 
  ggtitle(label = 'Box-Plot da variável SALDO_CC pela indicadora de compra do título') +
  theme_minimal()

# Cálculo do 3º quartil da variável QTD_LIGACOES_RELIZADAS

quantile(dados$QTD_LIGACOES_RELIZADAS, 0.75)

# As pessoas que mais adquiriram títulos têm idade entre 20 e 60 anos.
# As pessoas com com saldo na CC de até R$3.000 foram as que mais adquiriram títulos.
# Além disso, foram necessárias 3 ligações para que 75% das pessoas que compraram 
# os títulos os adquirissem

################################
# Análise da variável Profissão
################################

# Gráfico da Distribuição das Profissões e Compras com Percentual

dados %>%
  filter(!is.na(COMPROU)) %>%
  group_by(PROFISSAO, COMPROU) %>%
  summarise(Contagem = n()) %>%
  mutate(Percentual = Contagem / sum(Contagem) * 100) %>%
  ggplot(aes(x = fct_infreq(PROFISSAO), y = Contagem, fill = COMPROU)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_text(aes(label = paste0(round(Percentual, 1), '%')), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = 'Distribuição das Profissões e Compras com Percentual',
    x = 'Profissão', 
    y = 'Contagem',
    fill = 'Comprou'
  ) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust = 1))

# Tabela cruzada da variável indicadora de compra pela PROFISSAO

table(dados$COMPROU, dados$PROFISSAO)
prop.table(table(dados$COMPROU, dados$PROFISSAO), margin = 2) * 100 # Percentual
# por coluna, ou seja, por PROFISSAO

# Pessoas com as seguintes profissões, percentualmente, adquiriram mais o título: 
# APOSENTADO, DESEMPREGADO e ESTUDANTE. Por outro lado, DIARISTA, OPERARIOS, 
# EMPRESARIO e os SERVICOS GERAIS tenderam a não adquiri-lo

###################################
# Análise da variável Estado Civil
###################################

# Gráfico da Distribuição dos Estados Civil e Compras com Percentual

dados %>%
  filter(!is.na(COMPROU)) %>%
  group_by(ESTADO_CIVIL, COMPROU) %>%
  summarise(Contagem = n()) %>%
  mutate(Percentual = Contagem / sum(Contagem) * 100) %>%
  ggplot(aes(x = fct_infreq(ESTADO_CIVIL), y = Contagem, fill = COMPROU)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_text(aes(label = paste0(round(Percentual, 1), '%')), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = 'Distribuição dos Estados Civil e Compras com Percentual',
    x = 'Estado Civil', 
    y = 'Contagem',
    fill = 'Comprou'
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(vjust = 1))

# Tabela cruzada da variável indicadora de compra pelo ESTADO_CIVIL

table(dados$COMPROU, dados$ESTADO_CIVIL)
prop.table(table(dados$COMPROU, dados$ESTADO_CIVIL), margin = 2) * 100 # Percentual
# por coluna, ou seja, por ESTADO_CIVIL

# Em relação ao estado civil, SOLTEIRO tende a comprar mais o título. Por outro
# lado, os CASADOS tendem a adquirir com menos frequência os títulos

###############################
# Análise da variável Formação
###############################

# Gráfico da Distribuição das Formações e Compras com Percentual

dados %>%
  filter(!is.na(COMPROU)) %>%
  group_by(FORMACAO, COMPROU) %>%
  summarise(Contagem = n()) %>%
  mutate(Percentual = Contagem / sum(Contagem) * 100) %>%
  ggplot(aes(x = fct_infreq(FORMACAO), y = Contagem, fill = COMPROU)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_text(aes(label = paste0(round(Percentual, 1), '%')), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = 'Distribuição das Formações e Compras com Percentual',
    x = 'Formação', 
    y = 'Contagem',
    fill = 'Comprou'
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(vjust = 1))

# Tabela cruzada da variável indicadora de compra pela FORMACAO

table(dados$COMPROU, dados$FORMACAO)
prop.table(table(dados$COMPROU, dados$FORMACAO), margin = 2) * 100 # Percentual
# por coluna, ou seja, por FORMACAO

# Quanto à FORMACAO, na sua maioria, quem possui ENSINO SUPERIOR adquiriu os 
# títulos, e quem possui FUNDAMENTAL ou MÉDIO não adquiriu

####################################
# Análise pela Situação de crédito
####################################

# Gráfico da Distribuição da característica de DEVEDOR com Percentual

dados %>%
  filter(!is.na(COMPROU)) %>%
  group_by(CLIENTE_DEVEDOR, COMPROU) %>%
  summarise(Contagem = n()) %>%
  mutate(Percentual = Contagem / sum(Contagem) * 100) %>%
  ggplot(aes(x = fct_infreq(CLIENTE_DEVEDOR), y = Contagem, fill = COMPROU)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_text(aes(label = paste0(round(Percentual, 1), '%')), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = 'Distribuição da característica de DEVEDOR com Percentual',
    x = 'Devedor', 
    y = 'Contagem',
    fill = 'Comprou'
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(vjust = 1))

# Tabela cruzada da variável indicadora de compra pela indicadora de CLIENTE_DEVEDOR

table(dados$COMPROU, dados$CLIENTE_DEVEDOR)
prop.table(table(dados$COMPROU, dados$CLIENTE_DEVEDOR), margin = 2) * 100 # Percentual
# por coluna, ou seja, por CLIENTE_DEVEDOR

# Conforme a análise, a tendência é de que se o cliente é DEVEDOR, não adquire o título

# Gráfico da Distribuição da característica de POSSUI_HIPOTECA com Percentual

dados %>%
  filter(!is.na(COMPROU)) %>%
  group_by(POSSUI_HIPOTECA, COMPROU) %>%
  summarise(Contagem = n()) %>%
  mutate(Percentual = Contagem / sum(Contagem) * 100) %>%
  ggplot(aes(x = fct_infreq(POSSUI_HIPOTECA), y = Contagem, fill = COMPROU)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_text(aes(label = paste0(round(Percentual, 1), '%')), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = 'Distribuição da propriedade de hipoteca com Percentual',
    x = 'Possui Hipoteca', 
    y = 'Contagem',
    fill = 'Comprou'
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(vjust = 1))

# Tabela cruzada da variável indicadora de compra pela variável que indica se o
# cliente POSSUI_HIPOTECA

table(dados$COMPROU, dados$POSSUI_HIPOTECA)
prop.table(table(dados$COMPROU, dados$POSSUI_HIPOTECA), margin = 2) * 100 # Percentual
# por coluna, ou seja, por POSSUI_HIPOTECA

# Conforme a análise, a tendência é de que se o cliente possui HIPOTECA, não adquire o título

# Gráfico da Distribuição da característica de POSSUI_EMPRESTIMO com Percentual

dados %>%
  filter(!is.na(COMPROU)) %>%
  group_by(POSSUI_EMPRESTIMO, COMPROU) %>%
  summarise(Contagem = n()) %>%
  mutate(Percentual = Contagem / sum(Contagem) * 100) %>%
  ggplot(aes(x = fct_infreq(POSSUI_EMPRESTIMO), y = Contagem, fill = COMPROU)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_text(aes(label = paste0(round(Percentual, 1), '%')), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = 'Distribuição da propriedade de empréstimo com Percentual',
    x = 'Possui Emprestimo', 
    y = 'Contagem',
    fill = 'Comprou'
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    axis.text.x = element_text(vjust = 1),
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )

# Tabela cruzada da variável indicadora de compra pela variável que indica se o
# cliente POSSUI_EMPRESTIMO

table(dados$COMPROU, dados$POSSUI_EMPRESTIMO)
prop.table(table(dados$COMPROU, dados$POSSUI_EMPRESTIMO), margin = 2) * 100 # Percentual
# por coluna, ou seja, por POSSUI_EMPRESTIMO

# Conforme a análise, a tendência é de que se o cliente possui EMPRESTIMO, não adquire o título

# O público-alvo da venda do título é: aposentados, desempregados e estudantes, 
# solteiros, com idade entre 20 e 60 anos, que possuam nível superior, com melhor 
# situação de crédito, que não devam, que não possuam hipoteca e que não possuam 
# empréstimo,com até 3 mil reais na CC. Além disso, devem ser feitas, pelo menos 
# 3 ligações para o cliente visando realizar a venda do título
  