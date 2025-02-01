# install.packages("ineq")
# install.packages("openxlsx")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("tidyr")
# install.packages(c("plm", "lmtest", "car", "stargazer", "kableExtra","knitr"))

library(readxl)
library(openxlsx) 
library(dplyr)    
library(ggplot2) 
library(plotly) 
library(ineq)    
library(knitr)

### CARICAMENTO DEI DATI ###

data_1 <-read.xlsx("Aida_1.xlsx")
data_2 <-read.xlsx("Aida_2.xlsx")
data_3 <-read.xlsx("Aida_3.xlsx")
data_4 <-read.xlsx("Aida_4.xlsx")
data_5 <-read.xlsx("Aida_5.xlsx")
data_6 <-read.xlsx("Aida_6.xlsx")
data_7 <-read.xlsx("Aida_7.xlsx")
data_8 <-read.xlsx("Aida_8.xlsx")
data_9 <-read.xlsx("Aida_9.xlsx")
data_10 <-read.xlsx("Aida_10.xlsx")
data_11 <-read.xlsx("Aida_11.xlsx")
data_12 <-read.xlsx("Aida_12.xlsx")
data_13 <-read.xlsx("Aida_13.xlsx")
data_14 <-read.xlsx("Aida_14.xlsx")

Settori <- read_excel("Settori_Industriali.xlsx")

# Combina i dati dei vari settori in un unico data frame
data <- rbind.data.frame(data_1, data_2, data_3, data_4, data_5, data_6, data_7, data_8, data_9, data_10, data_11, data_12, data_13, data_14)


# nomi_dati = paste0("Aida_", 1:14, ".xlsx")
# data = read.xlsx(nomi_dati[1])
# for (nome in nomi_dati[2:length(nomi_dati)]){
#   data <- rbind(data, read.xlsx(nome))
# }

View(data)
str(data)
dim(data)









### SELEZIONE E TRASFORMAZIONE DEI DATI ###

# Pulizia nomi colonne: sostituzione di "..", ".", "%", "/", "()", ecc.
colnames(data) <- gsub("\\.\\.", "_", colnames(data))
colnames(data) <- gsub("\\.", "_", colnames(data))
colnames(data) <- gsub("%", "Anno", colnames(data))
colnames(data) <- gsub("/", "_", colnames(data))
colnames(data) <- gsub("[()]", "", colnames(data))
colnames(data) <- gsub("Dipendenti", "Dipendenti_Anno", colnames(data))
colnames(data) <- gsub("EUR", "EUR_Anno", colnames(data))

# Rimozione di colonne non necessarie
data <- data[, -c(1, 6,16,26,36,46,56,66,76,86,96,106,116,128,131,132,133,135)]

# Aggiunta di una colonna con i primi tre caratteri del codice NACE
data$NACE_Rev_2_first_3 <- substr(data$NACE_Rev_2, 1, 3)

# Ordinamento dei dati in base alla nuova colonna
data <- data[order(data$NACE_Rev_2_first_3), ]

# Sostituzione dei valori "n.d." con NA
data[data == "n.d."] <- NA
data <- data[c(1:94554),]
dim(data)

# Salvataggio dei dati trasformati
# write.xlsx(data, "data.xlsx")

### SUDDIVISIONE DATASET PER CODICE NACE ###

# Suddividere il dataset in una lista di data frame basati sul codice NACE
lista_dataset <- split(data, paste0("NACE_", data$NACE_Rev_2_first_3))
View(lista_dataset)

### TRASFORMAZIONE IN PANEL DATA ###

# trasformiamo ciascun dataset della lista lista_dataset in formato panel data, 
# un formato utile per analisi temporali e longitudinali.

# Carica il pacchetto necessario
library(tidyr)

# Trasformazione in panel data
lista_panel <- lapply(lista_dataset, function(df) {
  df %>%
    pivot_longer(
      cols = matches("(_Anno_|_%)\\d{4}$"),     # Seleziona colonne con "_Anno_" o "_%" seguito da 4 cifre
      names_to = c("Variabile", "Anno"),        # Crea le colonne "Variabile" e "Anno"
      names_pattern = "(.*)_(?:Anno|%)_(\\d{4})", # Estrai nome variabile e anno
      values_to = "Valore"
    ) %>%
    pivot_wider(
      names_from = Variabile,                   # Trasforma "Variabile" in colonne
      values_from = Valore                      # Usa i valori corrispondenti
    )
})

View(lista_panel)

### ANALISI DI CONCENTRAZIONE CON LA VARIABILE RICAVI DELLE VENDITE ###

# Funzione per calcolare gli indici di concentrazione
calcola_indici_ricavi <- function(df) {
  df <- df %>%
    mutate(Ricavi_delle_vendite_migl_EUR = as.numeric(Ricavi_delle_vendite_migl_EUR)) %>% 
    filter(!is.na(Ricavi_delle_vendite_migl_EUR))               
  
  # Calcolo degli indici per ciascun anno
  indici <- df %>%
    group_by(Anno) %>%
    summarize(
      CR4 = sum(sort(Ricavi_delle_vendite_migl_EUR, decreasing = TRUE)[1:4]) / sum(Ricavi_delle_vendite_migl_EUR) * 100,
      HHI = sum((Ricavi_delle_vendite_migl_EUR / sum(Ricavi_delle_vendite_migl_EUR))^2) * 10000,
      Gini = ineq(Ricavi_delle_vendite_migl_EUR, type = "Gini")
    )
  
  return(indici)
}

# Applicazione della funzione agli elementi della lista
# In questa sezione, applichiamo la funzione calcola_indici_ricavi a tutti i 
# dataset nella lista lista_panel. Il risultato è una lista di indici calcolati 
# per ciascun gruppo settoriale.

# Calcolo degli indici per ciascun dataset nella lista
lista_indici_ricavi <- lapply(lista_panel, calcola_indici_ricavi)

# Assegna un nome significativo ai risultati
names(lista_indici_ricavi) <- paste0("Indici_", names(lista_panel))
View(lista_indici_ricavi)

# Visualizza la struttura della lista risultante
str(lista_indici_ricavi)   

### Grafico di Serie Temporali per Indici di Concentrazione basato sui Ricavi di Vendita ###

# Preparazione dei Dati per il Grafico

# Combina i dataset in un unico data frame, aggiungendo un identificatore per ciascun settore
df_indici <- bind_rows(lista_indici_ricavi, .id = "Name")

# Unisce le informazioni sui settori
df_indici <- df_indici %>%
  left_join(Settori, by = "Name")

# Trasforma la colonna Anno in formato Data
df_indici <- df_indici %>%
  mutate(Anno = as.Date(paste0(Anno, "-01-01")))
View(df_indici)

# Grafico per CR4
p1 <- ggplot(df_indici, aes(x = Anno, y = CR4, color = Settore)) +
  geom_line(size = 1) +                          # Linee per le serie temporali
  geom_point(size = 2) +                         # Punti per gli anni
  labs(title = "Serie Temporale dell'Indice CR4 per Settore",
       x = "Anno", y = "CR4 (%)") +              # Titolo e etichette degli assi
  theme_minimal() +                              # Tema grafico minimale
  theme(legend.position = "bottom") +            # Posizione della legenda
  guides(color = guide_legend(ncol = 2))         # Legenda in due colonne per leggibilità

# Visualizza il grafico CR4 come interattivo
ggplotly(p1)

# Grafico per HHI
p2 <- ggplot(df_indici, aes(x = Anno, y = HHI, color = Settore)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Serie Temporale dell'Indice HHI per Settore",
       x = "Anno", y = "Indice HHI") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2))

# Visualizza il grafico HHI come interattivo
ggplotly(p2)

# Grafico per Gini
p3 <- ggplot(df_indici, aes(x = Anno, y = Gini, color = Settore)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Serie Temporale dell'Indice di Gini per Settore",
       x = "Anno", y = "Indice di Gini") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2))

# Visualizza il grafico Gini come interattivo
ggplotly(p3)

# Funzione per tracciare la Curva di Lorenz basato sui Ricavi di Vendita

# Definizione della funzione
calcola_lorenz_per_anno_ricavi <- function(df, nome_df) {
  # Ottieni l'elenco degli anni unici
  anni <- unique(df$Anno)
  
  for (anno in anni) {
    # Filtra i dati per l'anno corrente
    dati_anno <- df %>% filter(Anno == anno) %>% pull(Ricavi_delle_vendite_migl_EUR)
    
    # Traccia la curva di Lorenz per l'anno
    plot(
      Lc(dati_anno), 
      main = paste("Curva di Lorenz -", nome_df, "Anno", anno),
      xlab = "Percentuale cumulativa delle imprese",
      ylab = "Percentuale cumulativa dei ricavi",
      col = "blue", lwd = 2
    )
    
    # Aggiungi la linea di perfetta uguaglianza
    abline(0, 1, col = "red", lty = 2, lwd = 2)
  }
}

# Visualizzazione di Curve di Lorenz

par(mfrow = c(2, 2))  # Dividi la finestra grafica in un layout 2x2

# Applica la funzione a ciascun dataset nella lista
lapply(names(lista_panel), function(nome) {
  calcola_lorenz_per_anno_ricavi(lista_panel[[nome]], nome)
})

par(mfrow = c(1, 1))

### ANALISI DELL'IMPATTO DEGLI INDICI DI CONCENTRAZIONE SULLE PERFORMANCE DELLE IMPRESE ###

# combiniamo le medie annuali delle variabili di performance con gli indici di 
# concentrazione in un unico data frame, pronto per l’analisi statistica.

# Funzione per calcolare la media annuale delle variabili

# Elenco delle variabili su cui calcolare la media
variabili_da_calcolare <- c(
  "Totale_Attività_migl_EUR", "EBITDA_Vendite", "Ricavi_delle_vendite_migl_EUR", 
  "EBITDA_migl_EUR", "Utile_Netto_migl_EUR", "Redditività_delle_vendite_ROS", 
  "Redditività_del_totale_attivo_ROA", "Redditività_del_capitale_proprio_ROE", 
  "Debt_Equity_ratio", 
  "Capitale_sociale_migl_EUR", 
  "TOTALE_DEBITI_migl_EUR", "Dipendenti"
)

# Definisci la funzione per calcolare le medie annuali
calcola_media_per_anno <- function(df) {
  # Converte le variabili specificate in numerico
  df <- df %>%
    mutate(across(all_of(variabili_da_calcolare), as.numeric))
  
  # Raggruppa per anno e calcola la media
  df %>%
    group_by(Anno) %>%
    summarise(across(all_of(variabili_da_calcolare), mean, na.rm = TRUE)) %>%
    ungroup()
}

# Applica la funzione a ciascun data frame nella lista
lista_media_per_anno <- lapply(lista_panel, calcola_media_per_anno)

# Combinazione dei risultati

# Combina le medie annuali in un unico data frame, aggiungendo un identificatore "NACE"
lista_panel_completo <- bind_rows(lista_media_per_anno, .id = "NACE")

# Unisce le informazioni settoriali
lista_panel_completo <- lista_panel_completo %>%
  right_join(Settori, by = "NACE")

# Combina gli indici di concentrazione in un unico data frame
df_indici_ricavi <- bind_rows(lista_indici_ricavi, .id = "Name")

# Unisce le medie annuali con gli indici di concentrazione
df_final <- lista_panel_completo %>%
  left_join(df_indici_ricavi, by = c("Anno", "Name"))

View(df_final)

### Trasformazione delle Variabili Monetarie ###

# In questa sezione, trasformiamo alcune variabili monetarie in logaritmo naturale
# per normalizzare le distribuzioni ed evidenziare relazioni proporzionali 
# nelle analisi successive.

# Calcolo del logaritmo di alcune variabili monetarie
df_final$log_Totale_Attività <- log(df_final$Totale_Attività_migl_EUR)
df_final$log_Capitale_sociale <- log(df_final$Capitale_sociale_migl_EUR)
df_final$log_TOTALE_DEBITI <- log(df_final$TOTALE_DEBITI_migl_EUR)

# Verifica delle trasformazioni
head(df_final[, c("Totale_Attività_migl_EUR", "log_Totale_Attività",
                  "Capitale_sociale_migl_EUR", "log_Capitale_sociale",
                  "TOTALE_DEBITI_migl_EUR", "log_TOTALE_DEBITI")])

### STATISTICA DESCRITTIVA ###
# Questa sezione calcola le statistiche descrittive per le principali variabili
# numeriche del dataset.

# Caricamento del pacchetto richiesto
library(dplyr)

# Seleziona solo le colonne numeriche di interesse
df_stat <- df_final %>% select(
  CR4, HHI, Gini, 
  Redditività_delle_vendite_ROS, Redditività_del_totale_attivo_ROA, Redditività_del_capitale_proprio_ROE,
  EBITDA_Vendite, Debt_Equity_ratio,  
  log_Totale_Attività, log_TOTALE_DEBITI, log_Capitale_sociale
)


# Calcolo delle statistiche descrittive
statistiche_descrittive <- data.frame(
  Media = sapply(df_stat, function(x) round(mean(x, na.rm = TRUE), 3)),
  Deviazione_Standard = sapply(df_stat, function(x) round(sd(x, na.rm = TRUE), 3)),
  Minimo = sapply(df_stat, function(x) round(min(x, na.rm = TRUE), 3)),
  Massimo = sapply(df_stat, function(x) round(max(x, na.rm = TRUE), 3)),
  Mediana = sapply(df_stat, function(x) round(median(x, na.rm = TRUE), 3)),
  Q1 = sapply(df_stat, function(x) round(quantile(x, probs = 0.25, na.rm = TRUE), 3)),
  Q3 = sapply(df_stat, function(x) round(quantile(x, probs = 0.75, na.rm = TRUE), 3))
)

# Visualizza le statistiche descrittive
print(statistiche_descrittive)
View(statistiche_descrittive)
kable(statistiche_descrittive, caption = "Statistiche Descrittive delle Variabili Numeriche")
# write.xlsx(statistiche_descrittive, "C:/UNIVERSITA' magistrale/ANNO 2/PERIODO 2/ECONOMIA E DINAMICA INDUSTRIALE/Progetto R/Nostro lavoro/stat_des.xlsx")

### CALCOLO DELLA MATRICE DI CORRELAZIONE ###

# Questa sezione calcola la matrice di correlazione per le variabili numeriche 
# selezionate e ne visualizza i risultati tramite un grafico.

# Calcola la matrice di correlazione
View(df_stat)
cor_matrix <- cor(df_stat, use = "complete.obs", method="pearson")
cor_spearman <- cor(df_stat, use = "complete.obs", method = "spearman")

# Visualizza la matrice di correlazione
print(cor_matrix)
print(cor_spearman)

# Grafico della Matrice di Correlazione

# Carica il pacchetto necessario
library(corrplot)

new_names <- c("CR4", "HHI", "Gini", "ROS", "ROA", 
               "ROE", "EBITDA", "Debt_Equity", "log_Tot_Att", "log_Tot_Debt", "log_Cap_Soc")
colnames(cor_matrix) <- new_names
rownames(cor_matrix) <- new_names

# Genera il grafico di correlazione
corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                   tl.col = "black", tl.srt = 60,   # Colore e orientamento delle etichette
                   tl.cex = 0.7,  # Dimensione etichette
                   number.cex = 0.7,  # Dimensione numeri
                   addCoef.col = "black", mar = c(0, 0, 0, 0)) # Mostra i coefficienti di correlazione

colnames(cor_spearman) <- new_names
rownames(cor_spearman) <- new_names
corrplot::corrplot(cor_spearman, method = "color", type = "upper", 
                   tl.col = "black", tl.srt = 60,   # Colore e orientamento delle etichette
                   tl.cex = 0.7,  # Dimensione etichette
                   number.cex = 0.7,  # Dimensione numeri
                   addCoef.col = "black", mar = c(0, 0, 0, 0)) # Mostra i coefficienti di correlazione


### PREPARAZIONE DEL DATASET PANEL ###

# In questa sezione, creiamo un dataset in formato panel, selezionando solo 
# le variabili rilevanti per l’analisi delle performance aziendali e degli 
# indici di concentrazione.

# Seleziona le variabili rilevanti per l'analisi
df_panel_data <- df_final %>% select(
  Anno, NACE, Settore, Dipendenti, CR4, HHI, Gini, 
  Redditività_delle_vendite_ROS, Redditività_del_totale_attivo_ROA, Redditività_del_capitale_proprio_ROE,
  EBITDA_Vendite, Debt_Equity_ratio,  
  log_Totale_Attività, log_TOTALE_DEBITI, log_Capitale_sociale
)
View(df_panel_data)

#Serie Temporali delle Variabili di Performance

# In questa sezione, creiamo grafici delle serie temporali per quattro variabili 
# di performance aziendale, suddivise per settore, e li rendiamo 
# interattivi con plotly.  

# Prepara il dataset
df_performace <- df_panel_data %>%
  mutate(Anno = as.Date(paste0(Anno, "-01-01")))

# Lista delle variabili di performance
performance_vars <- c("Redditività_delle_vendite_ROS", "Redditività_del_capitale_proprio_ROE",
                      "Redditività_del_totale_attivo_ROA", "EBITDA_Vendite")

# Crea una lista per salvare i grafici
plots <- list()

# Ciclo per generare un grafico per ogni variabile di performance
for (var in performance_vars) {
  p <- ggplot(df_performace, aes(x = Anno, y = .data[[var]], color = Settore)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = paste("Serie Temporale di", var, "per Settore"),
         x = "Anno", y = var) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(ncol = 2))  # Organizza la legenda in 2 colonne
  
  # Converte in grafico interattivo e salva nella lista
  plots[[var]] <- ggplotly(p)
}

# Visualizza i grafici
plots$Redditività_delle_vendite_ROS  # Visualizza il grafico di una variabile
plots$Redditività_del_capitale_proprio_ROE  
plots$Redditività_del_totale_attivo_ROA  
plots$EBITDA_Vendite

### RELAZIONE TRA INDICI E VARIABILI DI PERFORMANCE AZIENDALI ###

# Questa sezione esplora la relazione tra gli indici di concentrazione e 
# le variabili di performance aziendale tramite un grafico a griglia con 
# regressioni lineari.

# Caricamento del pacchetto necessario
library(tidyr)
library(ggplot2)

# Trasformazione in formato long

colnames(df_panel_data) <- gsub("Redditività_delle_vendite_ROS", "ROS", colnames(df_panel_data))
colnames(df_panel_data) <- gsub("Redditività_del_capitale_proprio_ROE", "ROE", colnames(df_panel_data))
colnames(df_panel_data) <- gsub("Redditività_del_totale_attivo_ROA", "ROA", colnames(df_panel_data))

df_long <- df_panel_data %>%
  pivot_longer(cols = c("ROS", "ROE", 
                        "ROA", "EBITDA_Vendite"),
               names_to = "Performance", values_to = "ValorePerformance") %>%
  pivot_longer(cols = c("CR4", "HHI", "Gini"), 
               names_to = "Indice", values_to = "ValoreIndice")
View(df_long)

# Creazione del grafico con facet_grid
p <- ggplot(df_long, aes(x = ValoreIndice, y = ValorePerformance)) +
  geom_point(alpha = 0.6) +  # Punti con trasparenza per evidenziare densità
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Aggiunge retta di regressione
  labs(title = "Relazione tra Indici e Variabili di Performance Aziendale",
       x = "Valore Indice",
       y = "Valore Performance") +
  theme_minimal() +
  facet_grid(Performance ~ Indice, scales = "free")  # Crea una griglia con righe e colonne

# Visualizza il grafico
print(p)

colnames(df_panel_data) <- gsub("ROS", "Redditività_delle_vendite_ROS", colnames(df_panel_data))
colnames(df_panel_data) <- gsub("ROE", "Redditività_del_capitale_proprio_ROE", colnames(df_panel_data))
colnames(df_panel_data) <- gsub("ROA","Redditività_del_totale_attivo_ROA", colnames(df_panel_data))

### ANALISI PANEL DATA ###

# Questa sezione implementa un’analisi panel data per esplorare la relazione 
# tra gli indici di concentrazione e le variabili di performance aziendale, 
# con e senza variabili di controllo.

# Caricamento dei pacchetti richiesti
library(plm)
library(lmtest)
library(car)
library(stargazer)
library(readxl)

# Trasformazione delle colonne 'Anno' e 'NACE' in fattori
df_panel_data$Anno <- as.factor(df_panel_data$Anno)
df_panel_data$NACE <- as.factor(df_panel_data$NACE)

# Liste delle variabili di performance e degli indici
performance_vars <- c("Redditività_delle_vendite_ROS", "Redditività_del_capitale_proprio_ROE",
                      "Redditività_del_totale_attivo_ROA", "EBITDA_Vendite")
indici_vars <- c("CR4", "HHI", "Gini")

# Liste per raccogliere i modelli
modelli_senza_controllo <- list()
modelli_con_controllo <- list()

### MODELLI SENZA VARIABILI DI CONTROLLO ###

# Loop per costruire i modelli
for (perf_var in performance_vars) {
  for (indice_var in indici_vars) {
    # Formula del modello
    formula_senza <- as.formula(paste(perf_var, "~", indice_var))
    
    # Modello panel 'within' senza variabili di controllo
    model_senza <- plm(formula_senza, data = df_panel_data, index = c("NACE", "Anno"), model = "within")
    
    # Salva il modello nella lista
    modelli_senza_controllo[[paste(perf_var, indice_var, sep = "_")]] <- model_senza
  }
}

### MODELLI CON VARIABILI DI CONTROLLO ###

# Loop per costruire i modelli
for (perf_var in performance_vars) {
  for (indice_var in indici_vars) {
    # Formula del modello
    formula_con <- as.formula(paste(perf_var, "~", indice_var, "+ Debt_Equity_ratio + log_Totale_Attività + log_TOTALE_DEBITI + log_Capitale_sociale"))
    
    # Modello panel 'within' con variabili di controllo
    model_con <- plm(formula_con, data = df_panel_data, index = c("NACE", "Anno"), model = "within")
    
    # Salva il modello nella lista
    modelli_con_controllo[[paste(perf_var, indice_var, sep = "_")]] <- model_con
  }
}

# Visualizzazione dei risultati

# Visualizza i risultati dei modelli senza variabili di controllo
stargazer(modelli_senza_controllo, type = "text", title = "Risultati dei Modelli senza Variabili di Controllo")
# Visualizza i risultati dei modelli con variabili di controllo
stargazer(modelli_con_controllo, type = "text", title = "Risultati dei Modelli con Variabili di Controllo")

### MODELLI PANEL DATA E TEST DIAGNOSTICI ### 

# Questa sezione utilizza una funzione personalizzata per eseguire modelli 
# panel data e condurre test diagnostici, tra cui Hausman, Breusch-Pagan e Wooldridge, 
# per valutare la validità dei modelli.

# Caricamento dei pacchetti richiesti
library(plm)
library(lmtest)
library(car)

# Definisci le variabili di performance, gli indici e le variabili di controllo
performance_vars <- c("Redditività_delle_vendite_ROS", "Redditività_del_capitale_proprio_ROE", "Redditività_del_totale_attivo_ROA", "EBITDA_Vendite")
indices <- c("CR4", "HHI", "Gini")
control_vars <- c("Debt_Equity_ratio", "log_Totale_Attività", "log_TOTALE_DEBITI", "log_Capitale_sociale")

# Funzione per eseguire i modelli e i test
run_panel_tests <- function(dep_var, indep_var, controls = NULL) {
  # Definisci le formule per i due modelli
  if (is.null(controls)) {
    formula <- as.formula(paste(dep_var, "~", indep_var))
  } else {
    formula <- as.formula(paste(dep_var, "~", indep_var, "+", paste(controls, collapse = " + ")))
  }
  
  # Modello a effetti fissi
  fixed_model <- plm(formula, data = df_panel_data, model = "within")
  
  # Modello a effetti casuali
  random_model <- plm(formula, data = df_panel_data, model = "random")
  
  # Test di Hausman
  hausman_test <- tryCatch(phtest(fixed_model, random_model), error = function(e) NA)
  
  # Modello OLS per il test di Breusch-Pagan
  ols_model <- plm(formula, data = df_panel_data, model = "pooling")
  
  # Test di Breusch-Pagan per gli effetti casuali
  bp_effects_test <- tryCatch(plmtest(ols_model, type = "bp"), error = function(e) NA)
  
  # Test di Wooldridge per autocorrelazione
  wooldridge_test <- tryCatch(pbgtest(fixed_model), error = function(e) NA)
  
  # Test di Breusch-Pagan per eteroschedasticità
  bp_hetero_test <- tryCatch(bptest(fixed_model), error = function(e) NA)
  
  # Ritorna i risultati
  list(
    Hausman_Test = hausman_test,
    BP_Effects_Test = bp_effects_test,
    Wooldridge_Test = wooldridge_test,
    BP_Hetero_Test = bp_hetero_test
  )
}

# Esecuzione dei Test per Modelli Panel

# Questa sezione presenta i risultati dei test diagnostici sui modelli panel 
# per diverse combinazioni di variabili di performance e indici di concentrazione.

# Caricamento dei pacchetti
library(plm)
library(lmtest)
library(car)
library(kableExtra)

# Variabili di performance, indici e variabili di controllo
performance_vars <- c("Redditività_delle_vendite_ROS", "Redditività_del_capitale_proprio_ROE", "Redditività_del_totale_attivo_ROA", "EBITDA_Vendite")
indices <- c("CR4", "HHI", "Gini")
control_vars <- c("Debt_Equity_ratio", "log_Totale_Attività", "log_TOTALE_DEBITI", "log_Capitale_sociale")

# Esegui i modelli e i test per ogni combinazione
results <- list()

for (dep_var in performance_vars) {
  for (indep_var in indices) {
    # Modello con solo indice
    no_controls <- run_panel_tests(dep_var, indep_var)
    # Modello con indice + variabili di controllo
    with_controls <- run_panel_tests(dep_var, indep_var, control_vars)
    
    # Salva i risultati
    results[[paste(dep_var, indep_var, "No_Controls")]] <- no_controls
    results[[paste(dep_var, indep_var, "With_Controls")]] <- with_controls
  }
}

View(results)

### pvalue dei test diagnostici ####


res0 = results

nomi_righe = names(res0)
nomi_colonne = names(res0[[1]])

nomi_righe = nomi_righe %>% 
  {gsub('Redditività_delle_vendite_ROS','ROS',.)} %>%
  {gsub('Redditività_del_capitale_proprio_ROE','ROE',.)} %>%
  {gsub('Redditività_del_capitale_proprio_ROE','ROE',.)} %>%
  {gsub('Redditività_del_totale_attivo_ROA','ROA',.)} %>%
  {gsub('EBITDA_Vendite','EBITDA',.)} %>%
  {gsub('No_Controls','noctrl',.)} %>%
  {gsub('With_Controls','ctrl',.)}


nomi_righe

nr = length(nomi_righe)
nc = length(nomi_colonne)

tabPV = matrix(NA, nrow=nr, ncol=nc)
tabTest = matrix(NA, nrow=nr, ncol=nc)

rownames(tabPV) = rownames(tabTest) = nomi_righe
colnames(tabPV) = colnames(tabTest) = nomi_colonne

for (r in 1:nr){
  for (c in 1:nc){
    row = res0[[r]]
    cell = row[[c]]
    pval = cell$p.value
    test = cell$statistic
    
    tabPV[r,c] = pval
    tabTest[r,c] = test
  }
}




library(pheatmap)
pheatmap(tabPV, 
         color = colorRampPalette(c("white",
                                    "lightblue",
                                    "lightgreen", 
                                    "yellow", 
                                    "red"))(50), 
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE, 
         fontsize_number = 10,
         angle_col = 0,
         number_format = "%.4f")  




daTenere = c('ROS CR4 ctrl', 
             'ROS HHI ctrl',
             'ROS Gini ctrl',
             'ROS Gini noctrl',
             'ROE Gini ctrl',
             'ROE Gini noctrl',
             'EBITDA Gini ctrl'
)


pheatmap(tabPV[daTenere,], 
         color = colorRampPalette(c("white",
                                    #"lightblue",
                                    #"lightgreen", 
                                    #"yellow", 
                                    "red"))(50), 
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE, 
         fontsize_number = 10,
         angle_col = 0,
         number_format = "%.4f",
         main='p-value dei test sui modelli con almeno un coefficiente significativo al 5%')  







# Presentazione dei Risultati


# Crea il dataframe dei risultati
results <- data.frame(
  Modello = rep(c(
    "Redditività delle vendite ROS CR4 (No Controls)", 
    "Redditività delle vendite ROS CR4 (With Controls)",
    "Redditività delle vendite ROS HHI (No Controls)",
    "Redditività delle vendite ROS HHI (With Controls)",
    "Redditività delle vendite ROS GINI (No Controls)",
    "Redditività delle vendite ROS GINI (With Controls)",
    "Redditività del capitale proprio ROE CR4 (No Controls)", 
    "Redditività del capitale proprio ROE CR4 (With Controls)",
    "Redditività del capitale proprio ROE HHI (No Controls)",
    "Redditività del capitale proprio ROE HHI (With Controls)",
    "Redditività del capitale proprio ROE GINI (No Controls)",
    "Redditività del capitale proprio ROE GINI (With Controls)",
    "Redditività del totale attivo ROA CR4 (No Controls)", 
    "Redditività del totale attivo ROA CR4 (With Controls)", 
    "Redditività del totale attivo ROA HHI (No Controls)",
    "Redditività del totale attivo ROA HHI (With Controls)",
    "Redditività del totale attivo ROA GINI (No Controls)",
    "Redditività del totale attivo ROA GINI (With Controls)",
    "EBITDA Vendite CR4 (No Controls)",
    "EBITDA Vendite CR4 (With Controls)",
    "EBITDA Vendite HHI (No Controls)",
    "EBITDA Vendite HHI (With Controls)",
    "EBITDA Vendite GINI (No Controls)",
    "EBITDA Vendite GINI (With Controls)"
  ), each = 4),
  Test = rep(c("Hausman Test", "BP Effects Test", "Wooldridge Test", "BP Hetero Test"), times = 24),
  Valore = c(
    0.001, 10.718, 56.914, 3.160, 
    4.938, 8.307, 53.307, 6.729, 
    0.029, 10.755, 57.170, 0.484, 
    2.377, 8.538, 49.798, 6.117, 
    0.207, 13.689, 48.019, 1.516, 
    1.819, 12.056, 37.902, 3.781, 
    0.031, 0.506, 45.960, 6.380, 
    26.940, 0.995, 23.006, 8.445, 
    0.033, 0.361, 50.037, 3.947, 
    9.563, 0.640, 24.463, 7.637, 
    0.804, 0.221, 50.879, 0.023, 
    12.634, 0.508, 30.730, 2.648, 
    0.009, 1.115, 45.952, 9.319, 
    10.068, 0.217, 44.301, 14.008, 
    0.020, 1.110, 45.281, 11.089,
    4.827, 0.072, 44.685, 12.738, 
    0.431, 1.059, 43.273, 1.250, 
    5.408, 0.128, 39.004, 11.870, 
    0.053, 3.141, 44.626, 14.738,
    46.053, 2.502, 38.778, 29.818, 
    0.328, 3.031, 46.804, 7.206, 
    26.918, 3.916, 32.270, 20.382, 
    0.348, 2.646, 46.667, 0.023, 
    6.319, 1.531, 42.519, 25.926
  ),
  p_value = c(
    0.969, 0.001, 0.000, 0.075, 
    0.424, 0.004, 0.000, 0.242,
    0.865, 0.001, 0.000, 0.487, 
    0.795, 0.003, 0.000, 0.295, 
    0.649, 0.000, 0.000, 0.218, 
    0.874, 0.001, 0.001, 0.581,
    0.859, 0.477, 0.000, 0.012, 
    0.000, 0.318, 0.084, 0.133, 
    0.857, 0.548, 0.000, 0.047,
    0.089, 0.424, 0.058, 0.178, 
    0.370, 0.638, 0.000, 0.879, 
    0.027, 0.476, 0.010, 0.754, 
    0.926, 0.291, 0.000, 0.002, 
    0.073, 0.642, 0.000, 0.016, 
    0.887, 0.292, 0.000, 0.001, 
    0.437, 0.788, 0.000, 0.026, 
    0.511, 0.303, 0.000, 0.264, 
    0.368, 0.720, 0.001, 0.037, 
    0.817, 0.076, 0.000, 0.000, 
    0.000, 0.114, 0.001, 0.000, 
    0.567, 0.082, 0.000, 0.007, 
    0.000, 0.048, 0.006, 0.001, 
    0.555, 0.104, 0.000, 0.879, 
    0.276, 0.216, 0.000, 0.000
  )
)

View(results)
Risultati <- data.frame(nrow="")
results1 <- cbind(results, Risultati)
colnames(results1) <- gsub("nrow", "Risultati", colnames(results1))
View(results1)

for(i in 1:96){
  if(results1$Test[i] == "Hausman Test"){
    if(results1$p_value[i]<0.05){
      results1$Risultati[i]="Modelli a effetti fissi preferibile"
    }else if(results1$p_value[i]>=0.05){
      results1$Risultati[i]="Modelli a effetti casuali preferibile"
    }
  }else if(results1$Test[i] == "BP Effects Test"){
    if(results1$p_value[i]<0.05){
      results1$Risultati[i]="Effetti non significativi, OLS potrebbe essere adeguato"
    }else if(results1$p_value[i]>=0.05){
      results1$Risultati[i]="Effetti significativi, panel raccomandato"
    }
  }else if(results1$Test[i] == "Wooldridge Test"){
    if(results1$p_value[i]<0.05){
      results1$Risultati[i]="Presenza di autocorrelazione"
    }else if(results1$p_value[i]>=0.05){
      results1$Risultati[i]="Assenza di autocorrelazione"
    }
  }else if(results1$Test[i] == "BP Hetero Test"){
    if(results1$p_value[i]<0.05){
      results1$Risultati[i]="Presenza di eteroschedasticità"
    }else if(results1$p_value[i]>=0.05){
      results1$Risultati[i]="Assenza di eteroschedasticità"
    }
  }
}

# Stampa la tabella con Kable
kable(results1, format = "html", caption = "Risultati dei Test per Modelli Panel Completi") %>%
  kable_styling(full_width = F)
View(results1)

write.xlsx(results1, "test_completo.xlsx")









