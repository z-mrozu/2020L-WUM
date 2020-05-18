library(shiny)

set.seed(2137)

ui <- fluidPage(
  
  titlePanel("Analiza modeli"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("restart", "RESTART"),
      radioButtons("label_choice", "Label to prognoze:", choices = c("Biopsy", "Citology", "Schiller", "Hinselmann", "Ktorekolwiek"), selected = "Ktorekolwiek"),
      textOutput("podzial_info_train"),
      textOutput("podzial_info_val"),
      textOutput("podzial_info_test"),
      sliderInput("times", "Find best of:", min = 1, max = 100, value = 2),
      sliderInput("beta", "Beta in FBeta:", min = 0, max = 100, value = 20),
      sliderInput("M", "CrossValidation number:", min = 2, max = 10, value = 4),
      sliderInput("multiply_times", "Create more positive data:", min = 0, max = 20, value = 0),
      textInput("num_part_bins", "Number_of_sexual_partners_bins:", value = "1, 2, 3, 4, 5"),
      textInput("num_part_label", "Number_of_sexual_partners_labels:", value = "1, 2, 3, 4, 5"),
      textInput("num_preg_bins", "Num_of_pregnancies_bins:", value = "1, 2, 3, 4, 5, 6"),
      textInput("num_preg_label", "Num_of_pregnancies_labels:", value = "0, 1, 2, 3, 4, 5, 6"),
      textInput("smoke_bins", "Smokes_packs_year_bins:", value = "0.0001, 1.5, 3.5"),
      textInput("smoke_label", "Smokes_packs_year_labels:", value = "0, 1, 2, 3"),
      textInput("Cont_bins", "Hormonal_Contraceptives_years_bins:", value = "0.0001, 2, 5"),
      textInput("Cont_label", "Hormonal_Contraceptives_years_labels:", value = "0, 1, 2, 3"),
      textInput("IUD_bins", "IUD_years_bins:", value = "1, 2, 3, 4, 5, 6, 7, 8, 9"),
      textInput("IUD_label", "IUD_years_labels:", value = "0, 1, 2, 3, 4, 5, 6, 7, 8, 9"),
      textInput("first_bins", "STDs_Time_since_first_diagnosis_bins:", value = "1, 2, 3, 4, 5, 6, 7, 8, 13"),
      textInput("first_label", "STDs_Time_since_first_diagnosis_labels:", value = "0, 1, 2, 3, 4, 5, 6, 7, 11, 18")
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel(title = "kknn",
                 sliderInput("k", "k in knn:", min = 1, max = 100, value = 4),
                 textOutput("granica_knn"), textOutput("AUC_knn"), textOutput("FBeta_knn"), textOutput("W_R_knn"), textOutput("AUPRC_knn"), tableOutput("Table_knn"), plotOutput("ROC_plot_kknn")),
        tabPanel(title = "las losowy",
                 sliderInput("ntree", "Liczba drzew w lesie:", min = 1, max = 5000, value = 500),
                 sliderInput("maxnodes", "Glebokosc drzew:", min = 1, max = 10, value = 6),
                 textOutput("granica_las"), textOutput("AUC_las"), textOutput("FBeta_las"), textOutput("W_R_las"), textOutput("AUPRC_las"), tableOutput("Table_las"), plotOutput("ROC_plot_las")),
        tabPanel(title = "logistic regresion", textOutput("granica_log"), textOutput("AUC_log"), textOutput("FBeta_log"), textOutput("W_R_log"), textOutput("AUPRC_log"), tableOutput("Table_log"), plotOutput("ROC_plot_log")),
        tabPanel(title = "decision tree", textOutput("granica_tree"), textOutput("AUC_tree"), textOutput("FBeta_tree"), textOutput("W_R_tree"), textOutput("AUPRC_tree"), tableOutput("Table_tree"), plotOutput("ROC_plot_tree"))
      )
    )
  )
)



# zawartosc funkcje.R
library("jsonlite")
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(mice)
library(mlr)
library(kknn)
library(pROC)
library(neuralnet)
library(ddpcr)
library(MLmetrics)
library(stringr)
library(auprc)

pobrane_dane <- function(){
  # Funckja pobiera dane i zapisuje je w formie takiej, jak są udostępnione na stronie źródłowej
  
  json_file <- 'https://datahub.io/machine-learning/cervical-cancer/datapackage.json'
  json_data <- fromJSON(paste(readLines(json_file), collapse=""))
  
  for(i in 1:length(json_data$resources$datahub$type)){
    if(json_data$resources$datahub$type[i]=='derived/csv'){
      path_to_file = json_data$resources$path[i]
      data <- read.csv(url(path_to_file))
    }
  }
  
  return(data)
}

dane_obrobione <- function(data, mice_method = "pmm", mice_completion = 1,
                           
                           Number_of_sexual_partners_bins = c(1, 2, 3, 4, 5, Inf),
                           Number_of_sexual_partners_labels = c(1, 2, 3, 4, 5),
                           
                           Num_of_pregnancies_bins = c(-Inf, 1, 2, 3, 4, 5, 6, Inf),
                           Num_of_pregnancies_labels = c(0, 1, 2, 3, 4, 5, 6),
                           
                           Smokes_packs_year_bins = c(-Inf, 0.0001, 1.5, 3.5, Inf),
                           Smokes_packs_year_labels = c(0, 1, 2, 3),
                           
                           Hormonal_Contraceptives_years_bins = c(-Inf, 0.0001, 2, 5, Inf),
                           Hormonal_Contraceptives_years_labels = c(0, 1, 2, 3),
                           
                           IUD_years_bins = c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 9, Inf),
                           IUD_years_labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                           
                           STDs_Time_since_first_diagnosis_bins = c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 13, Inf),
                           STDs_Time_since_first_diagnosis_labels = c(0, 1, 2, 3, 4, 5, 6, 7, 11, 18)
){
  # Funkcja obrabia dane i przygotowuje je do PCA
  #
  # data - dane do obróbki
  # mice_method - metoda do mice'a
  # mice_completion - to też do mice'a, przy kończeniu imputacji
  #
  # Następujące argumenty to kubełki podziałów kolumn i ich etykiety
  # Number_of_sexual_partners_bins = c(-Inf, 1, 2, 3, 4, 5, Inf),
  # Number_of_sexual_partners_labels = c(0, 1, 2, 3, 4, 5),
  # 
  # Num_of_pregnancies_bins = c(-Inf, 1, 2, 3, 4, 5, 6, Inf),
  # Num_of_pregnancies_labels = c(0, 1, 2, 3, 4, 5, 6),
  # 
  # Smokes_packs_year_bins = c(-Inf, 0.0001, 1.5, 3.5, Inf),
  # Smokes_packs_year_labels = c(0, 1, 2, 3),
  # 
  # Hormonal_Contraceptives_years_bins = c(-Inf, 0.0001, 2, 5, Inf),
  # Hormonal_Contraceptives_years_labels = c(0, 1, 2, 3),
  # 
  # IUD_years_bins = c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 9, Inf),
  # IUD_years_labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  # 
  # STDs_Time_since_first_diagnosis_bins = c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 13, Inf),
  # STDs_Time_since_first_diagnosis_labels = c(0, 1, 2, 3, 4, 5, 6, 7, 11, 18)
  
  
  # Wyrzucenie danych bez danych o STDs (a więc uważanych przez nas za zbędne)
  data_wypierdolone <- data %>% filter(!is.na(STDs))
  
  # Wyrzucenie danych błędnych - osoby które miały inicjację seksualną w wieku wyższym niż wiek obecny
  data_wypierdolone <- data_wypierdolone  %>% filter(Age >= First.sexual.intercourse)
  
  # Wyrzucenie kolumn, które wg. nas nic nie wnoszą
  data_wyjebane <- data_wypierdolone %>% select(-STDs.genital.herpes, -STDs.pelvic.inflammatory.disease, -STDs.molluscum.contagiosum, -STDs.Hepatitis.B, -IUD, -Smokes, -Smokes..years., -STDs, -STDs..number., -STDs..Time.since.last.diagnosis, -STDs.AIDS, -STDs.cervical.condylomatosis, -Hormonal.Contraceptives, -STDs.HPV)
  
  # Stworzenie piątej zmiennej celu - Ktorekolwiek
  data_wyjebane$Ktorekolwiek <- ifelse(data_wyjebane$Schiller == 1 | data_wyjebane$Hinselmann == 1 | data_wyjebane$Citology == 1 | data_wyjebane$Biopsy == 1, 1, 0)
  
  # usunięcie zmiennych celu
  labels <- data_wyjebane %>% select(Biopsy, Citology, Schiller, Hinselmann, Ktorekolwiek)
  data_wyjebane <- data_wyjebane %>% select(-Biopsy, -Citology, -Schiller, -Hinselmann, -Ktorekolwiek)
  
  # Kategoryzacja danych, gdzie raczej nie będzie kubełków
  data_wyjebane$STDs..Number.of.diagnosis <- (data_wyjebane$STDs..Number.of.diagnosis==0) %>% ifelse(0, 1)
  data_wyjebane$STDs.vulvo.perineal.condylomatosis <- (data_wyjebane$STDs.vulvo.perineal.condylomatosis==2) %>% ifelse(1, data_wyjebane$STDs.vulvo.perineal.condylomatosis)
  data_wyjebane$STDs..Time.since.first.diagnosis <- data_wyjebane$STDs..Time.since.first.diagnosis %>% is.na %>% ifelse(0, data_wyjebane$STDs..Time.since.first.diagnosis)
  
  # faktoryzacja
  data_wyjebane$STDs.vaginal.condylomatosis <- as.factor(data_wyjebane$STDs.vaginal.condylomatosis)
  data_wyjebane$STDs.vulvo.perineal.condylomatosis <- as.factor(data_wyjebane$STDs.vulvo.perineal.condylomatosis)
  data_wyjebane$STDs.syphilis <- as.factor(data_wyjebane$STDs.syphilis)
  data_wyjebane$STDs.HIV <- as.factor(data_wyjebane$STDs.HIV)
  data_wyjebane$STDs..Number.of.diagnosis <- as.factor(data_wyjebane$STDs..Number.of.diagnosis)
  data_wyjebane$Dx.Cancer <- as.factor(data_wyjebane$Dx.Cancer)
  data_wyjebane$Dx.CIN <- as.factor(data_wyjebane$Dx.CIN)
  data_wyjebane$Dx.HPV <- as.factor(data_wyjebane$Dx.HPV)
  data_wyjebane$Dx <- as.factor(data_wyjebane$Dx)
  data_wyjebane$STDs.condylomatosis <- as.factor(data_wyjebane$STDs.condylomatosis)
  
  # mice - imputacja
  data_preimputed <- mice(data_wyjebane, m = 3, method = mice_method, maxit = 10, printFlag = FALSE)
  data_imputowane <- complete(data_preimputed, mice_completion)
  
  # kategoryzacja i faktoryzacja, ale z kubełkami; zamiana faktor na numeric
  data_imputowane$Number.of.sexual.partners <- cut(data_imputowane$Number.of.sexual.partners, Number_of_sexual_partners_bins, Number_of_sexual_partners_labels, right = FALSE)
  data_imputowane$Number.of.sexual.partners <- as.numeric(levels(data_imputowane$Number.of.sexual.partners))[data_imputowane$Number.of.sexual.partners]
  
  data_imputowane$Num.of.pregnancies <- cut(data_imputowane$Num.of.pregnancies, Num_of_pregnancies_bins, Num_of_pregnancies_labels, right = FALSE)
  data_imputowane$Num.of.pregnancies <- as.numeric(levels(data_imputowane$Num.of.pregnancies))[data_imputowane$Num.of.pregnancies]
  
  data_imputowane$Smokes..packs.year. <- cut(data_imputowane$Smokes..packs.year., Smokes_packs_year_bins, Smokes_packs_year_labels, right = FALSE)
  data_imputowane$Smokes..packs.year. <- as.numeric(levels(data_imputowane$Smokes..packs.year.))[data_imputowane$Smokes..packs.year.]
  
  data_imputowane$Hormonal.Contraceptives..years. <- cut(data_imputowane$Hormonal.Contraceptives..years., Hormonal_Contraceptives_years_bins, Hormonal_Contraceptives_years_labels, right = FALSE)
  data_imputowane$Hormonal.Contraceptives..years. <- as.numeric(levels(data_imputowane$Hormonal.Contraceptives..years.))[data_imputowane$Hormonal.Contraceptives..years.]
  
  data_imputowane$IUD..years. <- data_imputowane$IUD..years. %>% ceiling()
  data_imputowane$IUD..years. <- cut(data_imputowane$IUD..years., IUD_years_bins, IUD_years_labels, right = FALSE)
  data_imputowane$IUD..years. <- as.numeric(levels(data_imputowane$IUD..years.))[data_imputowane$IUD..years.]
  
  data_imputowane$STDs..Time.since.first.diagnosis <- cut(data_imputowane$STDs..Time.since.first.diagnosis, STDs_Time_since_first_diagnosis_bins, STDs_Time_since_first_diagnosis_labels, right = FALSE)
  data_imputowane$STDs..Time.since.first.diagnosis <- as.numeric(levels(data_imputowane$STDs..Time.since.first.diagnosis))[data_imputowane$STDs..Time.since.first.diagnosis]
  
  # Stworzenie kolumny dawnosc_inicjacji
  data_imputowane$dawnosc_inicjacji <- data_imputowane$Age - data_imputowane$First.sexual.intercourse
  data_imputowane$First.sexual.intercourse <- NULL
  
  # dodanie zmiennych celu do wyniku jako atrybut
  attr(data_imputowane, "labels") <- labels
  
  return(data_imputowane)
}

wycentruj <- function(num, srednie, odchylenia){
  (num - srednie)/odchylenia
}

powiel_rekordy_z_rakiem <- function(data, multiplication_value, label){
  # Powielenie kolumn z label = 1
  
  data_do_powielenia <- data[label == 1, ]
  as.data.frame(lapply(data_do_powielenia, function(x) rep(x, multiplication_value)))
  data <- rbind(data, data_do_powielenia)
  
  return(data)
}

# tu sie zaczyna server shiny
server <- function(input, output) {
  data_src <- reactive({
    input$restart # bedzie sie restartowac
    
    text_to_number <- function(text){
      tmp <- str_split(text, " ")[[1]]
      tmp1 <- numeric(0)
      quiet(
        for(i in 1:length(tmp)){
        tmp1[i] <- as.numeric(tmp[i])
        if(is.na(tmp1[i])){
          tmp1[i] <- tmp[i] %>% substr(1, nchar(tmp[i])-1) %>% as.numeric()
        }
      })
      tmp1
    }
    
    out <- dane_obrobione(pobrane_dane(), mice_method = "pmm", mice_completion = 1,
                          
                          Number_of_sexual_partners_bins = c(text_to_number(input$num_part_bins), Inf),
                          Number_of_sexual_partners_labels = text_to_number(input$num_part_label),
                          
                          Num_of_pregnancies_bins = c(-Inf, text_to_number(input$num_preg_bins), Inf),
                          Num_of_pregnancies_labels = text_to_number(input$num_preg_label),
                          
                          Smokes_packs_year_bins = c(-Inf, text_to_number(input$smoke_bins), Inf),
                          Smokes_packs_year_labels = text_to_number(input$smoke_label),
                          
                          Hormonal_Contraceptives_years_bins = c(-Inf, text_to_number(input$Cont_bins), Inf),
                          Hormonal_Contraceptives_years_labels = text_to_number(input$Cont_label),
                          
                          IUD_years_bins = c(-Inf, text_to_number(input$IUD_bins), Inf),
                          IUD_years_labels = text_to_number(input$IUD_label),
                          
                          STDs_Time_since_first_diagnosis_bins = c(-Inf, text_to_number(input$first_bins), Inf),
                          STDs_Time_since_first_diagnosis_labels = text_to_number(input$first_label) )
    out
  })
  
  # wczytanie danych
  label <- reactive({
    data <- data_src()
    label <- attr(data, "labels")
    label[,input$label_choice] %>% as.factor()
  })
  
  
  training_set <- reactive({ data_src() })
  m <- reactive({ (training_set() %>% dim)[1] })
  ktore <- reactive({ sample(m(), floor(m()*0.7)) })
  train <- reactive({
    tmp <- training_set()[ktore(),]
    tmp <- tmp %>% cbind(label()[ktore()] %>% as.numeric() - 1) %>% as.data.frame()
    tmp$label <- tmp[,length(colnames(tmp))] %>% as.factor()
    tmp[,length(colnames(tmp))-1] <- NULL
    
    tmp_numeryczne <- tmp %>% select(Age, Number.of.sexual.partners, Num.of.pregnancies, Smokes..packs.year.,
                                     Hormonal.Contraceptives..years.,IUD..years.,
                                     STDs..Time.since.first.diagnosis, dawnosc_inicjacji)
    tmp_faktory <- tmp %>% select(-Age, -Number.of.sexual.partners, -Num.of.pregnancies, -Smokes..packs.year.,
                                  -Hormonal.Contraceptives..years., -IUD..years.,
                                  -STDs..Time.since.first.diagnosis, -dawnosc_inicjacji)
    
    srednie <- tmp_numeryczne %>% sapply(mean)
    wariancje <- tmp_numeryczne %>% sapply(var)
    
    for(col in 1:length(colnames(tmp_numeryczne))){
      tmp_numeryczne[,col] <- wycentruj(tmp_numeryczne[,col], srednie[col], sqrt(wariancje[col]))
    }
    
    tmp <- cbind(tmp_numeryczne, tmp_faktory)
    
    attr(tmp, "srednie") <- srednie
    attr(tmp, "wariancje") <- wariancje
    
    tmp <- powiel_rekordy_z_rakiem(tmp, input$multiply_times, label()[ktore()] %>% as.numeric() - 1)
    
    tmp
  })
  data_test_i_val <- reactive({
    tmp <- training_set()[-ktore(),]
    tmp <- tmp %>% cbind(label()[-ktore()] %>% as.numeric() - 1) %>% as.data.frame()
    tmp$label <- tmp[,length(colnames(tmp))] %>% as.factor()
    tmp[,length(colnames(tmp))-1] <- NULL
    
    tmp_numeryczne <- tmp %>% select(Age, Number.of.sexual.partners, Num.of.pregnancies, Smokes..packs.year.,
                                     Hormonal.Contraceptives..years.,IUD..years.,
                                     STDs..Time.since.first.diagnosis, dawnosc_inicjacji)
    tmp_faktory <- tmp %>% select(-Age, -Number.of.sexual.partners, -Num.of.pregnancies, -Smokes..packs.year.,
                                  -Hormonal.Contraceptives..years., -IUD..years.,
                                  -STDs..Time.since.first.diagnosis, -dawnosc_inicjacji)
    
    
    for(col in 1:length(colnames(tmp_numeryczne))){
      tmp_numeryczne[,col] <- wycentruj(tmp_numeryczne[,col], attr(train(), "srednie")[col], sqrt(attr(train(), "wariancje")[col]))
    }
    
    tmp <- cbind(tmp_numeryczne, tmp_faktory)
    
    tmp
  })
  ktore2 <- reactive({
    sample(length(1:m() %>% setdiff(ktore())), floor(length(1:m() %>% setdiff(ktore()))*0.5))
  })
  test <- reactive({
    data_test_i_val()[ktore2(),]
  })
  val <- reactive({
    data_test_i_val()[-ktore2(),]
  })
  
  output$podzial_info_train <- renderText({paste("Ilosc targetu w zbiorze treningowym:",
                                                 (train()$label %>% as.numeric() - 1) %>% mean() %>% round(3))})
  output$podzial_info_val <- renderText({
    paste("\nIlosc targetu w zbiorze walidacyjnym:",
                                                (val()$label %>% as.numeric() - 1) %>% mean() %>% round(3))})
  output$podzial_info_test <- renderText({paste("\nIlosc targetu w zbiorze testowym:",
                                                (test()$label %>% as.numeric() - 1) %>% mean() %>% round(3))})
  
  
  
  
  
  
  
  # define task
  label_task <- reactive({ makeClassifTask(data = train(), target = "label") })
  # define CrossValidation
  cv <- reactive({ makeResampleDesc("CV", iters = input$M) })
  
  
  
  # knn
  pred_knn <- reactive({
    knn <- makeLearner("classif.kknn", par.vals = list("k" = input$k), predict.type = "prob")
    
    best_W_R <- 0
    best_wynik <- NULL
    for(times_i in 1:input$times){
      print(times_i)
      
      quiet( r <- resample(knn, label_task(), cv(), models = TRUE) )
      
      AUC_max <- 0
      model_best <- NULL
      for(i in 1:input$M){
        model <- r$models[[i]]
        
        pred_train <- predict(model, newdata = train())
        
        quiet( ROC <- roc(predictor = pred_train$data$prob.1, response = pred_train$data$truth) )
        auc(ROC) %>% as.numeric()
        if(auc(ROC) %>% as.numeric() > AUC_max){
          AUC_max <- auc(ROC) %>% as.numeric()
          model_best <- model
        }
      }
      pred_test <- predict(model_best, newdata = val())
      
      # prob_graniczny
      m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_test$data$prob.1)) # tyle bedzie przewidzianych zer
      prob_graniczny <- pred_test$data$prob.1[order(pred_test$data$prob.1)[m]] # m-ta statystyka
      
      # W_R
      tmp <- cbind(as.numeric(pred_test$data$prob.1 >= prob_graniczny), as.numeric(pred_test$data$truth)-1)
      TP <- 0
      FP <- 0
      TN <- 0
      FN <- 0
      for(i in 1:dim(tmp)[1]){
        if(tmp[i,1] == 1 & tmp[i,2] == 1){ TP <- TP + 1 }
        if(tmp[i,1] == 1 & tmp[i,2] == 0){ FP <- FP + 1 }
        if(tmp[i,1] == 0 & tmp[i,2] == 1){ FN <- FN + 1 }
        if(tmp[i,1] == 0 & tmp[i,2] == 0){ TN <- TN + 1 }
      }
      N <- TN + FP
      P <- TP + FN
      TPR <- TP/P
      TNR <- TN/N
      
      W_R <- (TPR * N/(P+N)) + (TNR * P/(P+N))
      
      # wybor najleprzego
      if(W_R > best_W_R){
        best_W_R <- W_R
        best_wynik <- pred_test$data
      }
    }
    attr(best_wynik, "W_R") <- best_W_R
    
    best_wynik
  })
  
  output$ROC_plot_kknn <- renderPlot({ plot(roc(predictor = pred_knn()$prob.1, response = pred_knn()$truth)) })
  output$AUC_knn <- renderText({ paste("AUC:", auc(roc(predictor = pred_knn()$prob.1, response = pred_knn()$truth)) %>% round(3)) })
  
  prob_graniczny_knn <- reactive({
    m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_knn()$prob.1)) # tyle bedzie przewidzianych zer
    prob_graniczny <- pred_knn()$prob.1[order(pred_knn()$prob.1)[m]] # m-ta statystyka
    prob_graniczny
  })
  output$granica_knn <- renderText({ paste("prawdopodobienstwo graniczne: ", as.character(prob_graniczny_knn() %>% round(3))) })
  output$FBeta_knn <- renderText({ paste("FBeta score: ", as.character(FBeta_Score(pred_knn()$truth, as.numeric(pred_knn()$prob.1 >= prob_graniczny_knn()), positive = NULL, beta = input$beta) %>% round(3))) })
  output$Table_knn <- renderTable({
    table(predicted = as.numeric(pred_knn()$prob.1 >= prob_graniczny_knn()), truth = pred_knn()$truth)
  })
  output$W_R_knn <- renderText({ 
    # prob_graniczny
    m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_knn()$prob.1)) # tyle bedzie przewidzianych zer
    prob_graniczny <- pred_knn()$prob.1[order(pred_knn()$prob.1)[m]] # m-ta statystyka
    
    tmp <- cbind(as.numeric(pred_knn()$prob.1 >= prob_graniczny), as.numeric(pred_knn()$truth)-1)
    TP <- 0
    FP <- 0
    TN <- 0
    FN <- 0
    for(i in 1:dim(tmp)[1]){
      if(tmp[i,1] == 1 & tmp[i,2] == 1){ TP <- TP + 1 }
      if(tmp[i,1] == 1 & tmp[i,2] == 0){ FP <- FP + 1 }
      if(tmp[i,1] == 0 & tmp[i,2] == 1){ FN <- FN + 1 }
      if(tmp[i,1] == 0 & tmp[i,2] == 0){ TN <- TN + 1 }
    }
    N <- TN + FP
    P <- TP + FN
    TPR <- TP/P
    TNR <- TN/N
    
    W_R <- (TPR * N/(P+N)) + (TNR * P/(P+N))
    
    paste("Weighted TPR-TNR:", as.character( W_R %>% round(3) ) )
  })
  output$AUPRC_knn <- renderText({
    paste("AUPRC:", as.character( auprc(prob = pred_knn()$prob.1, y_truth = pred_knn()$truth, positive_value = 1) %>% round(3) ) )
  })
  
  
  
  # las losowy
  pred_las <- reactive({
    classif_lrn <- makeLearner("classif.randomForest", par.vals = list(ntree = input$ntree, maxnodes = input$maxnodes), predict.type = "prob") 
    
    best_W_R <- 0
    best_wynik <- NULL
    for(times_i in 1:input$times){
      print(times_i)
      
      quiet( r <- resample(classif_lrn, label_task(), cv(), models = TRUE) )
      
      AUC_max <- 0
      model_best <- NULL
      for(i in 1:input$M){
        model <- r$models[[i]]
        
        quiet( pred_train <- predict(model, newdata = train()) )
        
        quiet( ROC <- roc(predictor = pred_train$data$prob.1, response = pred_train$data$truth) )
        auc(ROC) %>% as.numeric()
        if(auc(ROC) %>% as.numeric() > AUC_max){
          AUC_max <- auc(ROC) %>% as.numeric()
          model_best <- model
        }
      }
      pred_test <- predict(model_best, newdata = val())
      
      # prob_graniczny
      m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_test$data$prob.1)) # tyle bedzie przewidzianych zer
      prob_graniczny <- pred_test$data$prob.1[order(pred_test$data$prob.1)[m]] # m-ta statystyka
      
      # W_R
      tmp <- cbind(as.numeric(pred_test$data$prob.1 >= prob_graniczny), as.numeric(pred_test$data$truth)-1)
      TP <- 0
      FP <- 0
      TN <- 0
      FN <- 0
      for(i in 1:dim(tmp)[1]){
        if(tmp[i,1] == 1 & tmp[i,2] == 1){ TP <- TP + 1 }
        if(tmp[i,1] == 1 & tmp[i,2] == 0){ FP <- FP + 1 }
        if(tmp[i,1] == 0 & tmp[i,2] == 1){ FN <- FN + 1 }
        if(tmp[i,1] == 0 & tmp[i,2] == 0){ TN <- TN + 1 }
      }
      N <- TN + FP
      P <- TP + FN
      TPR <- TP/P
      TNR <- TN/N
      
      W_R <- (TPR * N/(P+N)) + (TNR * P/(P+N))
      
      # wybor najleprzego
      if(W_R > best_W_R){
        best_W_R <- W_R
        best_wynik <- pred_test$data
      }
    }
    attr(best_wynik, "W_R") <- best_W_R
    
    best_wynik
  })
  
  output$ROC_plot_las <- renderPlot({ plot(roc(predictor = pred_las()$prob.1, response = pred_las()$truth)) })
  output$AUC_las <- renderText({ paste("AUC:", auc(roc(predictor = pred_las()$prob.1, response = pred_las()$truth)) %>% round(3)) })
  
  prob_graniczny_las <- reactive({
    m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_las()$prob.1)) # tyle bedzie przewidzianych zer
    prob_graniczny <- pred_las()$prob.1[order(pred_las()$prob.1)[m]] # m-ta statystyka
    prob_graniczny
  })
  output$granica_las <- renderText({ paste("prawdopodobienstwo graniczne: ", as.character(prob_graniczny_las() %>% round(3))) })
  output$FBeta_las <- renderText({ paste("FBeta score: ", as.character(FBeta_Score(pred_las()$truth, as.numeric(pred_las()$prob.1 >= prob_graniczny_las()), positive = NULL, beta = input$beta) %>% round(3))) })
  output$Table_las <- renderTable({
    table(predicted = as.numeric(pred_las()$prob.1 >= prob_graniczny_las()), truth = pred_las()$truth)
  })
  output$W_R_las <- renderText({ 
    # prob_graniczny
    m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_las()$prob.1)) # tyle bedzie przewidzianych zer
    prob_graniczny <- pred_las()$prob.1[order(pred_las()$prob.1)[m]] # m-ta statystyka
    
    tmp <- cbind(as.numeric(pred_las()$prob.1 >= prob_graniczny), as.numeric(pred_las()$truth)-1)
    TP <- 0
    FP <- 0
    TN <- 0
    FN <- 0
    for(i in 1:dim(tmp)[1]){
      if(tmp[i,1] == 1 & tmp[i,2] == 1){ TP <- TP + 1 }
      if(tmp[i,1] == 1 & tmp[i,2] == 0){ FP <- FP + 1 }
      if(tmp[i,1] == 0 & tmp[i,2] == 1){ FN <- FN + 1 }
      if(tmp[i,1] == 0 & tmp[i,2] == 0){ TN <- TN + 1 }
    }
    N <- TN + FP
    P <- TP + FN
    TPR <- TP/P
    TNR <- TN/N
    
    W_R <- (TPR * N/(P+N)) + (TNR * P/(P+N))
    
    paste("Weighted TPR-TNR:", as.character( W_R %>% round(3) ) )
  })
  output$AUPRC_las <- renderText({
    paste("AUPRC:", as.character( auprc(prob = pred_las()$prob.1, y_truth = pred_las()$truth, positive_value = 1) %>% round(3) ) )
  })
  
  
  
  # logistic regresion
  pred_log <- reactive({
    logistic_learner <- makeLearner("classif.logreg", predict.type = "prob")
    
    best_W_R <- 0
    best_wynik <- NULL
    for(times_i in 1:input$times){
      print(times_i)
      
      quiet( r <- resample(logistic_learner, label_task(), cv(), models = TRUE) )
      
      AUC_max <- 0
      model_best <- NULL
      for(i in 1:input$M){
        model <- r$models[[i]]
        
        pred_train <- predict(model, newdata = train())
        
        quiet( ROC <- roc(predictor = pred_train$data$prob.1, response = pred_train$data$truth) )
        auc(ROC) %>% as.numeric()
        if(auc(ROC) %>% as.numeric() > AUC_max){
          AUC_max <- auc(ROC) %>% as.numeric()
          model_best <- model
        }
      }
      pred_test <- predict(model_best, newdata = val())
      
      # prob_graniczny
      m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_test$data$prob.1)) # tyle bedzie przewidzianych zer
      prob_graniczny <- pred_test$data$prob.1[order(pred_test$data$prob.1)[m]] # m-ta statystyka
      
      # W_R
      tmp <- cbind(as.numeric(pred_test$data$prob.1 >= prob_graniczny), as.numeric(pred_test$data$truth)-1)
      TP <- 0
      FP <- 0
      TN <- 0
      FN <- 0
      for(i in 1:dim(tmp)[1]){
        if(tmp[i,1] == 1 & tmp[i,2] == 1){ TP <- TP + 1 }
        if(tmp[i,1] == 1 & tmp[i,2] == 0){ FP <- FP + 1 }
        if(tmp[i,1] == 0 & tmp[i,2] == 1){ FN <- FN + 1 }
        if(tmp[i,1] == 0 & tmp[i,2] == 0){ TN <- TN + 1 }
      }
      N <- TN + FP
      P <- TP + FN
      TPR <- TP/P
      TNR <- TN/N
      
      W_R <- (TPR * N/(P+N)) + (TNR * P/(P+N))
      
      # wybor najleprzego
      if(W_R > best_W_R){
        best_W_R <- W_R
        best_wynik <- pred_test$data
      }
    }
    attr(best_wynik, "W_R") <- best_W_R
    
    best_wynik
  })
  
  output$ROC_plot_log <- renderPlot({ plot(roc(predictor = pred_log()$prob.1, response = pred_log()$truth)) })
  output$AUC_log <- renderText({ paste("AUC:", auc(roc(predictor = pred_log()$prob.1, response = pred_log()$truth)) %>% round(3)) })
  
  prob_graniczny_log <- reactive({
    m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_log()$prob.1)) # tyle bedzie przewidzianych zer
    prob_graniczny <- pred_log()$prob.1[order(pred_log()$prob.1)[m]] # m-ta statystyka
    prob_graniczny
  })
  output$granica_log <- renderText({ paste("prawdopodobienstwo graniczne: ", as.character(prob_graniczny_log() %>% round(3))) })
  output$FBeta_log <- renderText({ paste("FBeta score: ", as.character(FBeta_Score(pred_log()$truth, as.numeric(pred_log()$prob.1 >= prob_graniczny_log()), positive = NULL, beta = input$beta) %>% round(3))) })
  output$Table_log <- renderTable({
    table(predicted = as.numeric(pred_log()$prob.1 >= prob_graniczny_log()), truth = pred_log()$truth)
  })
  output$W_R_log <- renderText({ 
    # prob_graniczny
    m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_log()$prob.1)) # tyle bedzie przewidzianych zer
    prob_graniczny <- pred_log()$prob.1[order(pred_log()$prob.1)[m]] # m-ta statystyka
    
    tmp <- cbind(as.numeric(pred_log()$prob.1 >= prob_graniczny), as.numeric(pred_log()$truth)-1)
    TP <- 0
    FP <- 0
    TN <- 0
    FN <- 0
    for(i in 1:dim(tmp)[1]){
      if(tmp[i,1] == 1 & tmp[i,2] == 1){ TP <- TP + 1 }
      if(tmp[i,1] == 1 & tmp[i,2] == 0){ FP <- FP + 1 }
      if(tmp[i,1] == 0 & tmp[i,2] == 1){ FN <- FN + 1 }
      if(tmp[i,1] == 0 & tmp[i,2] == 0){ TN <- TN + 1 }
    }
    N <- TN + FP
    P <- TP + FN
    TPR <- TP/P
    TNR <- TN/N
    
    W_R <- (TPR * N/(P+N)) + (TNR * P/(P+N))
    
    paste("Weighted TPR-TNR:", as.character( W_R %>% round(3) ) )
  })
  
  output$AUPRC_log <- renderText({
    paste("AUPRC:", as.character( auprc(prob = pred_log()$prob.1, y_truth = pred_log()$truth, positive_value = 1) %>% round(3) ) )
  })
  
  
  
  # decision tree
  pred_tree <- reactive({
    dt_learner <- makeLearner("classif.rpart", predict.type = "prob")
    
    best_W_R <- 0
    best_wynik <- NULL
    for(times_i in 1:input$times){
      print(times_i)
      
      quiet( r <- resample(dt_learner, label_task(), cv(), models = TRUE) )
      
      AUC_max <- 0
      model_best <- NULL
      for(i in 1:input$M){
        model <- r$models[[i]]
        
        quiet( pred_train <- predict(model, newdata = train()) )
        
        quiet( ROC <- roc(predictor = pred_train$data$prob.1, response = pred_train$data$truth) )
        auc(ROC) %>% as.numeric()
        if(auc(ROC) %>% as.numeric() > AUC_max){
          AUC_max <- auc(ROC) %>% as.numeric()
          model_best <- model
        }
      }
      pred_test <- predict(model_best, newdata = val())
      
      # prob_graniczny
      m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_test$data$prob.1)) # tyle bedzie przewidzianych zer
      prob_graniczny <- pred_test$data$prob.1[order(pred_test$data$prob.1)[m]] # m-ta statystyka
      
      # W_R
      tmp <- cbind(as.numeric(pred_test$data$prob.1 >= prob_graniczny), as.numeric(pred_test$data$truth)-1)
      TP <- 0
      FP <- 0
      TN <- 0
      FN <- 0
      for(i in 1:dim(tmp)[1]){
        if(tmp[i,1] == 1 & tmp[i,2] == 1){ TP <- TP + 1 }
        if(tmp[i,1] == 1 & tmp[i,2] == 0){ FP <- FP + 1 }
        if(tmp[i,1] == 0 & tmp[i,2] == 1){ FN <- FN + 1 }
        if(tmp[i,1] == 0 & tmp[i,2] == 0){ TN <- TN + 1 }
      }
      N <- TN + FP
      P <- TP + FN
      TPR <- TP/P
      TNR <- TN/N
      
      W_R <- (TPR * N/(P+N)) + (TNR * P/(P+N))
      
      # wybor najleprzego
      if(W_R > best_W_R){
        best_W_R <- W_R
        best_wynik <- pred_test$data
      }
    }
    attr(best_wynik, "W_R") <- best_W_R
    
    best_wynik
  })
  
  output$ROC_plot_tree <- renderPlot({ plot(roc(predictor = pred_tree()$prob.1, response = pred_tree()$truth)) })
  output$AUC_tree <- renderText({ paste("AUC:", auc(roc(predictor = pred_tree()$prob.1, response = pred_tree()$truth)) %>% round(3)) })
  
  prob_graniczny_tree <- reactive({
    m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_tree()$prob.1)) # tyle bedzie przewidzianych zer
    prob_graniczny <- pred_tree()$prob.1[order(pred_tree()$prob.1)[m]] # m-ta statystyka
    prob_graniczny
  })
  output$granica_tree <- renderText({ paste("prawdopodobienstwo graniczne: ", as.character(prob_graniczny_tree() %>% round(3))) })
  output$FBeta_tree <- renderText({ paste("FBeta score: ", as.character(FBeta_Score(pred_tree()$truth, as.numeric(pred_tree()$prob.1 >= prob_graniczny_tree()), positive = NULL, beta = input$beta) %>% round(3) )) })
  output$Table_tree <- renderTable({
    table(predicted = as.numeric(pred_tree()$prob.1 >= prob_graniczny_tree()), truth = pred_tree()$truth)
  })
  output$W_R_tree <- renderText({ 
    # prob_graniczny
    m <- floor((1-(train()$label %>% as.numeric() - 1) %>% mean()) * length(pred_tree()$prob.1)) # tyle bedzie przewidzianych zer
    prob_graniczny <- pred_tree()$prob.1[order(pred_tree()$prob.1)[m]] # m-ta statystyka
    
    tmp <- cbind(as.numeric(pred_tree()$prob.1 >= prob_graniczny), as.numeric(pred_tree()$truth)-1)
    TP <- 0
    FP <- 0
    TN <- 0
    FN <- 0
    for(i in 1:dim(tmp)[1]){
      if(tmp[i,1] == 1 & tmp[i,2] == 1){ TP <- TP + 1 }
      if(tmp[i,1] == 1 & tmp[i,2] == 0){ FP <- FP + 1 }
      if(tmp[i,1] == 0 & tmp[i,2] == 1){ FN <- FN + 1 }
      if(tmp[i,1] == 0 & tmp[i,2] == 0){ TN <- TN + 1 }
    }
    N <- TN + FP
    P <- TP + FN
    TPR <- TP/P
    TNR <- TN/N
    
    W_R <- (TPR * N/(P+N)) + (TNR * P/(P+N))
    
    paste("Weighted TPR-TNR:", as.character( W_R %>% round(3) ) )
  })
  output$AUPRC_tree <- renderText({
    paste("AUPRC:", as.character( auprc(prob = pred_tree()$prob.1, y_truth = pred_tree()$truth, positive_value = 1) %>% round(3) ) )
  })
}








shinyApp(ui = ui, server = server)