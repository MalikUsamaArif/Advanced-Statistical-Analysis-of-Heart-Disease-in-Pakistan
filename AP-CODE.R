# Install required packages if not already installed
#install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "plotly", "reshape2", "caret", "nnet", "randomForest"))#
#install.packages("car")

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(caret)
library(nnet)
library(cluster)
library(car)
library(MASS)
library(randomForest)

# Load data
data <- read.csv("D://osama//Documents//4th Sem//Applied Statistics//heart-patients-in-pakistan.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Heart Disease Patients Analysis of Pakistan Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("EDA", tabName = "eda", icon = icon("search")),
      menuItem("Hypothesis Testing", tabName = "tests", icon = icon("chart-bar")),
      menuItem("Models", tabName = "models", icon = icon("robot")),
      menuItem("Suggestions for Policymakers", tabName = "suggestions", icon = icon("lightbulb"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "eda",
        fluidRow(
          box(title = "Gap Analysis: Symptoms vs Severity Level", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("gapSymptomsSeverityPlot")),
          box(title = "Multi-Disease Risk Detection", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("multiDiseasePlot")),
          box(title = "High Attention to Middle Aged Female Patients", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("highattentionfemalesPlot")),
          box(title = "Comorbidity Clusters Need Special Attention", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("comorbidityClustersPlot")),
          box(title = "Risk Stratification at Triage", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("risktriagePlot")),
          box(title = "Blood Markers Vs Heart Disease Severity", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("bloodmakersPlot")),
          box(title = "Follow Up Analysis By Severity Level and Mortality", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("followupPlot"))
        )
      ),
       # 2. Statistical Tests Tab ----
      tabItem(tabName = "tests",
              fluidRow(
                box(title = "Normality Test (Shapiro-Wilk)", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("normalityPlot")
                ),
                box(title = "Heart Disease Severity by Gender", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("severityGenderPlot")
                ),
                box(title = "ECG & Stress Test Indicators", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("ecgPlot")
                ),
                box(title = "Age vs Heart Disease Severity Level", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("ageSeverityPlot")
                ),
                box(title = "Enable Early Detection through Blood Tests", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("bloodPlot")
                ),
                box(title = "Lifestyle and Other Factors Association", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("lifestylePlot")
                ),
                box(title = "Biochemical Markers vs Heart Disease Severity Level", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("biochemPlot")
                )
              )
      ),

      # 3. Models Tab ----
      tabItem(tabName = "models",
              fluidRow(
                box(title = "Severity Prediction Model", width = 12, status = "success", solidHeader = TRUE,
                    plotlyOutput("severityModelPlot")
                ),
                box(title = "Predicting Death Using Logistic Regression", width = 12, status = "success", solidHeader = TRUE,
                    plotlyOutput("deathModelPlot")
                ),
                box(title = "Clustering Patients into Risk Groups", width = 12, status = "success", solidHeader = TRUE,
                    plotlyOutput("clusteringPlot")
                )
              )
      ),
      
      # 4. Suggestions Tab ----
      tabItem(tabName = "suggestions",
              fluidRow(
                box(title = "Policy Suggestions", width = 12, status = "warning", solidHeader = TRUE,
                   p(strong("1. Symptom-Based Risk Underestimation:"),
"Train healthcare workers and launch awareness campaigns so chest pain alone is never ignored, as it often signals severe heart disease."),
p(strong("2. Multi-Disease Risk Detection:"),
"Integrate routine diabetes and kidney screenings into cardiac checkups to catch high-risk patients early."),
p(strong("3. Middle-Aged Female Risk:"),
"Prioritize preventive screenings and lifestyle counseling for middle-aged women, especially those with diabetes and high cholesterol."),
p(strong("4. Comorbidity Clusters:"),
"Design healthcare programs that manage diabetes, hypertension, and cardiac conditions together, not separately."),
p(strong("5. Risk Stratification at Triage:"),
"Use triage-based risk tools in hospitals to prioritize treatment and resource allocation for moderate to high-risk patients."),
p(strong("6. Blood Markers vs Severity:"),
"Do not rely solely on basic blood counts; include specific cardiac biomarkers in heart disease diagnosis protocols."),
p(strong("7. Follow-up and Mortality:"),
"Encourage more frequent follow-up visits for cardiac patients to reduce mortality risk."),
p(strong("8. Gender and Severity:"),
"Despite similar severity distributions, ensure gender-sensitive care practices as males dominate every severity category."),
p(strong("9. ECG and Stress Test Indicators:"),
"Mandate interpretation training for ECG and stress test abnormalities to detect hidden heart risks."),
p(strong("10. Early Detection Through Blood Tests:"),
"Use linked blood parameters like WBC and hemoglobin as early warning signs for cardiac health issues."),
p(strong("11. Age vs Severity:"),
"Start regular cardiac checkups earlier for aging populations as severity rises significantly with age."),
p(strong("12. Lifestyle and Other Factor Associations:"),
"Raise awareness about thalassemia’s influence on heart disease and screen at-risk groups accordingly."),
p(strong("13. Biochemical Markers and Severity:"),
"Monitor kidney function and cardiac enzymes closely, as they are strong indicators of worsening heart disease."),
p(strong("14. Severity Prediction Model:"),
"Promote adoption of predictive tools using age, heart rate, and blood markers to flag high-risk heart patients early."),
p(strong("15. Mortality Prediction Model:"),
"Start heart health programs focused on seniors, men, and patients with high cholesterol, ST depression, and exercise-induced angina."),
p(strong("16. Clustering Risk Groups:"),
"Develop tailored care pathways for different patient risk groups identified through clustering (high, moderate, low risk).")
                )
              )
      )
    )
  )
)


# Server
server <- function(input, output) {
  
  output$gapSymptomsSeverityPlot <- renderPlotly({
    data$num <- as.factor(data$num)
    data$Symptom_Type <- case_when(
      grepl("chest pain", data$Clinical_Observation, ignore.case = TRUE) &
        grepl("sob|shortness of breath", data$Clinical_Observation, ignore.case = TRUE) ~ "Chest Pain + SOB",
      grepl("chest pain", data$Clinical_Observation, ignore.case = TRUE) &
        grepl("vomiting|nausea|sweating|cold|vertigo|dizziness|headache|palpitation", 
              data$Clinical_Observation, ignore.case = TRUE) ~ "Chest Pain + Other Symptoms",
      grepl("chest pain", data$Clinical_Observation, ignore.case = TRUE) ~ "Chest Pain Only",
      grepl("sob|shortness of breath", data$Clinical_Observation, ignore.case = TRUE) ~ "SOB Only",
      grepl("vomiting|nausea|sweating|cold|vertigo|dizziness|headache|palpitation", 
            data$Clinical_Observation, ignore.case = TRUE) ~ "Other Symptoms Only",
      TRUE ~ "Unspecified/Unknown"
    )
    
    symptom_table <- table(data$Symptom_Type, data$num)
    print(chisq.test(symptom_table))
    
    summary_df <- data %>%
      group_by(Symptom_Type, num) %>%
      summarise(Count = n(), .groups = 'drop')
    
    p <- ggplot(summary_df, aes(x = Symptom_Type, y = Count, fill = num,
                                text = paste("Symptom:", Symptom_Type,
                                             "<br>Severity Level:", num,
                                             "<br>Count:", Count))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Gap Analysis between Symptoms and Severity",
           x = "Reported Symptom Type",
           y = "Patient Count",
           fill = "Severity Level") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
output$multiDiseasePlot <- renderPlotly({
  data <- data %>%
    mutate(
      Kidney_Risk = ifelse(S.Cr > 1.2 | B.Urea > 40, TRUE, FALSE),
      High_Severity = ifelse(as.numeric(num) >= 3, TRUE, FALSE),
      Diabetes_Risk = ifelse(Diabetes == 1, TRUE, FALSE)
    )

  total_patients <- nrow(data)
  kidney_and_severity <- sum(data$Kidney_Risk & data$High_Severity, na.rm = TRUE)
  diabetes_and_severity <- sum(data$Diabetes_Risk & data$High_Severity, na.rm = TRUE)

  plot_df <- data.frame(
    Condition = c("Kidney Risk + High Severity", "Diabetes + High Severity"),
    Count = c(kidney_and_severity, diabetes_and_severity)
  ) %>%
    mutate(Percentage = round(100 * Count / total_patients, 1))

  p <- ggplot(plot_df, aes(x = Condition, y = Percentage, fill = Condition,
                           text = paste("Condition:", Condition,
                                        "<br>Percentage:", Percentage, "%"))) +
    geom_bar(stat = "identity", width = 0.6) +
    labs(title = "Percentage of Patients with Multi-Disease & High Heart Severity",
         y = "Percentage (%)", x = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 20, hjust = 1))

  ggplotly(p, tooltip = "text")
})

output$highattentionfemalesPlot <- renderPlotly({
  target_group <- data %>%
    filter(Gender == "Female", Age >= 45, Age <= 59)

  summary_stats <- target_group %>%
    summarise(
      Diabetes_Percent = mean(Diabetes == 1, na.rm = TRUE) * 100,
      HTN_Percent = mean(HTN == 1, na.rm = TRUE) * 100,
      Elevated_SCr = mean(S.Cr > 1.2, na.rm = TRUE) * 100,
      High_Chol = mean(chol > 200, na.rm = TRUE) * 100
    )

  comorbidity_data <- data.frame(
    Comorbidity = c("Diabetes", "HTN", "Elevated S.Cr", "High Cholesterol"),
    Percent = c(
      summary_stats$Diabetes_Percent,
      summary_stats$HTN_Percent,
      summary_stats$Elevated_SCr,
      summary_stats$High_Chol
    )
  )

  p <- ggplot(comorbidity_data, aes(x = Comorbidity, y = Percent, fill = Comorbidity,
                                    text = paste("Condition:", Comorbidity,
                                                 "<br>Percent:", round(Percent, 1), "%"))) +
    geom_bar(stat = "identity", width = 0.5) +
    labs(title = "Comorbidity Profile of Middle-Aged Female Patients (45–59)",
         y = "Percentage (%)", x = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 15, hjust = 1))

  ggplotly(p, tooltip = "text")
})

output$comorbidityClustersPlot <- renderPlotly({
  data <- data %>%
    mutate(
      Diabetes_Flag = Diabetes == 1,
      HTN_Flag = HTN == 1,
      High_BP = trestbps >= 140,
      Abnormal_CK = CK.MB > 25 | C.P.K > 300,
      Comorbidity_Cluster = ifelse(Diabetes_Flag & HTN_Flag & (High_BP | Abnormal_CK),
                                   "High Risk Cluster", "Others")
    )

  cluster_df <- data %>%
    group_by(Comorbidity_Cluster) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(Percentage = round(100 * Count / sum(Count), 1))

  p <- ggplot(cluster_df, aes(x = Comorbidity_Cluster, y = Percentage, fill = Comorbidity_Cluster,
                              text = paste("Group:", Comorbidity_Cluster,
                                           "<br>Percent:", Percentage, "%"))) +
    geom_bar(stat = "identity", width = 0.6) +
    labs(title = "Comorbidity Cluster: Diabetes + HTN + BP or CK Abnormalities",
         y = "Percentage (%)", x = NULL) +
    theme_minimal() +
    theme(legend.position = "none")

  ggplotly(p, tooltip = "text")
})

output$risktriagePlot <- renderPlotly({
  data <- data %>%
    mutate(
      Chest_Pain = as.integer(grepl("chest pain", Clinical_Observation, ignore.case = TRUE)),
      High_SCr = as.integer(S.Cr > 1.2),
      High_CKMB = as.integer(CK.MB > 25),
      Low_Thalach = as.integer(thalach < 100),
      High_Oldpeak = as.integer(oldpeak > 2),
      Abnormal_ECG = as.integer(restecg != 0),
      Abnormal_Slope = as.integer(slope != 1),
      Comorbidity_Count = as.integer(Diabetes == 1) + as.integer(HTN == 1) + High_SCr
    ) %>%
    mutate(
      risk_score = Chest_Pain + Comorbidity_Count + Low_Thalach + High_Oldpeak + High_CKMB +
                   Abnormal_ECG + Abnormal_Slope,
      Risk_Level = case_when(
        risk_score >= 5 ~ "High",
        risk_score >= 3 ~ "Moderate",
        TRUE ~ "Low"
      )
    )

  risk_summary <- data %>%
    group_by(Risk_Level) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(Percentage = round(100 * Count / sum(Count), 1))

  p <- ggplot(risk_summary, aes(x = Risk_Level, y = Percentage, fill = Risk_Level,
                                text = paste("Risk:", Risk_Level, "<br>Percent:", Percentage, "%"))) +
    geom_bar(stat = "identity") +
    labs(title = "Triage Risk Stratification of Patients",
         x = "Risk Level", y = "Percentage of Patients") +
    theme_minimal() +
    theme(legend.position = "none")

  ggplotly(p, tooltip = "text")
})

output$bloodmakersPlot <- renderPlotly({
  blood_vars <- c("WBC", "NEUTROPHIL", "Hemoglobin", "PLATELET_COUNT", "ESR")
  data[blood_vars] <- lapply(data[blood_vars], as.numeric)
  data$num <- as.numeric(as.character(data$num))

  cor_df <- data.frame(
    Biomarker = blood_vars,
    Spearman_Correlation = sapply(blood_vars, function(var) {
      cor(data[[var]], data$num, method = "spearman", use = "complete.obs")
    })
  )

  cor_df$Strength <- ifelse(abs(cor_df$Spearman_Correlation) > 0.5, "Strong",
                            ifelse(abs(cor_df$Spearman_Correlation) > 0.3, "Moderate", "Weak"))

  p <- ggplot(cor_df, aes(x = reorder(Biomarker, Spearman_Correlation), 
                          y = Spearman_Correlation, fill = Strength,
                          text = paste("Biomarker:", Biomarker,
                                       "<br>Spearman Correlation:", round(Spearman_Correlation, 2),
                                       "<br>Strength:", Strength))) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("Strong" = "darkred", "Moderate" = "orange", "Weak" = "gray")) +
    labs(title = "Blood Markers vs Heart Disease Severity",
         x = "Blood Marker", y = "Spearman Correlation") +
    theme_minimal()

  ggplotly(p, tooltip = "text")
})

output$followupPlot <- renderPlotly({
  data$num <- as.factor(data$num)
  data$Mortality <- as.factor(data$Mortality)
  data$Follow.Up <- as.numeric(data$Follow.Up)

  p <- ggplot(data, aes(x = Mortality, y = Follow.Up, fill = Mortality,
                        text = paste("Mortality:", Mortality,
                                     "<br>Follow-Up Days:", Follow.Up))) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.4) +
    labs(title = "Follow-Up Visits by Mortality Status",
         x = "Mortality (0 = Survived, 1 = Died)", y = "Follow-Up (Visits)") +
    theme_minimal()

  ggplotly(p, tooltip = "text")
})

  ## --- Hypothesis Testing Section ---
  ### Normality Plot
 output$normalityPlot <- renderPlotly({
  # Ensure required library
  library(ggplot2)
  library(plotly)
  library(scales)
  
  # Select numeric columns manually (avoids using where() which caused error)
  numeric_cols <- data[, sapply(data, is.numeric)]
  
  # Keep only continuous variables (more than 2 unique values)
  continuous_cols <- numeric_cols[, sapply(numeric_cols, function(x) length(unique(x)) > 2)]
  
  # Initialize empty data frame
  normality_scores <- data.frame(Column = character(), P_Value = numeric(), Non_Normality_Percent = numeric())
  
  # Shapiro-Wilk Test for each column
  for (col in colnames(continuous_cols)) {
    result <- shapiro.test(continuous_cols[[col]])
    non_normal_percent <- (1 - result$p.value) * 100
    normality_scores <- rbind(normality_scores, data.frame(
      Column = col,
      P_Value = result$p.value,
      Non_Normality_Percent = non_normal_percent
    ))
  }
  
  # Sort by non-normality percentage
  normality_scores <- normality_scores[order(-normality_scores$Non_Normality_Percent), ]
  
  # Plot
  gg <- ggplot(normality_scores, aes(x = reorder(Column, P_Value), y = P_Value)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
    geom_hline(yintercept = 0.05, color = "red", linetype = "dashed", size = 1) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", scientific_format())
    ) +
    labs(
      title = "Shapiro-Wilk Test: P-Values on Log Scale",
      x = "Columns",
      y = "P-Value (log scale)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(gg)
})
  
  ### Severity by Gender Plot
  output$severityGenderPlot <- renderPlotly({
    data$num <- as.numeric(data$num)
    data$Gender <- as.factor(data$Gender)
    summary_df <- data %>% group_by(Gender, num) %>% summarise(Count = n()) %>% ungroup()
    p <- ggplot(summary_df, aes(x = factor(num), y = Count, fill = Gender,
                                text = paste("Gender:", Gender,
                                             "<br>Severity Level:", num,
                                             "<br>Count:", Count))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Heart Disease Severity by Gender",
           x = "Severity Level", y = "Count") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  ### ECG & Stress Test
output$ecgPlot <- renderPlotly({
  req(data)

  # Clean and handle unexpected or missing values safely
  df_clean <- data %>%
    mutate(
      restecg = as.numeric(as.character(restecg)),
      slope = as.numeric(as.character(slope)),
      RestECG_Label = dplyr::case_when(
        restecg == 0 ~ "Normal",
        restecg == 1 ~ "ST-T Abnormality",
        restecg == 2 ~ "LV Hypertrophy",
        TRUE ~ "Other/Unknown"
      ),
      Slope_Label = dplyr::case_when(
        slope == 1 ~ "Upsloping",
        slope == 2 ~ "Flat",
        slope == 3 ~ "Downsloping",
        TRUE ~ "Other/Unknown"
      )
    )

  # Summarize counts
  df_plot <- df_clean %>%
    count(RestECG_Label, Slope_Label, name = "Count") %>%
    filter(!is.na(RestECG_Label), !is.na(Slope_Label))

  # Plot
  p <- ggplot(df_plot, aes(x = RestECG_Label, y = Count, fill = Slope_Label,
                           text = paste("ECG:", RestECG_Label,
                                        "<br>Slope:", Slope_Label,
                                        "<br>Count:", Count))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "ECG vs ST Segment Slope",
         x = "ECG Category",
         y = "Count",
         fill = "ST Slope") +
    theme_minimal()

  ggplotly(p, tooltip = "text")
})
  output$kruskalResult <- renderPrint({
  req(data)
  data$num <- as.factor(data$num)
  data$Age <- as.numeric(data$Age)

  kruskal_result <- kruskal.test(Age ~ num, data = data)
  kruskal_result
})

# Age vs Severity Level plot
output$ageSeverityPlot <- renderPlotly({
  req(data)

  data$num <- as.factor(data$num)
  data$Age <- as.numeric(data$Age)

  p <- ggplot(data, aes(x = num, y = Age, fill = num,
                        text = paste("Severity Level:", num,
                                     "<br>Age:", Age))) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "Age Distribution by Heart Disease Severity Level",
         x = "Severity Level (num)",
         y = "Age") +
    theme_minimal()

  ggplotly(p, tooltip = "text")
})
  ### Blood Heatmap
  output$bloodPlot <- renderPlotly({
    blood_vars <- c("WBC", "RBC", "Hemoglobin", "P.C.V", "M.C.V", "M.C.H", "M.C.H.C",
                    "PLATELET_COUNT", "NEUTROPHIL", "LYMPHO", "MO0CYTE", "EOSI0")
    blood_vars <- blood_vars[blood_vars %in% colnames(data)]
    data[blood_vars] <- lapply(data[blood_vars], as.numeric)
    cor_matrix <- cor(data[blood_vars], method = "spearman", use = "complete.obs")
    cor_df <- melt(cor_matrix)
    gg <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value,
                             text = paste("Correlation:", round(value, 2)))) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      labs(title = "Spearman Correlation Between Blood Parameters") +
      theme_minimal()
    ggplotly(gg, tooltip = "text")
  })
  
  ### Lifestyle Plot
  output$lifestylePlot <- renderPlotly({
    data$num <- as.factor(data$num)
    cat_factors <- c("Smoking", "Diabetes", "HTN", "Life_style", "Marital_Status", "Depression",
                     "Family_History", "Allergies", "Sleep", "Hyperlipi", "Gender", "exang", "cp", "fbs", "thal")
    cat_factors <- cat_factors[cat_factors %in% colnames(data)]
    chi_sq_summary <- lapply(cat_factors, function(factor) {
      table_data <- table(data[[factor]], data$num)
      chisq_test <- chisq.test(table_data)
      data.frame(Factor = factor, p_value = chisq_test$p.value)
    }) %>% bind_rows()
    p <- ggplot(chi_sq_summary, aes(x = reorder(Factor, -p_value), y = -log10(p_value), fill = p_value < 0.05)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray"), name = "Significant") +
      labs(title = "Chi-square Test: Lifestyle Factors vs Severity", x = "Factor", y = "-log10(p-value)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })
  
  ### Biochemical Plot
  output$biochemPlot <- renderPlotly({
    bio_vars <- c("BGR", "B.Urea", "S.Cr", "CK.MB", "C.P.K", "chol", "S.Sodium", "S.Potassium", "S.Chloride", "ESR")
    bio_vars <- bio_vars[bio_vars %in% colnames(data)]
    data[bio_vars] <- lapply(data[bio_vars], as.numeric)
    data$num <- as.numeric(as.character(data$num))
    cor_matrix <- sapply(data[bio_vars], function(var) {
      cor(var, data$num, method = "spearman", use = "complete.obs")
    })
    cor_df <- data.frame(Biomarker = names(cor_matrix), Correlation = cor_matrix)
    cor_df <- cor_df[order(abs(cor_df$Correlation), decreasing = TRUE), ]
    p <- ggplot(cor_df, aes(x = reorder(Biomarker, Correlation), y = Correlation, fill = Correlation)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      labs(title = "Biochemical Markers vs Heart Disease Severity",
           x = "Marker", y = "Spearman Correlation") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
    ##model
output$severityModelPlot <- renderPlotly({
  req(data)
  
  predictors <- c("Age", "Gender", "restecg", "slope", "thalach", "oldpeak", "WBC", "RBC", "Hemoglobin", 
                  "P.C.V", "NEUTROPHIL", "PLATELET_COUNT", "LYMPHO", "Smoking", "Diabetes", "HTN", 
                  "Life_style", "Sleep", "Hyperlipi", "thal", "cp", "exang", "fbs", "S.Cr", "CK.MB")
  
  predictors <- predictors[predictors %in% colnames(data)]
  model_data <- data[, c("num", predictors)]
  model_data <- na.omit(model_data)
  model_data$num <- as.factor(model_data$num)

  factor_vars <- intersect(c("Gender", "restecg", "slope", "Smoking", "Diabetes", "HTN", 
                             "Life_style", "Sleep", "Hyperlipi", "thal", "cp", "exang", "fbs"), predictors)
  numeric_vars <- setdiff(predictors, factor_vars)
  model_data[factor_vars] <- lapply(model_data[factor_vars], as.factor)
  model_data[numeric_vars] <- lapply(model_data[numeric_vars], as.numeric)

  set.seed(123)
  train_index <- createDataPartition(model_data$num, p = 0.8, list = FALSE)
  train_data <- model_data[train_index, ]
  
  rf_model <- randomForest(num ~ ., data = train_data, importance = TRUE, ntree = 200)
  imp_df <- data.frame(Feature = rownames(rf_model$importance),
                       Importance = rf_model$importance[, "MeanDecreaseGini"])
  
  p <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance,
                          text = paste("Feature:", Feature, "<br>Importance:", round(Importance, 2)))) +
    geom_col(fill = "darkred") +
    coord_flip() +
    labs(title = "Random Forest Feature Importance", x = "Variable", y = "Importance") +
    theme_minimal()
  
  ggplotly(p, tooltip = "text")
})
# the mortality_analysis reactive function with these corrections:
mortality_analysis <- reactive({
  # STEP 1: Data Preparation
  clean_data <- data %>%
    filter(!is.na(Mortality)) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(thal = case_when(
      thal == 7 ~ "reversible_defect",
      thal == 6 ~ "fixed_defect",
      TRUE ~ "normal"
    ) %>% as.factor())
  
  # Ensure Mortality is numeric (0/1)
  clean_data$Mortality <- as.numeric(as.character(clean_data$Mortality))
  
  # STEP 2: Univariate Screening
  all_predictors <- c("Age", "Gender", "S.Cr", "CK.MB", "B.Urea", "thal", 
                     "exang", "oldpeak", "chol", "trestbps", "HTN", "Diabetes", 
                     "Hemoglobin", "P.C.V", "LYMPHO")
  
  # Filter to only include predictors that exist in the data
  all_predictors <- all_predictors[all_predictors %in% colnames(clean_data)]
  
  univariate_results <- lapply(all_predictors, function(var) {
    tryCatch({
      if(is.numeric(clean_data[[var]])) {
        model <- glm(Mortality ~ scale(get(var)), data = clean_data, family = binomial)
      } else {
        model <- glm(Mortality ~ get(var), data = clean_data, family = binomial)
      }
      p_value <- coef(summary(model))[2, 4]
      data.frame(Predictor = var, p_value = p_value)
    }, error = function(e) data.frame(Predictor = var, p_value = NA))
  })
  
  univariate_df <- bind_rows(univariate_results) %>%
    filter(p_value < 0.1 & !is.na(p_value))
  
  # STEP 3: Model Building
  if(nrow(univariate_df) > 0) {
    formula_str <- paste("Mortality ~", paste(univariate_df$Predictor, collapse = " + "))
    full_model <- try(glm(as.formula(formula_str), data = clean_data, family = binomial), silent = TRUE)
    
    if(!inherits(full_model, "try-error")) {
      # Simplified risk score calculation for visualization
      clean_data <- clean_data %>%
        mutate(
          risk_score = 0.3 * scale(Age) + 
            0.4 * scale(S.Cr) + 
            0.3 * (thal == "reversible_defect") + 
            0.2 * HTN
        )
      
      # Plot
      p <- ggplot(clean_data, aes(x = risk_score, fill = factor(Mortality))) +
        geom_density(alpha = 0.5) +
        labs(title = "Mortality Risk Score Distribution",
             x = "Risk Score", y = "Density", fill = "Mortality") +
        theme_minimal()
      
      interactive_plot <- ggplotly(p, tooltip = "all") %>%
        style(hoverlabel = list(bgcolor = "white", font = list(color = "black"))) %>%
        layout(xaxis = list(title = "Risk Score"),
               yaxis = list(title = "Density"))
      
      # Return the plot
      list(plot = interactive_plot)
    } else {
      # Fallback if model fails
      list(plot = ggplotly(ggplot() + 
                           annotate("text", x = 0.5, y = 0.5, 
                                   label = "Model could not be fit with selected predictors") +
                           theme_void()))
    }
  } else {
    # Fallback if no significant predictors
    list(plot = ggplotly(ggplot() + 
                         annotate("text", x = 0.5, y = 0.5, 
                                 label = "No significant predictors found") +
                         theme_void()))
  }
})
output$deathModelPlot <- renderPlotly({
  mortality_analysis()$plot
})
output$clusteringPlot <- renderPlotly({
  req(data)
  
  cluster_vars <- c("Age", "S.Cr", "CK.MB", "chol", "trestbps", "thalach")
 raw_cluster_data <- dplyr::select(data, all_of(cluster_vars)) %>% na.omit()

  
  cluster_data <- scale(raw_cluster_data)
  set.seed(123)
  clusters <- kmeans(cluster_data, centers = 3)
  pca <- prcomp(cluster_data)

  pca_data <- data.frame(
    Dim1 = pca$x[,1],
    Dim2 = pca$x[,2],
    Cluster = factor(clusters$cluster),
    PatientID = rownames(raw_cluster_data)
  )

  dim1_var <- names(which.max(abs(pca$rotation[,1])))
  dim2_var <- names(which.max(abs(pca$rotation[,2])))

  pca_data$Cluster <- factor(pca_data$Cluster, levels = 1:3,
                             labels = c("High Risk", "Moderate Risk", "Low Risk"))

  pca_data <- cbind(pca_data, raw_cluster_data)

  pca_data$hover <- paste(
    "PatientID:", pca_data$PatientID, "<br>",
    "Age:", round(pca_data$Age, 1), "<br>",
    "S.Cr:", round(pca_data$S.Cr, 2), "<br>",
    "CK.MB:", round(pca_data$CK.MB, 1), "<br>",
    "Cholesterol:", round(pca_data$chol, 1), "<br>",
    "BP (trestbps):", round(pca_data$trestbps, 1), "<br>",
    "Max HR (thalach):", round(pca_data$thalach, 1), "<br>",
    "Cluster:", pca_data$Cluster
  )

  plot_ly(
    data = pca_data,
    x = ~Dim1, y = ~Dim2,
    type = "scatter", mode = "markers",
    color = ~Cluster,
    text = ~hover, hoverinfo = "text",
    colors = c("red", "green", "blue"),
    marker = list(size = 8, opacity = 0.7)
  ) %>% layout(
    title = "Patient Subtypes by Key Health Markers",
    xaxis = list(title = paste("PC1:", dim1_var)),    yaxis = list(title = paste("PC2:", dim2_var)),
    legend = list(title = list(text = "Patient Subtype"))
  )
})
}

# Run the app
shinyApp(ui,server)
