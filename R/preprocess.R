{
  library(tidymodels)
  library(corrplot)
  library(flextable)
  library(tidyverse)
  select <- dplyr::select
}

social_mobility <- read_csv("data/Social_mobility-2018-Data.csv")  

# 888888 - Unknown
# 999999 - NA
sc <- social_mobility %>% 
  mutate_all(~na_if(., 999999)) %>%
  # Filtering all ppl in age 45-54
  filter(Gil %in% c(6,7)) 

# preprocessing the outcome ----
y <- sc %>% 
  select(SerialNumber,
         # Details about apartment, car and Housekeeping:
         DiraBaalut_C, # בעלות על דירה
         DiraNosefetBaalut, # דירה נוספת בבעלות  
         TzfifutDiyurM, # מספר אנשים לחדר בדירה
         Mechonitbaalut_C, # מספר מכוניות בבעלות
         # Positions on residence
         MerutzeEzor, # מרוצה מהאיזור בו אתה גר
         # Health and lifestyle
         MatzavBriut, # מצב בריאות
         # Skills: Studies,languages,courses,military service and driver’s license
         TeudaGvoha, # מהי התעודה או התואר הגבוה ביותר שקיבלת?
         # Computer and Internet use and access to technology
         PehamimShimushInt, # בדרך כלל, כמה פעמים בשבוע אתה משתמש באינטרנט?
         # Employment
         MatzavTaasukaM_C, # מצב תעסוקה
         # contact with family and friends
         MerutzeKesherMishp, # האם את/ה מרוצה מהקשר שלך עם בני משפחתך?
         YeshChaverim,
         TadirutChaverim,
         # Economic status and income and welfare
         MerutzeChaim, # באופן כללי, האם את/ה מרוצה מחייך?
         MerutzeKalkali, # האם את/ה מרוצה ממצבך הכלכלי?
         HachnasaAvodaNeto # בחודש שעבר, מה הייתה הכנסה ברוטו
  ) %>% 
  mutate(TadirutChaverim = ifelse(YeshChaverim %in% c(2, 888888), yes = 5,
                                  no = TadirutChaverim),
         HachnasaAvodaNeto = case_when(MatzavTaasukaM_C %in% c(2,3) &
                                         is.na(HachnasaAvodaNeto) ~ 0,
                                       T ~ HachnasaAvodaNeto),
         HachnasaAvodaNeto = ifelse(HachnasaAvodaNeto == 11, yes = 0,
                                    no = HachnasaAvodaNeto),
         Mispar_dirot = case_when(DiraBaalut_C == 2 ~ 0,
                                  DiraNosefetBaalut == 2  | 
                                    DiraNosefetBaalut == 888888 |
                                    is.na(DiraNosefetBaalut) ~ 1,
                                  DiraNosefetBaalut == 1 ~ 2
         ),
         # הקטגוריה 0 עכשיו תהיה לא למד כלל/לא קיבל אף אחת מהתעודות הרשומות
         TeudaGvoha = ifelse(TeudaGvoha == 7, yes = 0,
                             no = TeudaGvoha)
  ) %>% 
  filter(!(is.na(HachnasaAvodaNeto) | HachnasaAvodaNeto == 888888)) %>%
  select(-c(YeshChaverim,
            DiraNosefetBaalut, DiraBaalut_C,
            MatzavTaasukaM_C)) %>% 
  mutate_all(~na_if(., 888888)) %>% 
  na.omit() %>% 
  # I want all variables to signify that "more" means "better", 
  # therefore i multiplied the relevant variables by -1
  # (the values will be scaled afterwards)
  mutate_at(.vars = vars(c(TzfifutDiyurM, MatzavBriut,
                           MerutzeKesherMishp,
                           MerutzeEzor, PehamimShimushInt,
                           MerutzeChaim, 
                           MerutzeKalkali, 
                           TadirutChaverim)),
            ~. * (-1)
            
  ) %>%  
  rename_at(.vars = vars(-SerialNumber), ~paste0(.x, "_y"))


# Preprocessing predictors --
x <- sc %>%
  select(SerialNumber,
         YelidBrham,
         SemelEretz,
         SemelEretzAv_C,
         SemelEretzEm_C,
         Minn,
         GilNisuinRishon,
         GilMegurimAtzmaim,
         GilYeledRishon, 
         DatiutYehudiBen15,
         TeudaGvoha,
         Dat,
         TeudaGvohaAv_C, # תעודה גבוהה - אב
         TeudaGvohaEm_C, # תעודה גבוהה - אם
         SherutTzahal,
         SherutLeumi,
         MaamadAvodaAv_C, # כשהיית בן/בת 15 , האם אביך היה:
         MaamadAvodaEm_C  # כשהיית בן/בת 15 , האם אמך היתה:
  ) %>% 
  mutate(birth_plcae = case_when(
    YelidBrham == 1 ~ "USSR",
    SemelEretz == 1 & SemelEretzEm_C != SemelEretzAv_C ~
      "Israel (parents: Mixed)",
    SemelEretz == 1 & SemelEretzEm_C == 1 & SemelEretzAv_C == 1 ~ 
      "Israel (parents: Israel)",
    SemelEretz == 1 & SemelEretzEm_C == 2 & SemelEretzAv_C == 2 ~
      "Israel (parents: Europe / America)",
    SemelEretz == 1 & SemelEretzEm_C == 3 & SemelEretzAv_C == 3 ~
      "Israel (parents: Asia)",
    SemelEretz == 1 & SemelEretzEm_C == 4 & SemelEretzAv_C == 4 ~
      "Israel (parents: Africa)",
    SemelEretz == 2 & YelidBrham == 2 ~ "Europe / America",
    SemelEretz == 3 ~ "Asia", 
    SemelEretz == 4 ~ "Africa"),
    Minn = case_when(Minn == 1 ~ "Male",
                     Minn == 2 ~ "Female"),
    GilNisuinRishon = case_when(GilNisuinRishon %in% c(7:10, 888888) ~ 7,
                                is.na(GilNisuinRishon) ~ 7,
                                T ~ GilNisuinRishon), # לא התחתנו עד גיל 30
    GilMegurimAtzmaim = ifelse(GilMegurimAtzmaim == 0 |
                                 GilMegurimAtzmaim == 888888,
                               yes = 8, no = GilMegurimAtzmaim),
    # לא הביאו ילדים עד גיל 30
    GilYeledRishon = ifelse(GilYeledRishon %in% c(8:10) |
                              is.na(GilYeledRishon) |
                              GilYeledRishon == 888888, yes = 7,
                            no = GilYeledRishon
    ),
    Ethnicity = case_when(
      Dat == 1 & DatiutYehudiBen15 == 1 ~ "Jew, Haredi",
      Dat == 1 & DatiutYehudiBen15 == 2 ~ "Jew, Religious",
      Dat == 1 & DatiutYehudiBen15 == 3 ~ "Jew, Masorti",
      Dat == 1 & DatiutYehudiBen15 == 4 ~ "Jew, Masorti not very Religious",
      Dat == 1 & DatiutYehudiBen15 == 5 ~ "Jew, Secular",
      Dat == 2 ~ "Musilm",
      !(Dat %in% c(1,2)) ~ "Else"
    ),
    Ethnicity = relevel(factor(Ethnicity), "Jew, Secular"),
    TeudaGvoha = case_when(TeudaGvoha == 6 ~ 5,
                           TeudaGvoha == 7 ~ 0,
                           T ~ TeudaGvoha),
    TeudaGvohaAv_C = ifelse(TeudaGvohaAv_C %in% c(7, 888888), yes = 0, 
                            no = TeudaGvohaAv_C),
    TeudaGvohaEm_C = ifelse(TeudaGvohaEm_C %in% c(7, 888888), yes = 0,
                            no = TeudaGvohaEm_C),
    service = case_when((SherutTzahal == 1) | 
                          (SherutTzahal == 2 & SherutLeumi == 1) ~ TRUE,
                        T ~ FALSE),
    father_work_at_age_15 = case_when(is.na(MaamadAvodaAv_C) | 
                                        MaamadAvodaAv_C == 3 ~ "Else",
                                      MaamadAvodaAv_C == 0 ~ "Didn't work",
                                      MaamadAvodaAv_C == 1 ~ "Employee",
                                      MaamadAvodaAv_C == 2 ~ "Business owner",
                                      MaamadAvodaAv_C == 4 ~ "Passed away"
    ),
    father_work_at_age_15 = relevel(factor(father_work_at_age_15), "Employee"),
    mother_work_at_age_15 = case_when(is.na(MaamadAvodaEm_C) |
                                        MaamadAvodaEm_C == 3 ~ "Else",
                                      MaamadAvodaEm_C == 0 ~ "Didn't work",
                                      MaamadAvodaEm_C == 1 ~ "Employee",
                                      MaamadAvodaEm_C == 2 ~ "Business owner",
                                      MaamadAvodaEm_C == 4 ~ "Passed away"
    ),
    mother_work_at_age_15 = relevel(factor(mother_work_at_age_15), "Employee")
  ) %>% 
  select(-c(YelidBrham, SemelEretz,
            SemelEretzAv_C, SemelEretzEm_C,
            Dat, DatiutYehudiBen15,
            SherutTzahal, SherutLeumi,
            MaamadAvodaEm_C, MaamadAvodaAv_C,
            TeudaGvoha))




## ==Splitting the data==
set.seed(305777468)
split_data <- initial_split(x, prop = 3/4)
# Test
test_dat <- testing(split_data) %>% 
  left_join(y, by = "SerialNumber") %>% 
  mutate_at(.vars = vars(ends_with("_y")),
            ~scale(., center = TRUE, scale = TRUE)) %>% 
  mutate(
    ses_outcome = rowMeans(
      select(., ends_with("_y")) #%>% 
    )) %>% 
  mutate(ses_outcome = ses_outcome > 0) %>% 
  select(-ends_with("_y"), -SerialNumber) %>% 
  na.omit() 



# Train
train_dat <- training(split_data) %>% 
  left_join(y, by = "SerialNumber") %>% 
  mutate_at(.vars = vars(ends_with("_y")),
            ~scale(., center = TRUE, scale = TRUE)) %>% 
  mutate(
    ses_outcome = rowMeans(
      select(., ends_with("_y")) #%>% 
    )) %>% 
  mutate(ses_outcome = ses_outcome > 0) %>% 
  select(-ends_with("_y"), -SerialNumber) %>% 
  na.omit()


# PCA ---------------------------------------------------------------------
library(cowplot)
pca_fit <- train_dat %>% 
  select_if(is.numeric) %>% 
  prcomp(scale = TRUE, center = TRUE) 

arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)


pca_fit %>%
  augment(train_dat %>% 
            select_if(is.numeric)) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2)) + 
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(malignant = "#D55E00", benign = "#0072B2")
  ) +
  theme_half_open(12) + background_grid()


pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal_grid(12)




# corr on train -----------------------------------------------------------------
cor_dat <- train_dat %>% 
  select_if(is.numeric) %>% 
  cor()
corrplot(cor_dat,
         method = 'number',
         type = "upper", diag = FALSE)

# glm ---------------------------------------------------------------------

gtsummary::tbl_summary(
  data = train_dat,
  by = ses_outcome) %>% 
  gtsummary::add_p() %>% 
  gtsummary::bold_p()

glm_fit <- glm(ses_outcome ~ .,
               data = train_dat,
               family = binomial(logit))

y_pred_train <- predict(glm_fit, type = "response") 
glm_train_accuracy <- mean((y_pred_train > 0.5) == train_dat$ses_outcome)
glm_train_auc <- round(pROC::auc(train_dat$ses_outcome,
                                 y_pred_train), 3)


y_pred_test <- predict(glm_fit, newdata = test_dat, type = "response")
glm_test_accuracy <- mean((y_pred_test > 0.5) == test_dat$ses_outcome)

glm_test_auc <- round(pROC::auc(test_dat$ses_outcome,
                                y_pred_test), 3)


gtsummary::tbl_regression(glm_fit) %>% 
  gtsummary::bold_p()


# Simple Tree model -------------------------------------------------------

library(rpart)
library(rpart.plot)
library(rattle)


tree <- rpart(ses_outcome ~ .,
              data = train_dat,
              method = "class")
prp(tree)

train_probs <- predict(tree, newdata = train_dat, type = "prob")[,2]
stree_accuracy_train <- mean((train_probs > 0.5) == train_dat$ses_outcome)
stree_train_auc <- round(pROC::auc(train_dat$ses_outcome,
                                   train_probs), 3)


test_probs <- predict(tree, newdata = test_dat, type = "prob")[,2]
stree_accuracy_test <- mean((test_probs > 0.5) == test_dat$ses_outcome)

stree_test_auc <- round(pROC::auc(test_dat$ses_outcome,
                                  test_probs), 3)

# importance plot
tree$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Classication")

# Random Forest -----------------------------------------------------------

library(randomForest)
library(mlbench)
library(caret)
library(e1071)

### Cross validation for random forest parameters
customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "nodesize"),
                                  class = rep("numeric", 3),
                                  label = c("mtry", "ntree", "nodesize"))


customRF$grid <- function(x, y, len = NULL, search = "grid") {}


customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree=param$ntree,
               nodesize = param$nodesize)
}

#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, 
                             submodels = NULL)
  predict(modelFit, newdata)

#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3,
                        allowParallel = TRUE)

tunegrid <- expand.grid(.mtry=c(1:5),
                        .ntree=c(5, 10, 15, 20), 
                        .nodesize = c(20, 40, 60, 80))

### Random forest model
set.seed(305777468)
rf_model <- train(factor(ses_outcome) ~ ., 
                  data = train_dat,
                  method = customRF, 
                  metric = 'Accuracy', 
                  tuneGrid = tunegrid, 
                  trControl = control)


# Choosing the best parameters
rf_model$results %>%  
  arrange(desc(Accuracy))

rf_train <- predict(rf_model, train_dat, type = "prob")[,2]
rf_accuracy_train <- mean((rf_train > 0.5) == train_dat$ses_outcome)
RF_train_auc <- round(pROC::auc(train_dat$ses_outcome,
                                rf_train), 3)


rf_test <- predict(rf_model, test_dat, type = "prob")[,2]
rf_accuracy_test <- mean((rf_test > 0.5) == test_dat$ses_outcome)
RF_test_auc <- round(pROC::auc(test_dat$ses_outcome,
                               rf_test), 3)

# importance plot
rf_model$finalModel$importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  ggplot(aes(x = fct_reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_pointrange(aes(ymin = 0, ymax = MeanDecreaseGini), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Random Forest")


# SVM -------------------------------------------------------------------------
# preprocess 
recp_dat <- recipe(ses_outcome ~ ., data = train_dat) %>% 
  step_dummy(c("birth_plcae",
               "Ethnicity",
               "father_work_at_age_15",
               "mother_work_at_age_15"
  ))

numeric_train_dat <- recipes::prep(recp_dat) %>% 
  recipes::bake(new_data = train_dat) %>% 
  mutate(service = as.numeric(service),
         Minn = ifelse(Minn == "Male", 1, 0)) %>%
  mutate_at(vars(-ses_outcome), ~scale(., center = TRUE, scale = TRUE)) 


numeric_test_dat <- recipes::prep(recp_dat) %>% 
  recipes::bake(new_data = test_dat) %>% 
  mutate(service = as.numeric(service),
         Minn = ifelse(Minn == "Male", 1, 0)) %>%
  mutate_at(vars(-ses_outcome), ~scale(., center = TRUE, scale = TRUE)) 


# SVM Linear
library(caret)
set.seed(305777468)
control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3,
                        allowParallel = TRUE)

svm_cv_linear <- train(ses_outcome ~., 
                       data = numeric_train_dat %>% 
                         mutate(ses_outcome = as.factor(ses_outcome)), 
                       method = "svmLinear", 
                       trControl = train_control, 
                       tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
                       preProcess = c("center","scale"), 
                       prob.model = TRUE)

svm_linear_pred_train <- predict(svm_cv_linear, newdata = numeric_train_dat,
                                 type = 'prob')[,2]

svm_linear_train_pred <- mean((svm_linear_pred_train > 0.5) == 
                                numeric_train_dat$ses_outcome)

svm_linear_train_auc <- round(pROC::auc(numeric_train_dat$ses_outcome,
                                        svm_linear_pred_train), 3)


svm_linear_pred_test <- predict(svm_cv_linear, newdata = numeric_test_dat,
                                type = 'prob')[,2]
svm_linear_test_pred <- mean((svm_linear_pred_test > 0.5) == 
                               numeric_test_dat$ses_outcome)

svm_linear_test_auc <- round(pROC::auc(numeric_test_dat$ses_outcome,
                                       svm_linear_pred_test), 3)


# SVM Radial
set.seed(305777468)
svm_cv_radial <- train(ses_outcome ~., 
                       data = numeric_train_dat %>% 
                         mutate(ses_outcome = as.factor(ses_outcome)), 
                       method = "svmRadial", 
                       trControl = train_control, 
                       tuneLength = 10,
                       preProcess = c("center","scale"), 
                       prob.model = TRUE)

svm_radial_pred_train <- predict(svm_cv_radial, newdata = numeric_train_dat,
                                 type = 'prob')[,2]
svm_radial_train_pred <- mean((svm_radial_pred_train > 0.5) == 
                                numeric_train_dat$ses_outcome)


svm_radial_pred_test <- predict(svm_cv_radial, newdata = numeric_test_dat,
                                type = 'prob')[,2]
svm_radial_test_pred <- mean((svm_radial_pred_test > 0.5) ==
                               numeric_test_dat$ses_outcome)



auc_radial_train <- round(as.numeric(pROC::auc(numeric_train_dat$ses_outcome,
                                               svm_radial_pred_train)), 3)

auc_radial_test <- round(pROC::auc(numeric_test_dat$ses_outcome,
                                   svm_radial_pred_test), 3)



# Neural Network -----------------------------------------------------------
set.seed(305777468)
train_neuralnetwork <- caret::train(as.factor(ses_outcome) ~., 
                                    data = numeric_train_dat, 
                                    method = "nnet",
                                    tuneGrid = expand.grid(size = c(10, 15, 20,
                                                                    25),
                                                           decay = c(0.1, 0.6, 0.7,
                                                                     0.8)),
                                    trControl = trainControl(method = "LGOCV",
                                                             number = 3, p = 0.8),
                                    trace = TRUE
)

nn_pred_train <- predict(train_neuralnetwork, newdata = numeric_train_dat,
                         type = 'prob')[,2]
nn_train_pred <- mean((nn_pred_train > 0.5) ==
                        numeric_train_dat$ses_outcome)
auc_nn_train <- round(pROC::auc(numeric_train_dat$ses_outcome,
                                nn_pred_train), 3)



nn_pred_test <- predict(train_neuralnetwork,
                        newdata = numeric_test_dat, type = 'prob')[,2]
nn_test_pred <- mean((nn_pred_test > 0.5) ==
                       numeric_test_dat$ses_outcome)
auc_nn_test <- round(pROC::auc(numeric_test_dat$ses_outcome,
                               nn_pred_test), 3)


# By Service

mean(train_dat$service == train_dat$ses_outcome)
mean(test_dat$service == test_dat$ses_outcome)


# Summary models performance ----------------------------------------------
library(glue)
tibble("Model" = c("Logistic Regression",
                   "Decision Tree",
                   "Random Forest",
                   "Radial SVM",
                   "Linear SVM",
                   "Neural Network"),
       "Train Accuracy (AUC)" = c(
         glue("{round(glm_train_accuracy*100, 3)}% ({glm_train_auc})"),
         glue("{round(stree_accuracy_train*100, 3)}% ({stree_train_auc})"),
         glue("{round(rf_accuracy_train*100, 3)}% ({RF_train_auc})"),
         glue("{round(svm_radial_train_pred*100, 3)}% ({auc_radial_train})"),
         glue("{round(svm_linear_train_pred*100, 3)}% ({svm_linear_train_auc})"),
         glue("{round(nn_train_pred*100, 3)}% ({auc_nn_train})")
       ),
       "Test Accuracy (AUC)" = c(
         glue("{round(glm_test_accuracy*100, 3)}% ({glm_test_auc})"),
         glue("{round(stree_accuracy_test*100, 3)}% ({stree_test_auc})"),
         glue("{round(rf_accuracy_test*100, 3)}% ({RF_test_auc})"),
         glue("{round(svm_radial_test_pred*100, 3)}% ({auc_radial_test})"),
         glue("{round(svm_linear_test_pred*100, 3)}% ({svm_linear_test_auc})"),
         glue("{round(nn_test_pred*100, 3)}% ({auc_nn_test})")
       )
) %>% 
  flextable()


# PCA on the outcome variable ---------------------------------------------

y_scaled <- y %>% 
  mutate_at(.vars = vars(-SerialNumber),
            ~scale(., center = TRUE, scale = TRUE))


pr <- y_scaled %>% 
  select(-SerialNumber) %>%   
  prcomp()
# prin <- y_scaled %>% 
#   select(-SerialNumber) %>% 
#   princomp()


library(ggfortify)

plotly::ggplotly(
  autoplot(pr,
           data = y  %>% 
             left_join(x,
                       by = "SerialNumber") %>% 
             select(-SerialNumber),
           colour = "service",
           frame = TRUE, 
           frame.type = 'norm',
           loadings.label = TRUE,
           loadings = TRUE
  ) +
    theme_classic()
)

#

# Understanding the 'Service' relationship with social mobility -----------

# This time we will try to understand how Military \ National Service is 
# related to people from a difficult background

names(x)

# Self Reported

x %>% 
  left_join(
    sc %>% select(SerialNumber , ShinuyRamatHaim)
  ) %>% 
  select(-SerialNumber) %>% 
  mutate(
      ShinuyRamatHaim = 
        case_when(
          ShinuyRamatHaim == 1 ~ "Better",
          ShinuyRamatHaim == 2 ~ "Worse",
          ShinuyRamatHaim == 3 ~ "Unchanged"
        )
    ) %>% 
  select(service, ShinuyRamatHaim, Ethnicity) %>% 
  filter(Ethnicity %in%
           c("Jew, Religious",
             "Jew, Haredi",
             "Jew, Masorti",
             "Jew, Masorti not very Religious"
             )) %>%
  select(-Ethnicity) %>% 
  gtsummary::tbl_summary(
    by = service
  ) 


# Self Reported






unique(x$mother_work_at_age_15)

# Exploring the data ------------------------------------------------------
## Big and meaningful conclusion!
service_by_ethnicity <- y %>%
  mutate(
  ses_outcome = rowMeans(
    select(., ends_with("_y")) #%>% 
  )) %>% 
  left_join(x,
            by = "SerialNumber") %>% 
  select(ses_outcome, service, Ethnicity) %>% 
  ggplot(
    aes(x = ses_outcome, fill = service)
  ) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Ethnicity) +
  xlab("Socioeconomic Score (centerd and scaled)") +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  labs(caption = "Else:Christians, Druze, other religions, atheists") +
  theme(plot.caption = element_text(hjust = 0, size = 11))





