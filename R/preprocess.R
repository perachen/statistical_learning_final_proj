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

# preprocessing the outcome
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
         # Skills: Studies, languages, courses, military service and driver’s license
         TeudaGvoha, # מהי התעודה או התואר הגבוה ביותר שקיבלת?
         # Computer and Internet use and access to technology
         PehamimShimushInt, # בדרך כלל, כמה פעמים בשבוע אתה משתמש באינטרנט? כולל שימוש באינטרנט לצורך עבודה
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
         HachnasaAvodaNeto = case_when(MatzavTaasukaM_C %in% c(2,3) & is.na(HachnasaAvodaNeto) ~ 0,
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
  # therefore i multiplied the relevant variables by -1 (the values will be scaled afterwards)
  mutate_at(.vars = vars(c(TzfifutDiyurM, MatzavBriut,
                           MerutzeKesherMishp,
                           MerutzeEzor, PehamimShimushInt,
                           MerutzeChaim, 
                           MerutzeKalkali, 
                           TadirutChaverim)),
            ~. * (-1)
            
  ) %>%  
  rename_at(.vars = vars(-SerialNumber), ~paste0(.x, "_y"))


# Preprocessing predictors ----
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
    SemelEretz == 1 & SemelEretzEm_C != SemelEretzAv_C ~ "Israel (parents: Mixed)",
    SemelEretz == 1 & SemelEretzEm_C == 1 & SemelEretzAv_C == 1 ~ "Israel (parents: Israel)",
    SemelEretz == 1 & SemelEretzEm_C == 2 & SemelEretzAv_C == 2 ~ "Israel (parents: Europe / America)",
    SemelEretz == 1 & SemelEretzEm_C == 3 & SemelEretzAv_C == 3 ~ "Israel (parents: Asia)",
    SemelEretz == 1 & SemelEretzEm_C == 4 & SemelEretzAv_C == 4 ~ "Israel (parents: Africa)",
    SemelEretz == 2 & YelidBrham == 2 ~ "Europe / America",
    SemelEretz == 3 ~ "Asia", 
    SemelEretz == 4 ~ "Africa"),
    Minn = case_when(Minn == 1 ~ "Male",
                     Minn == 2 ~ "Female"),
    GilNisuinRishon = case_when(GilNisuinRishon %in% c(7:10, 888888) ~ 7,
                                is.na(GilNisuinRishon) ~ 7,
                                T ~ GilNisuinRishon), # לא התחתנו עד גיל 30
    GilMegurimAtzmaim = ifelse(GilMegurimAtzmaim == 0 | GilMegurimAtzmaim == 888888,
                               yes = 8, no = GilMegurimAtzmaim),
    # לא הביאו ילדים עד גיל 30
    GilYeledRishon = ifelse(GilYeledRishon %in% c(8:10) |
                              is.na(GilYeledRishon) |
                              GilYeledRishon == 888888, yes = 7, no = GilYeledRishon
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
    TeudaGvohaAv_C = ifelse(TeudaGvohaAv_C %in% c(7, 888888), yes = 0, no = TeudaGvohaAv_C),
    TeudaGvohaEm_C = ifelse(TeudaGvohaEm_C %in% c(7, 888888), yes = 0, no = TeudaGvohaEm_C),
    service = case_when((SherutTzahal == 1) | (SherutTzahal == 2 & SherutLeumi == 1) ~ TRUE,
                        T ~ FALSE),
    father_work_at_age_15 = case_when(is.na(MaamadAvodaAv_C) | MaamadAvodaAv_C == 3 ~ "Else",
                                      MaamadAvodaAv_C == 0 ~ "Didn't work",
                                      MaamadAvodaAv_C == 1 ~ "Employee",
                                      MaamadAvodaAv_C == 2 ~ "Business owner",
                                      MaamadAvodaAv_C == 4 ~ "Passed away"
    ),
    father_work_at_age_15 = relevel(factor(father_work_at_age_15), "Employee"),
    mother_work_at_age_15 = case_when(is.na(MaamadAvodaEm_C) | MaamadAvodaEm_C == 3 ~ "Else",
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
  gtsummary::add_p()

glm_fit <- glm(ses_outcome ~ .,
               data = train_dat,
               family = binomial(logit))

y_pred_train <- predict(glm_fit, type = "response")
y_pred_test <- predict(glm_fit, newdata = test_dat, type = "response")

bind_cols(train_dat, pred = y_pred_train)
bind_cols(test_dat, pred = y_pred_test)

# Simple Tree model -------------------------------------------------------

library(rpart)
library(rpart.plot)
library(rattle)


tree <- rpart(ses_outcome ~ .,
              data = train_dat,
              method = "class")
prp(tree)

train_probs <- predict(tree, newdata = train_dat, type = "prob")[,2]
test_probs <- predict(tree, newdata = test_dat, type = "prob")[,2]

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

control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=3,
                        allowParallel = TRUE)

tunegrid <- expand.grid(.mtry=c(1:5),
                        .ntree=c(5, 10, 15), 
                        .nodesize = c(20, 40, 60))

set.seed(305777468)
custom <- train(factor(ses_outcome) ~ ., 
                data = train_dat,
                method=customRF, 
                metric='Accuracy', 
                tuneGrid=tunegrid, 
                trControl=control)

custom$results %>%  
  arrange(desc(Accuracy))

### Final random forest model
set.seed(305777468)
rf <- randomForest(factor(ses_outcome) ~ .,
                   data = train_dat,
                   ntree = 50,
                   mtry = 40, 
                   nodesize = 60)
prediction_func(rf, test_dat)$err
prediction_func(rf, train_dat)$err

importance(rf)
varImpPlot(rf)

rf_test <- predict(rf, test_dat, type = "prob")[,2]
rf_train <- predict(rf, train_dat, type = "prob")[,2]


# KNN ---------------------------------------------------------------------






