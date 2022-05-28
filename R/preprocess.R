{
  library(tidyverse)
  library(tidymodels)
  library(corrplot)
  library(flextable)
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
  mutate_at(.vars = vars(-SerialNumber),
            ~scale(., center = TRUE, scale = TRUE))


y_final <- y %>% 
  select(-SerialNumber) %>% 
  rowMeans() %>% 
  tibble(SerialNumber = y$SerialNumber, y = .) %>% 
  mutate(y = y > 0)


# Preprocessing predictors ----
x <- sc %>%
  inner_join(y_final , by = "SerialNumber") %>% 
  # TODO: Go BACK TO THE OTHER QUESTIONS BEFORE
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


# Final Data
final_data <- x %>% 
  left_join(y_final) %>% 
  select(-SerialNumber) %>% 
  rename(scs_outcome = y)


## ==Splitting the data==
set.seed(305777468)
split_data <- initial_split(final_data, prop = 3/4)
# Test
test <- testing(split_data)
# Train
train <- training(split_data) 



# PCA ---------------------------------------------------------------------
library(cowplot)
pca_fit <- train %>% 
  select_if(is.numeric) %>% 
  prcomp(scale = TRUE, center = TRUE) 

arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)


pca_fit %>%
  augment(train %>% 
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

cor_dat <- train %>% 
  select_if(is.numeric) %>% 
  cor()
corrplot(cor_dat,
         method = 'number',
         type = "upper", diag = FALSE)



# glm ---------------------------------------------------------------------

gtsummary::tbl_summary(
  data = train,
  by = scs_outcome
) %>% 
  gtsummary::add_p()

glm_fit <- glm(scs_outcome ~ .,
               data = train,
               family = binomial(logit))

y_pred_train <- predict(glm_fit, type = "response")
y_pred_test <- predict(glm_fit, newdata = test, type = "response")

bind_cols(train, pred = y_pred_train) %>% View()
bind_cols(test, pred = y_pred_test) %>% View()
