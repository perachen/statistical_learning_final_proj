{
  library(tidyverse)
  library(broom) 
}

social_mobility <- read_csv("data/Social_mobility-2018-Data.csv")  
  # filter(!(HachnasaKolelet %in% c(999999, 888888))) %>% 
  # select(SerialNumber,
  #        # Details about apartment, car and Housekeeping:
  #        DiraBaalut_C, # בעלות על דירה
  #        DiraNosefetBaalut, # דירה נוספת בבעלות  
  #        TzfifutDiyurM, # מספר אנשים לחדר בדירה
  #        Mechonitbaalut_C, # מספר מכוניות בבעלות
  #        # Positions on residence
  #        MerutzeEzor, # מרוצה מהאיזור בו אתה גר
  #        # Health and lifestyle
  #        MatzavBriut, # מצב בריאות
  #        # Skills: Studies, languages, courses, military service and driver’s license
  #        TeudaGvoha, # מהי התעודה או התואר הגבוה ביותר שקיבלת?
  #        # Computer and Internet use and access to technology
  #        PehamimShimushInt, # בדרך כלל, כמה פעמים בשבוע אתה משתמש באינטרנט? כולל שימוש באינטרנט לצורך עבודה
  #        # Employment
  #        MatzavTaasukaM_C, # מצב תעסוקה
  #        # contact with family and friends
  #        MerutzeKesherMishp, # האם את/ה מרוצה מהקשר שלך עם בני משפחתך?
  #        YeshChaverim,
  #        TadirutChaverim,
  #        # Economic status and income and welfare
  #        MerutzeChaim, # באופן כללי, האם את/ה מרוצה מחייך?
  #        MerutzeKalkali, # האם את/ה מרוצה ממצבך הכלכלי?
  #        HachnasaKolelet, # בחודש שעבר, מה הייתה הכנסתך ברוטו
  # ) 
  # 
  

# preprocess
# 888888 - Unknown
sc <- social_mobility %>% 
  mutate_all(~na_if(., 999999)) %>%
  select(-c(shana,
            contains("Kodem"),
            SibaMaavarMegurim,
            contains("Zara"),
            )) %>%
  # Filtering all ppl in age 45-54
  filter(Gil %in% c(6,7)) 
  
  
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
   HachnasaKolelet # בחודש שעבר, מה הייתה הכנסת משק הבית ברוטו
    ) %>% 
  # TODO: create new variable from DiraBaalut_C
  # TODO: להורי את מי שמקבל עוזר מביטוח לאומי
  filter((HachnasaKolelet != 888888 ) & !is.na(HachnasaKolelet)) %>%
  mutate(TadirutChaverim = ifelse(YeshChaverim == 2, yes = 0, no = TadirutChaverim),
         HachnasaKolelet = ifelse(HachnasaKolelet == 11, yes = 0, no = HachnasaKolelet),
         Mispar_dirot = case_when(DiraBaalut_C == 2 ~ 0,
                                         DiraNosefetBaalut == 2  | 
                                           DiraNosefetBaalut == 888888 |
                                           is.na(DiraNosefetBaalut) ~ 1,
                                         DiraNosefetBaalut == 1 ~ 2
         ),
         TadirutChaverim = ifelse(is.na(TadirutChaverim), yes = 0, no = TadirutChaverim)) %>% 
  select(-c(YeshChaverim,
            DiraNosefetBaalut, DiraBaalut_C)) %>% 
  mutate_all(~na_if(., 888888)) %>% 
  na.omit()
  
  

library(cowplot)

pca_fit <- y %>% 
  select(-SerialNumber) %>% 
  prcomp(scale = TRUE, center = TRUE) 

arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

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

  
  
  x <- social_mobility %>% 
    # TODO: Go BACK TO THE OTHER QUESTIONS BEFORE
    select(GilMegurimAtzmaim,
           GilYeledRishon, # TODO: NEED TO MAKE MANIPULATION WITH THE AGE)
           Pop_Group,
           Dat,
           DatiutLoYehudi,
           DatiutLoYehudiBen15,
           DatiutYehudi,
           ShinuyDatiut,
           HagdaratMotza_C,
           TeudaGvohaAv_C, # תעודה גבוהה - אב
           TeudaGvohaEm_C, # תעודה גבוהה - אם
           MaamadAvodaAv_C, # כשהיית בן/בת 15 , האם אביך היה:
           MaamadAvodaEm_C,  # כשהיית בן/בת 15 , האם אמך היתה:
           ,
           ,
           ,
           ,
           )