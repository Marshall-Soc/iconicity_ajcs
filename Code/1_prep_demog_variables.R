
# Header ------------------------------------------------------------------

# Purpose: Do the final data prep before analysis
# Author: Michael Lee Wood & Marshall A. Taylor


# Merge survey data with link cluster assignment --------------------------

icon.data.2 <- icon.data.sub


# Prep Demographic Variables ----------------------------------------------


#rename the individual-level variables
icon.data.2 <- icon.data.2 %>% 
  dplyr::rename(Gender = Q396,
                Race = Q397,
                Education = Q399,
                Income = Q401,
                Birth_Year = Q398,
                Where_Live = Q400,
                Vote_2016 = Q394,
                Vote_2012 = Q393,
                Pol_Party = Q390,
                Pol_Ideo = Q389_1,
                Pres_Words_1 = Q395_1,
                Pres_Words_2 = Q395_2,
                Pres_Words_3 = Q395_3,
                Pres_Words_4 = Q395_4,
                Pres_Words_5 = Q395_5,
                Role_Chief_State = Q402_1,
                Role_Chief_Exec = Q402_2,
                Role_Chief_Leg = Q402_3,
                Role_Command = Q402_4,
                Role_Chief_Diplo = Q402_5,
                Role_Chief_Party = Q402_6,
                Role_Chief_Citiz = Q402_7)

#fix formatting
icon.data.2$Pol_Ideo <- as.numeric(icon.data.2$Pol_Ideo)
icon.data.2$Birth_Year <- as.numeric(icon.data.2$Birth_Year)
icon.data.2$Age <- (2021 - icon.data.2$Birth_Year)
icon.data.2$Where_Live <- as.factor(icon.data.2$Where_Live)
icon.data.2$Income <- as.factor(icon.data.2$Income)
icon.data.2$Income <- factor(icon.data.2$Income,
                             ordered = TRUE,
                             levels = c("$0 to $ 25,000",
                                        "$25,001 to $48,000",
                                        "$48,001 to $79,000",
                                        "$79,001 to $127,000",
                                        "$127,001 or above"))
icon.data.2$Education <- as.factor(icon.data.2$Education)
icon.data.2$Education <- factor(icon.data.2$Education,
                                ordered = TRUE,
                                levels = c("8th grade or less",
                                           "Some high school",
                                           "High school graduate",
                                           "Some college",
                                           "2-year college graduate",
                                           "4-year college graduate",
                                           "Graduate/professional school"))
icon.data.2$Role_Chief_Citiz <- as.numeric(icon.data.2$Role_Chief_Citiz)
icon.data.2$Role_Chief_Exec <- as.numeric(icon.data.2$Role_Chief_Exec)
icon.data.2$Role_Chief_State <- as.numeric(icon.data.2$Role_Chief_State)
icon.data.2$Role_Chief_Leg <- as.numeric(icon.data.2$Role_Chief_Leg)
icon.data.2$Role_Command <- as.numeric(icon.data.2$Role_Command)
icon.data.2$Role_Chief_Diplo <- as.numeric(icon.data.2$Role_Chief_Diplo)
icon.data.2$Role_Chief_Party <- as.numeric(icon.data.2$Role_Chief_Party)

icon.data.2 <- icon.data.2 %>%
  mutate(gender2 = case_when(
    Gender == "Female" ~ "Female",
    Gender == "Male" ~ "Male",
    TRUE ~ NA_character_),
    race2 = case_when(
      Race == "White" ~ "White",
      Race == "Black or African American" ~ "Black",
      Race == "Hispanic or Latinx" ~ "Hispanic/Latinx",
      Race == "American Indian" ~ NA_character_,
      Race == "Asian" ~ "Asian",
      Race == "Other" ~ NA_character_,
      TRUE ~ "Multiracial"),
    white = case_when(
      Race == "White" ~ "White",
      TRUE ~ "Non-White"),
    educ2 = case_when(
      Education <= "2-year college graduate" ~ "Less than 4-year degree",
      TRUE ~ "At least a 4-year degree"),
    party2 = case_when(
      Pol_Party == "Independent" ~ "Independent",
      Pol_Party == "Independent near Democrat" ~ "Independent",
      Pol_Party == "Independent near Republican" ~ "Independent",
      Pol_Party == "Not strong Democrat" ~ "Democrat",
      Pol_Party == "Strong Democrat" ~ "Democrat",
      Pol_Party == "Not strong Republican" ~ "Republican",
      Pol_Party == "Strong Republican" ~ "Republican",
      TRUE ~ "Other"),
    living = case_when(
      Where_Live == "Big city" ~ "Big city",
      Where_Live == "Mid or Small-size city" ~ "Mid/Small city",
      Where_Live == "Smal town/rural" ~ "Small town/Rural",
      Where_Live == "Suburbs" ~ "Suburbs",
      TRUE ~ NA_character_),
    v2016.c = case_when(
      Vote_2016 == "Hillary Clinton" ~ "Clinton",
      Vote_2016 == "Was not eligible to vote" ~ NA_character_,
      TRUE ~ "Not Clinton"),
    v2016.not = case_when(
      Vote_2016 == "Did not vote" ~ "Did not vote",
      Vote_2016 == "Was not eligible to vote" ~ NA_character_,
      TRUE ~ "Did vote"),
    v2016.t = case_when(
      Vote_2016 == "Donald Trump" ~ "Trump",
      Vote_2016 == "Was not eligible to vote" ~ NA_character_,
      TRUE ~ "Not Trump"),
    repub = case_when(
      party2 == "Republican" ~ "Republican",
      TRUE ~ "Not Republican"),
    asian = case_when(
      race2 == "Asian" ~ "Asian",
      TRUE ~ "Not Asian"),
    democ = case_when(
      party2 == "Democrat" ~ "Democrat",
      TRUE ~ "Not Democrat"))
