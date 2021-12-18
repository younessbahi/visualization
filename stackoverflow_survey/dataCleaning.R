source('global/functions.R')
source('global/dependencies.R')

tictoc::tic("Importing Data", quiet = FALSE, func.tic = my.msg.tic)
#Download data from: https://insights.stackoverflow.com/survey
#Create directories and store the downloaded 2020 & 2021 datasets:
#dir.create('data')
#dir.create('data/2020')
#dir.create('data/2021')
survey20 <- read.csv('data/2020/survey_results_public.csv', header = T) %>% tibble
survey21 <- read.csv('data/2021/survey_results_public.csv', header = T) %>% tibble
tictoc::toc(quiet = FALSE, func.toc = my.msg.toc, info = "DONE")

tictoc::tic("Processing Data", quiet = FALSE, func.tic = my.msg.tic)
colnames(survey21) <- gsub('HaveWorkedWith', '', colnames(survey21))
colnames(survey20) <- gsub('WorkedWith', '', colnames(survey20))

#Extract currency symbol for later conversion
survey21 %<>% mutate(
  CurrencySymbol = if (! is.na(Currency)) str_extract(Currency, '^[A-Z]{3}') else NA
) %>%
  rename(Respondent = ResponseId)

#keep identical columns in both datasets
cols20     <- names(survey20)
cols21     <- names(survey21)
identical_ <- cols20[cols20 %in% cols21]

survey20_ <-
  survey20 %>%
    select(all_of(identical_)) %>%
    mutate(
      Period     = as.integer(2020),
      Respondent = paste0(Respondent, 'A')
    )

survey21_ <-
  survey21 %>%
    select(all_of(identical_)) %>%
    mutate(
      Period     = as.integer(2021),
      Respondent = paste0(Respondent, 'B')
    )

joined <-
  rbind(survey21_, survey20_)

#Convert available currencies to today last rate
currSymb <-
  na.omit(joined$CurrencySymbol) %>%
    unique

#Convert currencies to dollar last price
quote <-
  quantmod::getQuote(paste0(currSymb, 'USD', "=X")) %>%
    select(Last) %>%
    rownames_to_column('SymbConvertion')

joined %<>%
  rowwise() %>%
  mutate(
    SymbConvertion = paste0(CurrencySymbol, 'USD=X'),
  ) %>%
  dplyr::left_join(., quote, by = "SymbConvertion") %>%
  mutate(
    Converted  = {
      if (! is.na(CompFreq) & ! is.na(CompTotal)) {
        case_when(
          CompFreq == "Monthly" ~ (CompTotal * Last) * 12,
          CompFreq == "Weekly" ~ (CompTotal * Last) * 48,
          CompFreq == "Yearly" ~ (CompTotal * Last)
        )
      } else {
        CompTotal
      }
    },
    Converted_ = {
      if (! is.na(Converted)) {
        case_when(
          Converted < 700000 & Converted > 300 ~ Converted
        )
      } else NA
    },
    Gender     = {
      if (! is.na(Gender)) {
        case_when(
          str_detect(Gender, '^Man$') ~ 'Man',
          str_detect(Gender, '^Woman$') ~ 'Woman',
          str_detect(Gender, 'Man;Or, in your own words:') ~ 'Man',
          str_detect(Gender, 'Woman;Or, in your own words:') ~ 'Woman',
          str_detect(Gender, 'Man;Non-binary+') ~ 'Man',
          str_detect(Gender, 'Woman;Non-binary+') ~ 'Woman',
          TRUE ~ 'Other'
        )
      } else NA
    }
  ) %>%
  group_by(Country) %>%
  # calculate zscore by country
  mutate(
    zscore = ave(Converted_, Country, FUN = scale)
  ) %>%
  ungroup()


#Split and unnest multiple inputs in DevType var
df <- joined
df$DevType_split <- str_split(df$DevType, ';')
df %<>%
  unnest(cols = DevType_split) %>%
  filter(DevType_split != "Other (please specify):")


#Split and unnest multiple inputs in Language var
df$Language_split <- str_split(df$Language, ';')
df %<>%
  unnest(cols = Language_split)

DevTypes <- unique(df$DevType_split)

df %<>%
  mutate(
    Field = {
      case_when(
        str_detect(tolower(DevType_split), '^dev') ~ 'Developer',
        str_detect(tolower(DevType_split), '^engineer') ~ 'Engineer',
        str_detect(tolower(DevType_split), 'scien|analyst') ~ 'Scientist/Analyst/Researcher',
        str_detect(tolower(DevType_split), 'product|marketing|research') ~ 'Business/Marketing',
        str_detect(tolower(DevType_split), 'administrator') ~ 'IT',
        str_detect(tolower(DevType_split), 'executive') ~ 'Executive',
        str_detect(tolower(DevType_split), 'educ') ~ 'Educator',
        str_detect(tolower(DevType_split), 'design') ~ 'Designer',
        TRUE ~ 'Other'
      
      )
    }
  )
tictoc::toc(quiet = FALSE, func.toc = my.msg.toc, info = "DONE")
