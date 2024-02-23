#### CO-OP DEMOGRAPHICS ########################################################

# Canadian population -----------------------------------------------------

library(cancensus)

# Age
get_census("CA21", regions = list(C = 01), level = "PR", vectors = c(
  age_18 = "v_CA21_83", age_19 = "v_CA21_86", age_20_24 = "v_CA21_89",
  age_25_29 = "v_CA21_107", age_30_34 = "v_CA21_125", age_35_39 = "v_CA21_143",
  age_40_44 = "v_CA21_161", age_45_49 = "v_CA21_179", age_50_54 = "v_CA21_197",
  age_55_59 = "v_CA21_215", age_60_64 = "v_CA21_233", age_65 = "v_CA21_251")) |> 
  select(region = `Region Name`, age_18:age_65) |> 
  summarize(across(c(-region), sum)) |> 
  transmute(age_18_29 = age_18 + age_19 + age_20_24 + age_25_29,
            age_30_49 = age_30_34 + age_35_39 + age_40_44 + age_45_49,
            age_50_64 = age_50_54 + age_55_59 + age_60_64,
            age_65) |> 
  pivot_longer(everything(), names_to = "age", values_to = "n") |> 
  mutate(pct = n / sum(n)) |> 
  transmute(age, pct = scales::percent(pct, 0.1))

# Household size
get_census("CA21", regions = list(C = 01), level = "PR", vectors = c(
  size_1 = "v_CA21_444", size_2 = "v_CA21_445", size_3 = "v_CA21_446",
  size_4 = "v_CA21_447", size_5 = "v_CA21_448")) |> 
  select(region = `Region Name`, size_1:size_5) |> 
  summarize(across(c(-region), sum)) |> 
  transmute(size_1, size_2 = size_2 * 2, size_3 = size_3 * 3, 
            size_4 = size_4 * 4 + size_5 * 5) |> 
  pivot_longer(everything(), names_to = "size", values_to = "n") |> 
  mutate(pct = n / sum(n)) |> 
  transmute(size, pct = scales::percent(pct, 0.1))

# Gender
get_census("CA21", regions = list(C = 01), level = "PR", vectors = c(
  male = "v_CA21_9", female = "v_CA21_10")) |> 
  select(region = `Region Name`, male, female) |> 
  summarize(across(c(-region), sum)) |> 
  pivot_longer(everything(), names_to = "gender", values_to = "n") |> 
  mutate(pct = n / sum(n)) |> 
  arrange(gender) |> 
  transmute(gender, pct = scales::percent(pct, 0.1))

# Race/ethnicity
get_census("CA21", regions = list(C = 01), level = "PR", vectors = c(
  not_vis = "v_CA21_4914", white = "v_CA21_4968", black = "v_CA21_4884", 
  south_asian = "v_CA21_4878", chinese = "v_CA21_4881", 
  filipino = "v_CA21_4887", arab = "v_CA21_4890", 
  southeast_asian = "v_CA21_4896", west_asian = "v_CA21_4899", 
  korean = "v_CA21_4902", japanese = "v_CA21_4905", latin = "v_CA21_4893",
  indigenous = "v_CA21_4971")) |> 
  select(region = `Region Name`, not_vis:latin) |> 
  summarize(across(c(-region), sum)) |> 
  transmute(asian = south_asian + chinese + filipino + arab + southeast_asian +
              west_asian + korean + japanese, black, indigenous, latin, 
            white = not_vis + white) |> 
  pivot_longer(everything(), names_to = "race", values_to = "n") |> 
  mutate(pct = n / sum(n)) |> 
  transmute(race, pct = scales::percent(pct, 0.1))

# Immigration
get_census("CA21", regions = list(C = 01), vectors = c(
  imm = "v_CA21_4410", total = "v_CA21_4404")) |> 
  mutate(imm_pct = imm / total)

# Participant demographics ------------------------------------------------

# Age
transcripts |> 
  filter(category == "C") |> 
  distinct(transcript, hh_size, accommodation, indigenous, race, gender,
           language, country, age) |> 
  mutate(age = as.numeric(age)) |> 
  summarize(
    age_18_29 = sum(between(age, 18, 29), na.rm = TRUE),
    age_30_49 = sum(between(age, 30, 49), na.rm = TRUE),
    age_50_64 = sum(between(age, 50, 64), na.rm = TRUE),
    age_65 = sum(age > 64, na.rm = TRUE),
    age_withheld = sum(is.na(age))) |> 
  pivot_longer(everything()) |> 
  mutate(pct = scales::percent(value / sum(value)))

# HH size
transcripts |> 
  filter(category == "C") |> 
  distinct(transcript, hh_size, accommodation, indigenous, race, gender,
           language, country, age) |> 
  summarize(
    size_1 = sum(hh_size == 1, na.rm = TRUE),
    size_2 = sum(hh_size == 2, na.rm = TRUE),
    size_3 = sum(hh_size == 3, na.rm = TRUE),
    size_4 = sum(hh_size >= 4, na.rm = TRUE)) |> 
  pivot_longer(everything()) |> 
  mutate(pct = scales::percent(value / sum(value)))

# Gender
transcripts |> 
  filter(category == "C") |> 
  distinct(transcript, hh_size, accommodation, indigenous, race, gender,
           language, country, age) |> 
  summarize(
    female = sum(gender == "Female", na.rm = TRUE),
    male = sum(gender == "Male", na.rm = TRUE),
    non_binary = sum(gender == "Non-binary", na.rm = TRUE)) |> 
  pivot_longer(everything()) |> 
  mutate(pct = scales::percent(value / sum(value)))

# Race/ethnicity
transcripts |> 
  filter(category == "C") |> 
  distinct(transcript, hh_size, accommodation, indigenous, race, gender,
           language, country, age) |> 
  summarize(
    asian = sum(race %in% c("South Asian", "Asian")),
    black = sum(race == "Black/African"),
    hispanic = sum(race == "South American"),
    indigenous = sum(indigenous),
    white = sum(race %in% c("White", "White/Caucasian", "Canadian"))) |> 
  pivot_longer(everything()) |> 
  mutate(pct = scales::percent(value / sum(value)))

# Ability
transcripts |> 
  filter(category == "C") |> 
  distinct(transcript, hh_size, accommodation, indigenous, race, gender,
           language, country, age) |> 
  count(accommodation) |> 
  mutate(pct = scales::percent(n / sum(n)))

# Immigrant
transcripts |> 
  filter(category == "C") |> 
  distinct(transcript, hh_size, accommodation, indigenous, race, gender,
           language, country, age) |> 
  count(immigrant = country != "Canada") |> 
  mutate(pct = scales::percent(n / sum(n)))