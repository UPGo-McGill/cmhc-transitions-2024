#### Descriptive census data ###################################################

library(tidyverse)
library(cancensus)


# 1.1 Canadian household mobility -----------------------------------------

# 1-year and 5-year mobility
get_census("CA21", region = list(C = "01"),
           vectors = c(move_1 = "v_CA21_5751", total_1 = "v_CA21_5745",
                       move_5 = "v_CA21_5778", total_5 = "v_CA21_5772")) |> 
  summarize(move_1_pct = move_1 / total_1, 
            move_5_pct = move_5 / total_5)

