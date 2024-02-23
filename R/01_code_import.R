#### 01 CODE IMPORT ############################################################

library(tidyverse)


# Import codes ------------------------------------------------------------

code_paths <- 
  list.files("data", full.names = TRUE) |> 
  str_subset("Codes.csv")

codes <-
  # Take the most recent file that matches
  code_paths[length(code_paths)] |> 
  read_csv(show_col_types = FALSE, na = "") |> 
  select(code = `Code Name`,
         n = `Number of Snippets`,
         url = `Code URL`) |> 
  arrange(code) |> 
  mutate(category = str_remove(code, "-.*"), .after = code) |> 
  group_by(code, category) |> 
  summarize(n = sum(n), url = first(url), .groups = "drop")


# Import code crosstab ----------------------------------------------------

crosstab_paths <- 
  list.files("data", full.names = TRUE) |> 
  str_subset("CodeXCode.csv")

crosstabs <-
  # Take the most recent file that matches
  crosstab_paths[length(crosstab_paths)] |> 
  read_csv(skip = 4, na = "", show_col_types = FALSE) |> 
  select(-1, -2) |> 
  rename(code_1 = "Code Name") |> 
  pivot_longer(cols = -code_1, names_to = "code_2", values_to = "n") |> 
  filter(!is.na(n))


# Import descriptor crosstab ----------------------------------------------

descriptor_paths <- 
  list.files("data", full.names = TRUE) |> 
  str_subset("Descriptor.csv")

descriptors <-
  # Take the most recent file that matches
  descriptor_paths[length(descriptor_paths)] |> 
  read_csv(skip = 4, na = "", show_col_types = FALSE) |> 
  select(-1, -2) |> 
  rename(code = "Code Name") |> 
  pivot_longer(cols = -code, names_to = "descriptor", values_to = "n") |> 
  filter(!is.na(n))


# Import transcript crosstab ----------------------------------------------

transcript_paths <- 
  list.files("data", full.names = TRUE) |> 
  str_subset("Transcript.csv")

transcripts <-
  # Take the most recent file that matches
  transcript_paths[length(transcript_paths)] |> 
  read_csv(skip = 4, na = "", show_col_types = FALSE) |> 
  select(-1, -2) |> 
  rename(code = "Code Name") |> 
  pivot_longer(cols = -code, names_to = "transcript", values_to = "n") |> 
  filter(!is.na(n)) |> 
  mutate(category = str_remove(code, "-.*"), .after = code)


# Import snippets ---------------------------------------------------------

snippet_paths <- 
  list.files("data", full.names = TRUE) |> 
  str_subset("Snippets.csv")

snippet_probs <- 
  suppressWarnings(snippet_paths[length(snippet_paths)] |> 
                     read_csv(na = "", show_col_types = FALSE))

snippet_rows <- nrow(snippet_probs)
  
snippet_cols <- 
  snippet_probs |> 
  problems() |> 
  count(actual) |> 
  pull(actual) |> 
  str_subset("columns") |> 
  str_remove(" columns") |> 
  as.integer() |> 
  max()

snippet_base_col_names <- 
  c("transcript", "snippet", "URL", "memo", "coder", "hh_size", "accommodation",
    "indigenous", "race", "gender", "language", "country", "age")

snippet_col_names <- 
  c(snippet_base_col_names, paste0(
    "code_", seq_len(snippet_cols - length(snippet_base_col_names))))

snippets <-
  map(seq_len(snippet_rows), \(x) {
    snippet_paths[length(snippet_paths)] |> 
      read_csv(skip = x, n_max = 1, na = "", col_names = snippet_col_names,
               col_types = paste0(paste(rep("c", 16), collapse = ""), "dd", 
                                  paste(rep("c", snippet_cols - length(
                                    snippet_base_col_names)), collapse = "")),
               show_col_types = FALSE)}) |> 
  bind_rows() |> 
  rowwise() |> 
  mutate(codes = list(c(code_1, code_2, code_3))) |> 
  mutate(codes = list(codes[!is.na(codes)])) |> 
  ungroup() |> 
  select(all_of(c(snippet_base_col_names, "codes"))) |> 
  select(-memo, -coder)


# Join descriptors to transcripts -----------------------------------------

transcripts <- 
  snippets |> 
  group_by(transcript) |> 
  slice(1) |> 
  ungroup() |> 
  select(-snippet, -URL) |> 
  right_join(transcripts, by = "transcript", multiple = "all") |> 
  relocate(transcript, code, category, n) |> 
  arrange(transcript, category, code)


# Additional tweaks -------------------------------------------------------

transcripts <- 
  transcripts |> 
  mutate(indigenous = case_when(indigenous == "No" ~ FALSE,
                                indigenous == "Yes" ~ TRUE,
                                .default = NA))

snippets <- 
  snippets |> 
  mutate(indigenous = case_when(indigenous == "No" ~ FALSE,
                                indigenous == "Yes" ~ TRUE,
                                .default = NA))

# Add category to snippets
stopifnot(0 == transcripts |> 
            count(transcript, category) |> 
            filter(n() > 1, .by = transcript) |> 
            nrow())

snippets <- 
  snippets |> 
  inner_join(distinct(transcripts, transcript, category), by = "transcript") |> 
  relocate(category, .after = transcript)


# Clean up ----------------------------------------------------------------

rm(code_paths, crosstab_paths, descriptor_paths, snippet_base_col_names, 
   snippet_col_names, snippet_cols, snippet_paths, snippet_probs, snippet_rows, 
   transcript_paths)
  