# Analysis of Black Voter and Non-voter Demographics and Political Views
# Author: Zachary Lorico Hertz
# Date: November 2024
# Description: Analysis of 2020 CES data comparing Black voters and non-voters

# Load required libraries ------------------------------------------------
library(here)
library(tidyverse)
library(survey)
library(ggalt)
library(dataverse)

# Data Import ----------------------------------------------------------
# Function to import CES data from Harvard Dataverse
get_ces_data <- function(filename, dataset) {
  get_dataframe_by_name(
    filename = filename,
    dataset = dataset,
    original = TRUE,
    .f = haven::read_dta,
    server = "dataverse.harvard.edu"
  )
}

# Import 2020 CES data
ces20 <- get_ces_data(
  "CES20_Common_OUTPUT_vv.dta",
  "10.7910/DVN/E9N6PH"
)


# Data Processing ------------------------------------------------------
# Function to process CES data and create analysis variables
process_data <- function(data) {
  data %>%
    subset(race == 2) %>%
    filter(!is.na(commonpostweight)) %>%
    mutate(
      # Demographics --------------
      # Convert gender to descriptive categories
      gender = case_when(
        gender == 1 ~ "Male",
        gender == 2 ~ "Female",
        TRUE ~ NA_character_
      ),
      
      # Create age generational variable
      age = case_when(
        (2020 - birthyr) < 30 ~ "18 to 29",
        (2020 - birthyr) >= 30 & (2020 - birthyr) < 50 ~ "30 to 49",
        (2020 - birthyr) >= 50 & (2020 - birthyr) < 65 ~ "50 to 64",
        (2020 - birthyr) >= 65 ~ "65+"
      ),
      
      # Create income variable with ordered factor levels
      income = case_when(
        faminc_new %in% c(1,2,3) ~ "Under $30,000",
        faminc_new %in% c(4,5) ~ "$30,000 to $49,999",
        faminc_new %in% c(6,7,8,9) ~ "$50,000 to $99,999",
        faminc_new %in% c(10,11,12) ~ "$100,000 to $199,999",
        faminc_new %in% c(13,14,15,16) ~ "$200,000 or more",
        faminc_new == 97 ~ NA_character_,
        TRUE ~ NA_character_
      ),
      
      # Convert to ordered factor
      income = factor(income, 
                      levels = c("Under $30,000",
                                 "$30,000 to $49,999",
                                 "$50,000 to $99,999",
                                 "$100,000 to $199,999",
                                 "$200,000 or more"),
                      ordered = TRUE),
      
      # Create parental status indicator
      parent.18 = case_when(
        child18 == 1 ~ "Parent of a child under 18",
        child18 == 2 ~ "Not a parent of a child under 18",
        TRUE ~ NA_character_
      ),
      
      # Create employment status indicator
      employmentft = case_when(
        employ == 1 ~ "Employed full-time",
        employ > 1 & employ < 10 ~ "Not employed full-time",
        TRUE ~ NA_character_
      ),
      
      # Education level
      college = if_else(educ < 3, "No college", "College"),
      
      # Voting Behavior --------------
      voter = !is.na(CL_2020gvm),
      biden = CC20_410 == 1,
      vote.3.way = case_when(
        CC20_410 == 1 ~ "Biden",
        CC20_410 == 2 ~ "Trump",
        CC20_410 > 2 ~ "Other"
      ),
      
      # Policy preferences (1 = support, 0 = oppose)
      # Economic policies
      gmw = case_when(
        CC20_350b == 1 ~ 1,
        CC20_350b == 2 ~ 0,
        TRUE ~ 0
      ),  # $15 minimum wage
      oequalpay = case_when(
        CC20_350d == 1 ~ 1,
        CC20_350d == 2 ~ 0,
        TRUE ~ 0
      ),  # Equal pay for equal work
      
      # Healthcare policies
      am4a = case_when(
        CC20_327a == 1 ~ 1,
        CC20_327a == 2 ~ 0,
        TRUE ~ 0
      ),  # Medicare for All
      lowerage = case_when(
        CC20_327c == 1 ~ 1,
        CC20_327c == 2 ~ 0,
        TRUE ~ 0
      ),  # Lower Medicare age to 50
      zaca = case_when(
        CC20_327d == 1 ~ 1,
        CC20_327d == 2 ~ 0,
        TRUE ~ 0
      ),  # Repeal ACA
      tmandate = case_when(
        CC20_327e == 1 ~ 1,
        CC20_327e == 2 ~ 0,
        TRUE ~ 0
      ),  # Restore ACA mandate
      
      # Gun policies
      yarban = case_when(
        CC20_330b == 1 ~ 1,
        CC20_330b == 2 ~ 0,
        TRUE ~ 0
      ),  # Ban assault rifles
      sconceal = case_when(
        CC20_330c == 1 ~ 1,
        CC20_330c == 2 ~ 0,
        TRUE ~ 0
      ),  # Easier concealed carry
      
      # Immigration policies
      bpatrols = case_when(
        CC20_331b == 1 ~ 1,
        CC20_331b == 2 ~ 0,
        TRUE ~ 0
      ),  # Increase border patrol
      wdeptreports = case_when(
        CC20_331c == 1 ~ 1,
        CC20_331c == 2 ~ 0,
        TRUE ~ 0
      ),  # Report immigration status
      pspendwall = case_when(
        CC20_331e == 1 ~ 1,
        CC20_331e == 2 ~ 0,
        TRUE ~ 0
      ),  # Border wall funding
      
      # Abortion policies
      nchoice = case_when(
        CC20_332a == 1 ~ 1,
        CC20_332a == 2 ~ 0,
        TRUE ~ 0
      ),  # Pro-choice
      jabortsafe = case_when(
        CC20_332b == 1 ~ 1,
        CC20_332b == 2 ~ 0,
        TRUE ~ 0
      ),  # Exceptions only
      kabortban = case_when(
        CC20_332f == 1 ~ 1,
        CC20_332f == 2 ~ 0,
        TRUE ~ 0
      ),  # Total ban
      eweek20 = case_when(
        CC20_332c == 1 ~ 1,
        CC20_332c == 2 ~ 0,
        TRUE ~ 0
      ),  # 20-week ban
      
      # Criminal justice policies
      vmnsnt = case_when(
        CC20_334a == 1 ~ 1,
        CC20_334a == 2 ~ 0,
        TRUE ~ 0
      ),  # End mandatory minimums
      rbodycam = case_when(
        CC20_334b == 1 ~ 1,
        CC20_334b == 2 ~ 0,
        TRUE ~ 0
      ),  # Body cameras
      dfund = case_when(
        CC20_334c == 1 ~ 1,
        CC20_334c == 2 ~ 0,
        TRUE ~ 0
      ),  # Increase police funding
      cdefund = case_when(
        CC20_334d == 1 ~ 1,
        CC20_334d == 2 ~ 0,
        TRUE ~ 0
      ),  # Decrease police funding
      uchoke = case_when(
        CC20_334e == 1 ~ 1,
        CC20_334e == 2 ~ 0,
        TRUE ~ 0
      ),  # Ban chokeholds
      
      # Other policies
      paris = case_when(
        CC20_355a == 1 ~ 1,
        CC20_355a == 2 ~ 0,
        TRUE ~ 0
      ),  # Paris Agreement
      ltransban = case_when(
        CC20_355d == 1 ~ 1,
        CC20_355d == 2 ~ 0,
        TRUE ~ 0
      ),  # Trans military ban
      fsnap = case_when(
        CC20_355e == 1 ~ 1,
        CC20_355e == 2 ~ 0,
        TRUE ~ 0
      ),  # SNAP work requirements
      
      # Racial attitudes
      mwadv = case_when(
        CC20_440a < 3 ~ 1,
        CC20_440a >= 3 ~ 0,
        TRUE ~ 0
      ),  # White advantage
      saisolate = case_when(
        CC20_440b < 3 ~ 1,
        CC20_440b >= 3 ~ 0,
        TRUE ~ 0
      ),  # Racial problems isolated
      qwadv2 = case_when(
        CC20_441f < 3 ~ 1,
        CC20_441f >= 3 ~ 0,
        TRUE ~ 0
      ),  # Structural racism
      wsxism_b = case_when(
        CC20_440d < 3 ~ 1,
        CC20_440d >= 3 ~ 0,
        TRUE ~ 0
      ),  # Women easily offended
      istructural = case_when(
        CC20_441b < 3 ~ 1,
        CC20_441b >= 3 ~ 0,
        TRUE ~ 0
      ),  # Historical disadvantage
      
      # Police attitudes
      hpolice.unsafe = case_when(
        CC20_307 > 2 ~ 1,
        CC20_307 <= 2 ~ 0,
        TRUE ~ 0
      )  # Police safety feelings
    )
}
# Process data and create survey design object
black.adults <- process_data(ces20)
black.svydat <- svydesign(
  ids = ~0,
  data = black.adults, 
  weights = ~commonpostweight
)

# Demographic Comparisons --------------------------------------------------
# Create demographics comparison table with significance tests
create_demo_table <- function(design) {
  # Helper function to get percentages and test significance
  get_stats <- function(var_name, category_name) {
    # Get percentages
    pcts <- svytable(as.formula(paste0("~", var_name, " + voter")), 
                     design = design) %>%
      as.data.frame() %>%
      setNames(c("category", "voter_status", "n")) %>%
      group_by(voter_status) %>%
      mutate(pct = n/sum(n) * 100) %>%
      ungroup() %>%
      select(-n) %>%
      pivot_wider(
        names_from = voter_status,
        values_from = pct,
        names_prefix = "voter_"
      )
    
    # Test for significant differences
    sig_test <- svychisq(
      as.formula(paste0("~", var_name, " + voter")),
      design = design
    )
    
    # Add significance indicator and category
    pcts %>%
      mutate(
        sig = if(sig_test$p.value < 0.05) "*" else "",
        Category = category_name
      )
  }
  
  # Get stats for each demographic variable with explicit category names
  gender_stats <- get_stats("gender", "Gender")
  age_stats <- get_stats("age", "Age")
  college_stats <- get_stats("college", "Education")
  parent_stats <- get_stats("parent.18", "Parental Status")
  income_stats <- get_stats("income", "Income")
  employ_stats <- get_stats("employmentft", "Employment")
  
  demo_table <- bind_rows(
    gender_stats,
    age_stats,
    college_stats,
    parent_stats,
    income_stats,
    employ_stats
  ) %>%
    rename(
      "Characteristic" = category,
      "Non-voters" = voter_FALSE,
      "Voters" = voter_TRUE
    ) %>%
    mutate(
      across(c(`Non-voters`, `Voters`), round, 0),
      `Voters` = paste0(`Voters`, sig),
      `Non-voters` = paste0(`Non-voters`, "%"),
      `Voters` = paste0(`Voters`, "%")
    ) %>%
    select(-sig) %>%
    select(Category, Characteristic, `Non-voters`, `Voters`) %>%
    mutate(
      Characteristic = case_when(
        Category == "Income" & grepl(" to ", Characteristic) ~ 
          gsub(" to ", "<span style='font-style: normal;'> to </span>", Characteristic),
        TRUE ~ Characteristic
      )
    )
  
  return(demo_table)
}

# Create and print the table
demo_table <- create_demo_table(black.svydat)

# Voter Choice Plot ------------------------------------------------------
# Calculate voter choice proportions with confidence intervals
calc_vote_choice <- function(design) {
  # Calculate proportions by voter status and vote choice
  svyby(~vote.3.way, ~voter, design, svymean, na.rm = TRUE) %>%
    # Reshape data to separate vote choices and standard errors
    pivot_longer(
      cols = starts_with(c("vote.3.way", "se.vote.3.way")),
      names_to = c(".value", "vote_choice"),
      names_pattern = "(vote.3.way|se.vote.3.way)(.*)"
    ) %>%
    # Convert to percentages and calculate confidence intervals
    mutate(
      vote.3.way = vote.3.way * 100,
      se.vote.3.way = se.vote.3.way * 100,
      upper = vote.3.way + 1.96 * se.vote.3.way,
      lower = vote.3.way - 1.96 * se.vote.3.way,
      # Order vote choices with Biden at top
      vote_choice = factor(vote_choice, levels = c("Other", "Trump", "Biden")),
      # Properly convert voter status to factor
      voter = factor(as.numeric(voter), levels = c(0, 1))  # Fixed this line
    )
}

# Create vote choice visualization
create_vote_plot <- function(data) {
  ggplot(data, 
         aes(y = vote_choice, x = vote.3.way, color = voter)) +
    geom_pointrange(aes(xmin = lower, xmax = upper), 
                    position = position_dodge(width = 0.3)) +
    scale_color_manual(labels = c("Non-voter", "Voters"),
                       values = c("#88498F", "#73956F")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(y = "Vote Choice",
         x = "Percent Vote Share",
         color = "") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(0, 100, by = 5), limits = c(0, 100)) +
    ggtitle("Black 2020 nonvoters backed Trump more than Black 2020 voters, but not by much") +
    labs(caption = "Plot: Zachary L. Hertz\nData: n=3,096 Black voters and n=1,816 Black non-voters from 2020 CES\nBars indicate 95 percent confidence intervals")
}

# Calculate and create vote choice plot
vote_choice_data <- calc_vote_choice(black.svydat)
vote_plot <- create_vote_plot(vote_choice_data)

# Analysis Functions ------------------------------------------------------
# Calculate proportions with confidence intervals
calc_prop_ci <- function(var, design) {
  # Get the results from svyby
  results <- svyby(
    as.formula(paste0("~", var)), 
    ~voter, 
    design, 
    svymean, 
    na.rm = TRUE
  )
  
  # Extract the actual column names from results
  value_col <- names(results)[2]  # First column is voter, second is the value
  se_col <- names(results)[3]     # Third column is the standard error
  
  # Add confidence intervals and standardize column names
  results_processed <- results %>%
    # Add confidence intervals using the actual column names
    mutate(
      value = !!sym(value_col),
      se = !!sym(se_col),
      upper = value + 1.96 * se,
      lower = value - 1.96 * se,
      variable = var
    ) %>%
    # Select only the columns we need
    select(voter, value, se, upper, lower, variable)
  
  return(results_processed)
}

# Function to test statistical significance
test_significance <- function(voter_mean, nonvoter_mean, voter_se, nonvoter_se) {
  # Calculate z-score for difference between means
  diff <- abs(voter_mean - nonvoter_mean)
  se_diff <- sqrt(voter_se^2 + nonvoter_se^2)
  z_score <- diff / se_diff
  
  # Two-tailed test at 95% confidence level (critical value = 1.96)
  return(z_score > 1.96)
}

# Create wide format data for plotting with significance testing
create_wide_format <- function(results_list) {
  # Combine all results
  combined_results <- bind_rows(results_list) %>%
    group_by(variable) %>%
    mutate(
      # Test if difference is statistically significant
      is_significant = test_significance(
        voter_mean = value[voter == 1],
        nonvoter_mean = value[voter == 0],
        voter_se = se[voter == 1],
        nonvoter_se = se[voter == 0]
      )
    ) %>%
    ungroup()
  
  # Create wide format
  wide_data <- combined_results %>%
    select(variable, voter, value, is_significant) %>%
    mutate(
      type = if_else(voter == 1, "Voters", "Nonvoters")
    ) %>%
    select(-voter) %>%
    pivot_wider(
      names_from = type,
      values_from = value
    ) %>%
    mutate(
      Nonvoters = Nonvoters * 100,
      Voters = Voters * 100,
      difference = Nonvoters - Voters,
      absdif = abs(difference),
      statsig = if_else(is_significant, "#457EAC", "#DADFF7")
    )
  
  return(wide_data)
}

# Execute Analysis ------------------------------------------------------
# Calculate proportions for all variables
vars_to_analyze <- c(
  "am4a", "zaca", "lowerage", "tmandate",
  "yarban", "sconceal",
  "bpatrols", "wdeptreports", "pspendwall",
  "gmw", "fsnap", "oequalpay",
  "nchoice", "jabortsafe", "kabortban", "eweek20",
  "vmnsnt", "rbodycam", "dfund", "cdefund", "uchoke",
  "istructural", "mwadv", "qwadv2", "saisolate", "wsxism_b", "ltransban"
)

# Calculate all proportions
prop_list <- map(vars_to_analyze, ~calc_prop_ci(.x, black.svydat))

# Create final datasets with significance testing
fulldat <- create_wide_format(prop_list) %>%
  mutate(
    issue_lab = case_when(
      variable == "zaca" ~ "Repeal the Affordable Care Act.",
      variable == "yarban" ~ "Ban assault rifles.",
      variable == "wsxism_b" ~ "Women are too easily offended.",
      variable == "wdeptreports" ~ "Withhold federal funds from police departments that do not report illegal immigrants.",
      variable == "vmnsnt" ~ "Eliminate mandatory minimum sentences for non-violent drug offenders.",
      variable == "uchoke" ~ "Ban the use of choke holds by police.",
      variable == "tmandate" ~ "Restore the Affordable Care Act's individual mandate.",
      variable == "sconceal" ~ "Make it easier for people to obtain concealed-carry permits.",
      variable == "saisolate" ~ "Racial problems in the U.S. are rare, isolated situations.",
      variable == "rbodycam" ~ "Require police officers to wear body cameras while on duty.",
      variable == "qwadv2" ~ "Whites get away with offenses that African Americans would never get away with.",
      variable == "pspendwall" ~ "Increase spending on border security by $25 billion, including building a wall.",
      variable == "oequalpay" ~ "Require equal pay for women and men doing similar jobs with similar qualifications.",
      variable == "nchoice" ~ "Always allow a woman to obtain an abortion as a matter of choice.",
      variable == "mwadv" ~ "White people in the U.S. have certain advantages because of the color of their skin.",
      variable == "ltransban" ~ "Ban Transgender People in the Military.",
      variable == "lowerage" ~ "Lower the eligibility age for Medicare from 65 to 50.",  # Added this line
      variable == "kabortban" ~ "Make abortions illegal in all circumstances.",
      variable == "jabortsafe" ~ "Permit abortion only in case of rape, incest or when the woman's life is in danger.",
      variable == "istructural" ~ "Generations of slavery and discrimination have created conditions that make it difficult for blacks to work their way out of the lower class.",
      variable == "hpolice.unsafe" ~ "Do the police make you feel safe?",
      variable == "gmw" ~ "Raise the minimum wage to $15 an hour.",
      variable == "fsnap" ~ "Require able-bodied adults under 50 to have a job to receive food stamps.",
      variable == "eweek20" ~ "Prohibit all abortions after the 20th week of pregnancy.",
      variable == "dfund" ~ "Increase the number of police on the street by 10 percent.",
      variable == "cdefund" ~ "Decrease the number of police on the street by 10 percent.",
      variable == "bpatrols" ~ "Increase the number of border patrols on the US-Mexican border.",
      variable == "am4a" ~ "Expand Medicare to a single public health care program that covers all Americans."
    )
  )

# Plotting Functions -----------------------------------------------------
create_dumbbell_plot <- function(data, title, xlab, wrap_text = FALSE) {
  # First sort the data by absolute difference
  data <- data %>%
    mutate(
      # Optionally wrap text and then create ordered factor
      issue_lab = if(wrap_text) {
        factor(str_wrap(issue_lab, width = 50), 
               levels = str_wrap(issue_lab[order(absdif)], width = 50))  # Note the minus sign to order from largest to smallest
      } else {
        factor(issue_lab, 
               levels = issue_lab[order(absdif)])  # Note the minus sign to order from largest to smallest
      }
    )
  
  # Create long format for points
  data_long <- data %>%
    pivot_longer(
      cols = c(Nonvoters, Voters),
      names_to = "group",
      values_to = "value"
    )
  
  # Create plot
  ggplot(data, aes(y = issue_lab)) +
    geom_point(
      data = data_long, 
      aes(x = value, color = group), 
      size = 3
    ) +
    geom_dumbbell(
      aes(x = Nonvoters, xend = Voters),
      size = 1.5, 
      color = data$statsig, 
      size_x = 3, 
      size_xend = 3,
      colour_x = "#88498F", 
      colour_xend = "#73956F"
    ) +
    scale_color_manual(
      name = "", 
      values = c("Nonvoters" = "#88498F", "Voters" = "#73956F")
    ) +
    theme_minimal() +
    labs(
      x = xlab,
      y = "",
      title = title,
      caption = "Plot: Zachary L. Hertz\nData: n=3,096 Black voters and n=1,816 Black non-voters from 2020 CES\nBlue bars indicate differences that are statistically significant at the 95 percent confidence level."
    ) +
    theme(legend.position = "bottom") +
    xlim(0, 100) +
    geom_vline(xintercept = 50, linetype = "dotted")
}

# Create plots
policy_plot <- create_dumbbell_plot(
  subset(fulldat, !variable %in% c("wsxism_b", "saisolate", "qwadv2", "mwadv", "istructural", "hpolice.unsafe")),
  "How do Black 2020 voters' policy preferences differ from Black 2020 nonvoters?",
  "Percent who support a proposal to...",
  wrap_text = FALSE
)

attitudes_plot <- create_dumbbell_plot(
  subset(fulldat, variable %in% c("wsxism_b", "saisolate", "qwadv2", "mwadv", "istructural", "hpolice.unsafe")),
  "How do attitudes among Black 2020 voters differ from Black 2020 nonvoters?",
  "Percent who agree with the statement that...",
  wrap_text = TRUE
)

# Save outputs ------------------------------------------------------
# ggsave("plot_votes.png", vote_plot, width = 10, height = 6, dpi = 900)
# ggsave("plot_policy.png", policy_plot, width = 12, height = 6, dpi = 900)
# ggsave("plot_attitudes.png", attitudes_plot, width = 9.5, height = 6, dpi = 900)
# Save table (if needed for external use)
# write_csv(demo_table, "demographic_comparison.csv")