library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(plotly)
library(stringr)
library(grid)
library(htmlwidgets)
library(maps)

# Load data
sipp_updated <- read_csv("data/sipp_updated.csv", show_col_types = FALSE)

# Theme colors
col_2020 <- "#82AC7C"
col_2023 <- "#54738E"

year_colors <- c("2020" = col_2020, "2023" = col_2023)
gender_colors <- c("Male" = col_2020, "Female" = col_2023)
poverty_colors <- c("Not in poverty" = col_2020, "In poverty" = col_2023)

activity_colors <- c(
  "After-School Programs" = "#A3C299",
  "Clubs" = "#82AC7C",
  "Lessons" = "#6F9B95",
  "Sports" = "#54738E",
  "Any Extracurricular" = "#3E576A",
  "Clubs or Organizations" = "#82AC7C",
  "After-School Lessons" = "#A3C299",
  "Sports Teams" = "#54738E",
  "Before/After-School Programs" = "#6F9B95"
)

theme_set(theme_minimal(base_size = 14))

## Question 1

## Parent Education
parent_edu_plot_data <- sipp_updated %>%
  filter(!is.na(Any_extracurricular),
         !is.na(Weight),
         !is.na(Parents_college),
         Survey_year %in% c(2021, 2024)) %>%
  mutate(
    Survey_year = recode(as.character(Survey_year),
                         "2021" = "2020",
                         "2024" = "2023"),
    ParentEduCat = factor(
      ifelse(Parents_college == 0,
             "Both parents high school or less",
             "At least one parent some college or higher"),
      levels = c("Both parents high school or less",
                 "At least one parent some college or higher")
    )
  ) %>%
  group_by(Survey_year, ParentEduCat) %>%
  summarize(
    extr_rate = weighted.mean(Any_extracurricular, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hover_text = paste0(
      "Year: ", Survey_year, "<br>",
      "Parent education: ", ParentEduCat, "<br>",
      "Participation: ", percent(extr_rate, accuracy = 0.1)
    )
  )

p_parent_edu <- ggplot(
  parent_edu_plot_data,
  aes(x = ParentEduCat, y = extr_rate, fill = Survey_year, text = hover_text)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(y = extr_rate + 0.03, label = percent(extr_rate, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.7
  ) +
  scale_x_discrete(labels = c(
    "Both parents high school or less" = "Both parents\nhigh school or less",
    "At least one parent some college or higher" =
      "At least one parent\nsome college or higher"
  )) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = year_colors, name = "Year") +
  labs(
    title = "Extracurricular Participation by Parent Education Category",
    x = "Parent Education Category",
    y = "% Participating in Extracurriculars",
    caption = "Any Extracurricular = sports, clubs, lessons, or before/after-school care program"
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

## Family Type
family_type_plot_data <- sipp_updated %>%
  filter(!is.na(Any_extracurricular),
         !is.na(Weight),
         !is.na(Family_type),
         Survey_year %in% c(2021, 2024)) %>%
  mutate(
    Survey_year = recode(as.character(Survey_year),
                         "2021" = "2020",
                         "2024" = "2023"),
    FamilyType = factor(
      Family_type,
      levels = c(1, 2, 3),
      labels = c("Married couple",
                 "Female, no spouse present",
                 "Male, no spouse present")
    )
  ) %>%
  group_by(Survey_year, FamilyType) %>%
  summarize(
    extr_rate = weighted.mean(Any_extracurricular, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hover_text = paste0(
      "Year: ", Survey_year, "<br>",
      "Family type: ", FamilyType, "<br>",
      "Participation: ", percent(extr_rate, accuracy = 0.1)
    )
  )

p_family_type <- ggplot(
  family_type_plot_data,
  aes(x = FamilyType, y = extr_rate, fill = Survey_year, text = hover_text)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(y = extr_rate + 0.03, label = percent(extr_rate, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.7
  ) +
  scale_x_discrete(labels = c(
    "Married couple" = "Married\ncouple",
    "Female, no spouse present" = "Female,\nno spouse",
    "Male, no spouse present" = "Male,\nno spouse"
  )) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = year_colors, name = "Year") +
  labs(
    title = "Extracurricular Participation by Family Type",
    x = "Family Type",
    y = "% Participating in Extracurriculars"
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

## Race
race_df <- sipp_updated %>%
  select(Survey_year, TRACE, Weight, Sports, Clubs, Lessons,
         After_school_program, Any_extracurricular) %>%
  pivot_longer(
    cols = c(After_school_program, Clubs, Lessons, Sports, Any_extracurricular),
    names_to = "Activity",
    values_to = "Participation"
  ) %>%
  mutate(
    Survey_year = recode(as.character(Survey_year),
                         "2021" = "2020",
                         "2024" = "2023"),
    Activity = case_when(
      Activity == "After_school_program" ~ "After-School Programs",
      Activity == "Clubs" ~ "Clubs",
      Activity == "Lessons" ~ "Lessons",
      Activity == "Sports" ~ "Sports",
      Activity == "Any_extracurricular" ~ "Any Extracurricular"
    ),
    Activity = factor(
      Activity,
      levels = c("After-School Programs", "Clubs", "Lessons", "Sports", "Any Extracurricular")
    ),
    Race = case_when(
      TRACE == 1 ~ "White",
      TRACE == 2 ~ "Black",
      TRACE == 3 ~ "AI/AN",
      TRACE == 4 ~ "Asian",
      TRACE == 5 ~ "NH/PI",
      TRACE >= 6 ~ "Multiracial"
    )
  ) %>%
  filter(!is.na(Participation), !is.na(Weight), !is.na(Race)) %>%
  group_by(Survey_year, Race, Activity) %>%
  summarize(
    Participation_Rate = weighted.mean(Participation, w = Weight, na.rm = TRUE),
    .groups = "drop"
  )

race_order <- race_df %>%
  filter(Activity == "Any Extracurricular", Survey_year == "2023") %>%
  arrange(Participation_Rate) %>%
  pull(Race)

race_df <- race_df %>%
  mutate(
    Race = factor(Race, levels = race_order),
    Survey_year = factor(Survey_year, levels = c("2020", "2023")),
    LabelText = ifelse(Participation_Rate == 0, "", percent(Participation_Rate, accuracy = 1)),
    hover_text = paste0(
      "Year: ", Survey_year, "<br>",
      "Race: ", Race, "<br>",
      "Activity: ", Activity, "<br>",
      "Participation: ", percent(Participation_Rate, accuracy = 0.1)
    )
  )

p_race <- ggplot(
  race_df,
  aes(x = Participation_Rate, y = Race, fill = Survey_year, text = hover_text)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(
      label = LabelText,
      x = ifelse(Participation_Rate == 0, NA, pmin(Participation_Rate + 0.07, 0.98))
    ),
    position = position_dodge(width = 0.8),
    hjust = 0,
    size = 2.2,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  facet_wrap(~ Activity, ncol = 3) +
  scale_fill_manual(
    values = year_colors,
    name = "Year",
    labels = c("2020" = "Green = 2020", "2023" = "Blue = 2023")
  ) +
  scale_x_continuous(
    labels = percent,
    limits = c(0, 1.0),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Extracurricular Participation by Race",
    x = "Participation Rate",
    y = "Race Category"
  ) +
  theme(
    legend.position = "bottom",
    panel.spacing.y = unit(.5, "lines"),
    axis.text.x = element_text(size = 7)
  )

## Question 2

## Poverty
poverty_plot_data <- sipp_updated %>%
  filter(!is.na(Any_extracurricular),
         !is.na(Weight),
         !is.na(Poverty),
         Survey_year %in% c(2021, 2024)) %>%
  group_by(Survey_year, Poverty) %>%
  summarize(
    extr_rate = weighted.mean(Any_extracurricular, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Poverty = factor(Poverty, levels = c(0, 1),
                     labels = c("Not in poverty", "In poverty")),
    DisplayYear = factor(Survey_year,
                         levels = c(2021, 2024),
                         labels = c("2020", "2023")),
    hover_text = paste0(
      "Year: ", DisplayYear, "<br>",
      "Poverty status: ", Poverty, "<br>",
      "Participation: ", percent(extr_rate, accuracy = 0.1)
    )
  )

p_poverty <- ggplot(
  poverty_plot_data,
  aes(x = DisplayYear, y = extr_rate, fill = Poverty, text = hover_text)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(y = extr_rate + 0.03, label = percent(extr_rate, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.8
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.85),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = poverty_colors, name = "Poverty Status") +
  labs(
    title = "Extracurricular Participation by Year and Poverty Status",
    x = "Year",
    y = "% Participating in Extracurriculars"
  )

## Age
age_plot_data <- sipp_updated %>%
  filter(!is.na(Any_extracurricular),
         !is.na(Age),
         !is.na(Weight),
         Survey_year %in% c(2021, 2024)) %>%
  group_by(Survey_year, Age) %>%
  summarize(
    extr_rate = weighted.mean(Any_extracurricular, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    DisplayYear = factor(
      Survey_year,
      levels = c(2021, 2024),
      labels = c("2020", "2023")
    ),
    hover_text = paste0(
      "Year: ", DisplayYear, "<br>",
      "Age: ", Age, "<br>",
      "Participation: ", scales::percent(extr_rate, accuracy = 0.1)
    )
  )

p_age <- ggplot(
  age_plot_data,
  aes(x = Age, y = extr_rate,
      color = DisplayYear,
      group = DisplayYear,   # <- ensure line connects by year
      text  = hover_text)
) +
  geom_point(size = 2.5, alpha = 0.9) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = year_colors, name = "Year") +
  coord_cartesian(ylim = c(0.30, 0.70)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Age",
    y = "% Participating in Extracurriculars",
    title = "Extracurricular Participation by Age"
  ) +
  theme(axis.text.x = element_text(size = 8))

## Gender
plot_data_gender <- sipp_updated %>%
  select(Survey_year, Gender, Weight, Sports, Clubs, Lessons,
         After_school_program, Any_extracurricular) %>%
  pivot_longer(
    cols = c(Clubs, Lessons, Sports, After_school_program, Any_extracurricular),
    names_to = "Activity",
    values_to = "Participation"
  ) %>%
  mutate(
    Survey_year = recode(as.character(Survey_year),
                         "2021" = "2020",
                         "2024" = "2023"),
    Activity = case_when(
      Activity == "Clubs" ~ "Clubs or Organizations",
      Activity == "Lessons" ~ "After-School Lessons",
      Activity == "Sports" ~ "Sports Teams",
      Activity == "After_school_program" ~ "Before/After-School Programs",
      Activity == "Any_extracurricular" ~ "Any Extracurricular"
    ),
    Activity = factor(
      Activity,
      levels = c("Clubs or Organizations",
                 "After-School Lessons",
                 "Sports Teams",
                 "Before/After-School Programs",
                 "Any Extracurricular")
    )
  ) %>%
  filter(!is.na(Participation), !is.na(Weight)) %>%
  group_by(Survey_year, Gender, Activity) %>%
  summarize(
    Participation_Rate = weighted.mean(Participation, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hover_text = paste0(
      "Year: ", Survey_year, "<br>",
      "Gender: ", Gender, "<br>",
      "Activity: ", Activity, "<br>",
      "Participation: ", percent(Participation_Rate, accuracy = 0.1)
    )
  )

p_gender <- ggplot(
  plot_data_gender,
  aes(x = Activity, y = Participation_Rate, fill = Gender, text = hover_text)
) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(y = Participation_Rate + 0.03,
        label = percent(Participation_Rate, accuracy = 1)),
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 2.6
  ) +
  scale_fill_manual(values = gender_colors) +
  scale_y_continuous(
    labels = percent,
    limits = c(0, 0.8),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Extracurricular Participation by Gender and Year",
    x = "Extracurricular Type",
    y = "Participation Rate",
    fill = "Gender"
  ) +
  facet_wrap(~ Survey_year, ncol = 1) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 8), panel.spacing = unit(1.5, "lines"))

## Question 3

## School Disruption
disrupt_plot_data <- sipp_updated %>%
  filter(!is.na(Any_extracurricular),
         !is.na(Weight),
         !is.na(Repeated_grade),
         !is.na(Suspended_expelled),
         Survey_year %in% c(2021, 2024)) %>%
  mutate(
    DisplayYear = factor(Survey_year,
                         levels = c(2021, 2024),
                         labels = c("2020", "2023")),
    Disruption = if_else(
      Repeated_grade == 1 | Suspended_expelled == 1,
      "Repeated grade and/or suspended/expelled",
      "No repeated grade or suspension/expulsion"
    )
  ) %>%
  group_by(DisplayYear, Disruption) %>%
  summarize(
    extr_rate = weighted.mean(Any_extracurricular, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hover_text = paste0(
      "Year: ", DisplayYear, "<br>",
      "Status: ", Disruption, "<br>",
      "Participation: ", percent(extr_rate, accuracy = 0.1)
    )
  )

p_rep_susp <- ggplot(
  disrupt_plot_data,
  aes(x = Disruption, y = extr_rate, fill = DisplayYear, text = hover_text)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(y = extr_rate + 0.03,
        label = scales::percent(extr_rate, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.8
  ) +
  scale_x_discrete(labels = c(
    "Repeated grade and/or suspended/expelled" =
      "Repeated grade\nand/or suspended/expelled",
    "No repeated grade or suspension/expulsion" =
      "No repeated grade\nor suspension/expulsion"
  )) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = year_colors, name = "Year") +
  labs(
    title = "Extracurricular Participation by School Disruption and Year",
    x = "School Disruption Status",
    y = "% Participating in Extracurriculars"
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

## Homework Engagement
homework_plot_data <- sipp_updated %>%
  filter(!is.na(Any_extracurricular),
         !is.na(Weight),
         !is.na(Does_homework),
         Survey_year %in% c(2021, 2024)) %>%
  mutate(
    DisplayYear = factor(Survey_year,
                         levels = c(2021, 2024),
                         labels = c("2020", "2023")),
    HomeworkFreq = factor(
      Does_homework,
      levels = 1:6,
      labels = c(
        "All of the time",
        "Most of the time",
        "Some of the time",
        "None of the time",
        "N/A",
        "Not enrolled"
      )
    )
  ) %>%
  group_by(DisplayYear, HomeworkFreq) %>%
  summarize(
    extr_rate = weighted.mean(Any_extracurricular, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hover_text = paste0(
      "Year: ", DisplayYear, "<br>",
      "Homework: ", HomeworkFreq, "<br>",
      "Participation: ", percent(extr_rate, accuracy = 0.1)
    )
  )

p_homework <- ggplot(
  homework_plot_data,
  aes(x = HomeworkFreq, y = extr_rate, fill = DisplayYear, text = hover_text)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(y = extr_rate + 0.03,
        label = percent(extr_rate, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.6
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = year_colors, name = "Year") +
  labs(
    title = "Extracurricular Participation by Homework Engagement and Year",
    x = "Homework Engagement",
    y = "% Participating in Extracurriculars"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

## Cares About School
care_plot_data <- sipp_updated %>%
  filter(!is.na(Any_extracurricular),
         !is.na(Weight),
         !is.na(Cares_about_school),
         Survey_year %in% c(2021, 2024)) %>%
  mutate(
    DisplayYear = factor(Survey_year,
                         levels = c(2021, 2024),
                         labels = c("2020", "2023")),
    CaresSchool = factor(
      Cares_about_school,
      levels = 1:6,
      labels = c(
        "All of the time",
        "Most of the time",
        "Some of the time",
        "None of the time",
        "N/A",
        "Not enrolled"
      )
    )
  ) %>%
  group_by(DisplayYear, CaresSchool) %>%
  summarize(
    extr_rate = weighted.mean(Any_extracurricular, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hover_text = paste0(
      "Year: ", DisplayYear, "<br>",
      "Cares about school: ", CaresSchool, "<br>",
      "Participation: ", percent(extr_rate, accuracy = 0.1)
    )
  )

p_cares <- ggplot(
  care_plot_data,
  aes(x = CaresSchool, y = extr_rate, fill = DisplayYear, text = hover_text)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(y = extr_rate + 0.03,
        label = percent(extr_rate, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.6
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = year_colors, name = "Year") +
  labs(
    title = "Extracurricular Participation by Cares About School and Year",
    x = "Cares About School",
    y = "% Participating in Extracurriculars"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

### Region Map
region_summary <- sipp_updated %>%
  mutate(
    Region = case_when(
      Region == 1 ~ "Northeast",
      Region == 2 ~ "Midwest",
      Region == 3 ~ "South",
      Region == 4 ~ "West"
    ),
    extracurricular = ifelse(Any_extracurricular == 1, 1, 0),
    YearPlot = case_when(
      Survey_year == 2021 ~ "2020",
      Survey_year == 2024 ~ "2023"
    )
  ) %>%
  group_by(YearPlot, Region) %>%
  summarize(
    extracurricular_rate = mean(extracurricular, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    hover_text = paste0(
      "Year: ", YearPlot, "<br>",
      "Region: ", Region, "<br>",
      "Participation: ", round(extracurricular_rate, 1), "%"
    )
  )

states <- map_data("state") %>%
  mutate(
    Region = case_when(
      region %in% c("maine", "new hampshire", "vermont", "massachusetts",
                    "rhode island", "connecticut", "new york",
                    "new jersey", "pennsylvania") ~ "Northeast",
      region %in% c("ohio", "indiana", "illinois", "michigan", "wisconsin",
                    "minnesota", "iowa", "missouri", "north dakota",
                    "south dakota", "nebraska", "kansas") ~ "Midwest",
      region %in% c("delaware", "maryland", "district of columbia", "virginia",
                    "west virginia", "north carolina", "south carolina",
                    "georgia", "florida", "kentucky", "tennessee",
                    "mississippi", "alabama", "oklahoma", "texas",
                    "arkansas", "louisiana") ~ "South",
      region %in% c("montana", "idaho", "wyoming", "colorado", "new mexico",
                    "arizona", "utah", "nevada", "washington", "oregon",
                    "california", "alaska", "hawaii") ~ "West"
    )
  )

map_data_region <- states %>%
  mutate(
    state_name = str_to_title(region)  # e.g., "california" -> "California"
  ) %>%
  left_join(region_summary, by = "Region") %>%
  mutate(
    hover_text = paste0(
      "State: ", state_name, "<br>",
      "Region: ", Region, "<br>",
      "Year: ", YearPlot, "<br>",
      "Participation: ", round(extracurricular_rate, 1), "%"
    )
  )

label_positions <- data.frame(
  Region = c("West", "Midwest", "South", "Northeast"),
  long = c(-119, -93, -88, -74),
  lat  = c(40, 42, 33, 42)
)

label_data <- region_summary %>%
  left_join(label_positions, by = "Region") %>%
  mutate(
    label = paste0(Region, "\n", round(extracurricular_rate, 1), "%")
  )

plot_region <- ggplot(
  map_data_region,
  aes(x = long, y = lat, group = group,
      fill = extracurricular_rate, text = hover_text)
) +
  geom_polygon(color = "white") +
  geom_text(
    data = label_data,
    aes(x = long, y = lat, label = label),
    inherit.aes = FALSE,
    size = 3,
    color = "black"
  ) +
  coord_fixed(1.3) +
  facet_wrap(
    ~ YearPlot,
    ncol = 1,
    labeller = as_labeller(c("2020" = "2020", "2023" = "2023"))
  ) +
  scale_fill_gradient(
    low  = "#f0f5fa",
    high = "#8CAFCF",
    name = "Extracurricular\nParticipation\nPercentage (%)"
  ) +
  labs(
    title = "Extracurricular Participation by U.S. Region and Survey Year",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  )

# Export interactive Plotly widgets for the static website.
output_dir <- "assets/interactive"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

widgets <- list(
  parent_education = list(plot = p_parent_edu, height = 540),
  family_type = list(plot = p_family_type, height = 520),
  race = list(plot = p_race, height = 760),
  poverty = list(plot = p_poverty, height = 520),
  age = list(plot = p_age, height = 520),
  gender = list(plot = p_gender, height = 760),
  repeated_suspension = list(plot = p_rep_susp, height = 540),
  homework = list(plot = p_homework, height = 560),
  cares_school = list(plot = p_cares, height = 560),
  region = list(plot = plot_region, height = 820)
)

for (widget_name in names(widgets)) {
  widget <- ggplotly(
    widgets[[widget_name]]$plot,
    tooltip = "text",
    height = widgets[[widget_name]]$height
  )

  saveWidget(
    widget,
    file = file.path(output_dir, paste0(widget_name, ".html")),
    selfcontained = FALSE,
    libdir = "lib"
  )
}
