
# library(devtools)
# devtools::install_github('NHSEngland/ESA_Avoidable_ED_Attendances')

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(ESAAvoidableAtt)
library(janitor)
library(tidyr)
library(forcats)
library(stringr)

# rm(list=ls())
# source("C:/Users/euan.ives/Desktop/R Stuff/scripts/load_libraries.R", local = TRUE)

# Set PC Directory 
my_dir <- "C:/Users/euan.ives/Desktop/R Stuff/"

# Week ending function (Sunday) Weekly Reporting
closest_future_sunday <- function(date) {
  my_date <- ymd(date)
  my_day <- wday(my_date)
  shift <- ifelse(my_day <= 1, 0, 7)
  return(my_date + days(1 - my_day + shift))
}

# Last Sunday - Full Week Check
last_sunday <- function(date) {
  my_date <- ymd(date)
  my_day <- wday(my_date)
  shift <- ifelse(my_day > 1, 0, -7)
  return(my_date + days(1 - my_day + shift))
}

# Lkp
today <- Sys.Date()+3
last_sunday <- today -(as.POSIXlt(today)$wday)
lkp_box_weeks <- c(last_sunday, last_sunday - 7)
current_wday <- wday(today) # Sunday = 1, Wednesday = 4
shift <- ifelse(current_wday >= 4, 0, -7)
target_wednesday <- today + days(4 - current_wday + shift)


# Logs
my_run_log_lkp <- readRDS(paste0(my_dir,"data/logs/uec_event_df_sql.rds"))

# Check Logs For any missing Data Runs 
filtered_df <- my_run_log_lkp |> 
  filter(Date <= Sys.Date())

# Checker
lkp_log_check <- nrow(filtered_df) == sum(filtered_df$Value, na.rm = TRUE)

# Check if log file up to date
if(lkp_log_check) {
  run_read_rds <- TRUE
} else {
  run_read_rds <- FALSE
  source("C:/Users/euan.ives/Desktop/R Stuff/scripts/personal_credentials.R", local = TRUE)
}

# -------------------------------------------------------------------------------------------------------
# Log Checker - Run SQL || RDS
if(run_read_rds){
  cat(crayon::green("✔ RDS File Used !"))
  
  df_spot <- readRDS(paste0(my_dir,"data/rds/Spot_Week_UEC.rds"))
  summary_df <- readRDS(paste0(my_dir,"data/rds/Actual_UEC_Attendances.rds"))
  df_clean <- readRDS(paste0(my_dir,"data/rds/Actual_UEC_Attendances_Events.rds"))
  summary_df_avoidable <- readRDS(paste0(my_dir,"data/rds/Avoidable_UEC_Discharge.rds"))
  df_avoidable_event <- readRDS(paste0(my_dir,"data/rds/Avoidable_UEC_Discharge_Events.rds"))
  
} else {
  cat(crayon::green("✔ SQL Running !"))
  
  
df_sql <- DBI::dbGetQuery(conn = con_udal, statement = "

SELECT
    EC.Arrival_Date,
    MONTH(EC.Arrival_Date) AS [Month],
    YEAR(EC.Arrival_Date) AS [Year],
    EC.Der_Activity_Month,
    CASE 
        WHEN EC.Provider_Code = 'RA7' THEN 'RVJ'
        ELSE EC.Provider_Code
    END AS Provider_Code,
    CASE 
        WHEN P.Site_Name = 'BFT SOUTHMEAD SOUTHMEAD HOSPITAL' THEN 'SOUTHMEAD HOSPITAL'
        ELSE P.Site_Name
    END AS [Organisation_Name],
    ICB.STP_Code,
    S.STP AS STP,
    EC.Der_EC_Duration,
    EC.Der_EC_Investigation_All,
    EC.Der_EC_Treatment_All,
    EC.EC_Department_Type,
    EC.EC_Discharge_Status_SNOMED_CT,
    CASE WHEN EC.Discharge_Destination_SNOMED_CT IN ('306689006','306691003','306694006','306705005','50861005') THEN 1 
         ELSE 0 
    END AS DISCHARGED,
    EC.EC_Chief_Complaint_SNOMED_CT,
    TOS.ECDS_Group1,
    EC.EC_Attendance_Source_SNOMED_CT,
    EC.EC_Arrival_Mode_SNOMED_CT,
    EC.EC_AttendanceCategory,
    CASE 
        WHEN EC.Age_At_Arrival BETWEEN 0 AND 18 THEN '0-18'
        WHEN EC.Age_At_Arrival BETWEEN 19 AND 64 THEN '19-64'
        WHEN EC.Age_At_Arrival >= 65 THEN '65+'
        ELSE 'Unknown'
    END AS Age_Band
    
FROM MESH_ECDS.EC_Core AS EC

LEFT JOIN Reporting_UKHD_ODS.Provider_Site AS P
    ON EC.Site_Code_Of_Treatment = P.Site_Code
    
LEFT JOIN (SELECT DISTINCT Organisation_Code, STP_Code, Organisation_Name, STP_Name
                  FROM Reporting_UKHD_ODS.Provider_Hierarchies
                  WHERE Region_Name = 'South West'
                  AND ODS_Organisation_Type like '%NHS%') AS ICB
    ON EC.Provider_Code = ICB.Organisation_Code
    
LEFT JOIN Internal_Reference.Provider_DeliveryBoard AS S
    ON EC.Provider_Code = S.Code
LEFT JOIN (SELECT DISTINCT Snomed_Code, ECDS_Group1 FROM UKHD_ECDS_TOS.Code_Sets 
                  WHERE Sheet_Name = '13.4 CHIEF COMPLAINT' 
                  AND ECDS_Group1 <> 'Code deprecated') AS TOS
    ON EC.EC_Chief_Complaint_SNOMED_CT = TOS.Snomed_Code
    
WHERE EC.Arrival_Date >= DATEADD(MONTH, -12, GETDATE())
AND EC.EC_Department_Type = '01'
AND (EC.EC_Discharge_Status_SNOMED_CT NOT IN ('1077031000000103', '1077781000000101', '63238001')
        OR EC.EC_Discharge_Status_SNOMED_CT IS NULL)
AND (EC.EC_AttendanceCategory IN ('1', '2', '3') OR EC.EC_AttendanceCategory IS NULL)
AND S.STP IN (
      'Bath And North East Somerset, Swindon And Wiltshire STP',
      'Bristol, North Somerset And South Gloucestershire STP',
      'Cornwall And The Isles Of Scilly Health & Social Care Partnership (STP)',
      'Devon STP',
      'Dorset STP',
      'Gloucestershire STP',
      'Somerset STP'
  )
  AND EC.Der_EC_Duration < 86400
  AND EC.Der_Dupe_Flag = 0

") |>
  clean_names("upper_camel")


# -------------------------------------------------------------------------------------------------------
# 
# # Only Need To Run Once a year ##
# 
# # ONS Lookup
# icb_ons_lookup_sw <- tribble(
#   ~AreaCode,  ~AreaName,                                                                        ~StpCode, ~SubICB_Code, ~ICB_Name_Short,
#   "E38000230", "NHS Devon ICB - 15N",                                                             "QJK",    "15N",        "Devon",
#   "E38000222", "NHS Bristol, North Somerset and South Gloucestershire ICB - 15A",                "QUY",    "15A",        "BNSSG",
#   "E38000231", "NHS Bath and North East Somerset, Swindon and Wiltshire ICB - 92A",                "QOX",    "92A",        "BSW",
#   "E38000062", "NHS Gloucestershire ICB - 11M",                                                    "QR1",    "11M",        "Gloucestershire",
#   "E38000150", "NHS Somerset ICB - 11X",                                                         "QSL",    "11X",        "Somerset",
#   "E38000089", "NHS Cornwall and the Isles of Scilly ICB - 11N",                                  "QT6",    "11N",        "Cornwall",
#   "E38000045", "NHS Dorset ICB - 11J",                                                           "QVV",    "11J",        "Dorset"
# )
# 
# # ONS Data by AGe
# df_population_raw <- read_csv(paste0(my_dir, "Data/csv/2022 SNPP SICB pop persons.csv")) |> 
#   clean_names("upper_camel") |> 
#   filter(AgeGroup != "All ages") |> 
#   filter(AreaCode %in% icb_ons_lookup_sw$AreaCode) |> 
#   select(AreaCode, AreaName, AgeGroup, Population = X2025) |> 
#   left_join(icb_ons_lookup_sw |> select(AreaCode, StpCode), by = join_by(AreaCode)) |> 
#   mutate(age_num = case_when(
#     AgeGroup == "90 and over" ~ 90, TRUE ~ as.numeric(AgeGroup)),
#     AgeBand = case_when(
#       age_num >= 0  & age_num <= 18 ~ "0-18",
#       age_num >= 19 & age_num <= 64 ~ "19-64",
#       age_num >= 65                 ~ "65+",
#       TRUE                          ~ "Unknown")) %>%
#   select(-age_num)
# 
# # Total By Ages
# df_ons <- df_population_raw |> 
#   group_by(StpCode, AreaName, AgeBand) |> 
#   summarise(Population = sum(Population))
# 
# # Save
# if(nrow(df_ons) >3){
#   saveRDS(df_ons, paste0(my_dir,"data/rds/UEC_Attendances_ONS_lkp.rds"))
#   # Message
#   cat(crayon::green("✔ UEC_Attendances_ONS_lkp -  Success!"))
# } else {
#   # Message
#   cat(crayon::red("Error: UEC_Attendances_ONS_lkp -  Failed."), "\n")
# }
# 
# -------------------------------------------------------------------------------------------------------

# Message
cat(crayon::green("✔ SQL Success!"))
# cat(crayon::red("Error: Connection failed."), "\n")

# Check for Full Previous Weeks data 
if(max(df_sql$ArrivalDate) >= last_sunday(Sys.Date())){
  
  lkp_max_week <- last_sunday(Sys.Date())
  
  # Message
  cat(crayon::green("✔ Full Weeks Data -  Success!"))
} else {
  # Message
  err_msg <- sprintf(
    "Data Incomplete: Maximum ArrivalDate in SQL (%s) is prior to last Sunday (%s).",
    max(df_sql$ArrivalDate),
    last_sunday(Sys.Date())
  )
  cat(crayon::red(err_msg, "\n"))
  stop(err_msg, call. = FALSE) # Stop Main IF Statement
}
  


# Clean names for Avoidable ED Attendance Package 
df_clean <- df_sql |> 
  mutate(Week = closest_future_sunday(ArrivalDate)) |> 
  filter(Week <= lkp_max_week) |> # Full Weeks only 
  rename(
  Department_Type = EcDepartmentType,
  Discharge_Status = EcDischargeStatusSnomedCt,
  AttendanceCategory = EcAttendanceCategory,
  Arrival_Mode = EcArrivalModeSnomedCt,
  Investigation = DerEcInvestigationAll,
  Treatment = DerEcTreatmentAll
)

# Update 
lkp_box_weeks <- c(max(df_clean$Week), max(df_clean$Week) - 7)

# Save 
if(nrow(df_clean) >1000){
  saveRDS(df_clean, paste0(my_dir,"data/rds/Actual_UEC_Attendances_Events.rds"))
  # Message
  cat(crayon::green("✔ Actual_UEC_Attendances_Events -  Success!"))
} else {
  # Message
  cat(crayon::red("Error: Actual_UEC_Attendances_Events.rds -  Failed."), "\n")
}

# -------------------------------------------------------------------------------------------------------

# ECDS_ETOS_v4.0.8
snomed_lkp <- tibble(
  EcAttendanceSourceSnomedCt = c("1991000124105","1065391000000104","315261000000101","276491000","1082331000000106",
                  "879591000000102","1066431000000102","1066441000000106","835091000000109","835101000000101",
                  "1465211000000108","1079521000000104","1077191000000103","1052681000000105","185363009",
                  "1065401000000101","1077201000000101","1065991000000100","877171000000103","1077761000000105",
                  "1077211000000104","1066011000000104","1066001000000101","185369008","185366001","185368000",
                  "1066021000000105","198261000000104","889801000000100","1066031000000107","1066061000000102",
                  "1066041000000103","1066051000000100"),
  snomed_att_def = c("Referred by self","Referred by carer","Advised to attend accident and emergency department",
                               "Referred by member of Primary Health Care Team","Referred by out of hours service",
                               "Referred by NHS 111 service","Referred by hospital emergency department",
                               "Referred by urgent treatment centre","Referred by hospital outpatient department",
                               "Referred by hospital ward","Referral by UEC ECE (urgent and emergency care extended care episode)",
                               "Referred by private sector physician","Referred by community nurse","Referred by health visitor",
                               "Referred by midwife","Referred by school nurse","Referred by community mental health nurse",
                               "Referred by mental health assessment team","Referred by Social Services","Referred by adult day care centre",
                               "Referred by homeless drop-in centre","Referred by HM Prison Service","Referred by detention centre",
                               "Referred by pharmacist","Referred by dentist","Referred by optician","Referred by advanced care practitioner",
                               "Referred by ambulance service","Referred by police","Referred by Fire and Rescue Service",
                               "Referred by search and rescue service","Referred by Coastguard Rescue Service","Referred by mountain rescue service"))

### Investigations
cols <- seq(0:24)

df_inv <- df_clean |>
  filter(Discharged == 1) |>
  separate_wider_delim(Investigation,  
                       delim = ",", 
                       names = c(paste0('Investigation_',cols)),
                       too_few = 'align_start')


### Treatment
cols2 <- seq(0:24)

df_treat <- df_inv |>
  separate_wider_delim(Treatment,  
                       delim = ",", 
                       names = c(paste0('Treatment_',cols2)),
                       too_few = 'align_start')


## all columns for the look up need to be strings. which they appear to be.
isAvoidable <- calculateAvoidableEDAtt(df_treat, 
                                       "Department_Type", 
                                       "Discharge_Status", 
                                       "AttendanceCategory", 
                                       "Arrival_Mode", 
                                       paste0("Investigation_", 1:24),
                                       paste0("Treatment_", 1:24),
                                       "snomed")
# Merge
df_avoid_join <- cbind(df_treat,isAvoidable)

# Add Snomed Code reasons
df_avoidable_event <- df_avoid_join |> 
  left_join(snomed_lkp, by = join_by(EcAttendanceSourceSnomedCt))


# Save 
if(nrow(df_avoidable_event) >1000){
  saveRDS(df_avoidable_event, paste0(my_dir,"data/rds/Avoidable_UEC_Discharge_Events.rds"))
  # Message
  cat(crayon::green("✔ Avoidable_UEC_Discharge_Events.rds -  Success!"))
} else {
  # Message
  cat(crayon::red("Error: Avoidable_UEC_Discharge_Events.rds -  Failed."), "\n")
}

# -------------------------------------------------------------------------------------------------------

# Spot Week :: Attendance, AgeBand and Chief Complaint 

df_spot_1 <- df_clean |> 
  filter(Week == max(Week, na.rm = TRUE)) |> 
  filter(!is.na(EcdsGroup1), !is.na(AgeBand)) |> 
  group_by(AgeBand) |> 
  mutate(Grp = fct_lump_n(EcdsGroup1, n = 5, w = NULL, other_level = "Other")) |> 
  group_by(Week, AgeBand, Grp) |> 
  summarise(Total_Attendances = n(), .groups = "drop_last") |> 
  mutate(
    Percentage = Total_Attendances / sum(Total_Attendances),
    Rank = min_rank(desc(Total_Attendances))
  ) |> 
  ungroup() |> 
  mutate(DataType = "Attendances")

df_spot_2 <- df_avoidable_event |> 
  filter(isAvoidable == TRUE) |> 
  mutate(Week = closest_future_sunday(ArrivalDate)) |> 
  filter(Week == max(Week, na.rm = TRUE)) |> 
  filter(!is.na(EcdsGroup1), !is.na(AgeBand)) |> 
  group_by(AgeBand) |> 
  mutate(Grp = fct_lump_n(EcdsGroup1, n = 5, w = NULL, other_level = "Other")) |> 
  group_by(Week, AgeBand, Grp) |> 
  summarise(Total_Attendances = n(), .groups = "drop_last") |> 
  mutate(
    Percentage = Total_Attendances / sum(Total_Attendances),
    Rank = min_rank(desc(Total_Attendances))
  ) |> 
  ungroup() |> 
  mutate(DataType = "Avoid_Attendances")

df_spot <- rbind(df_spot_1, df_spot_2)

# Save 
if(nrow(df_spot) >10){
  saveRDS(df_spot, paste0(my_dir,"data/rds/Spot_Week_UEC.rds"))
  # Message
  cat(crayon::green("✔ Spot_Week_UEC -  Success!"))
} else {
  # Message
  cat(crayon::red("Error: Spot_Week_UEC -  Failed."), "\n")
}

# -------------------------------------------------------------------------------------------------------

# Summary Values - ACTUALS (Validation)
summary_df <- df_clean %>%
  group_by(
    Week,
    Month,
    Year,
    DerActivityMonth,
    ProviderCode,
    OrganisationName,
    Stp,
    Department_Type,
    AgeBand
  ) %>%
  summarise(
    `4Hours`     = sum(DerEcDuration >= 0 & DerEcDuration <= 239, na.rm = TRUE),
    `4-12Hours`  = sum(DerEcDuration >= 240 & DerEcDuration <= 719, na.rm = TRUE),
    `12Hours`    = sum(DerEcDuration >= 720, na.rm = TRUE),
    Attendances  = n(),
    .groups = "drop"
  )

# Save 
if(nrow(summary_df) >1000){
  saveRDS(summary_df, paste0(my_dir,"data/rds/Actual_UEC_Attendances.rds"))
  # Message
  cat(crayon::green("✔ Actual_UEC_Attendances.rds -  Success!"))
} else {
  # Message
  cat(crayon::red("Error: Actual_UEC_Attendances.rds -  Failed."), "\n")
}

# Summary Values Avoidable
summary_df_avoidable <- df_avoidable_event %>%
  filter(isAvoidable == TRUE) |> 
  mutate(Week = closest_future_sunday(ArrivalDate)) |> 
  mutate(STP = if_else(nchar(Stp) > 32, str_trunc(Stp, width = 32, side = "right", ellipsis = "..."), Stp)) |> 
  group_by(
    Week,
    Month,
    Year,
    DerActivityMonth,
    ProviderCode,
    OrganisationName,
    Stp,
    Department_Type,
    AgeBand,
    isAvoidable
  ) %>%
  summarise(
    `4Hours`     = sum(DerEcDuration >= 0 & DerEcDuration <= 239, na.rm = TRUE),
    `4-12Hours`  = sum(DerEcDuration >= 240 & DerEcDuration <= 719, na.rm = TRUE),
    `12Hours`    = sum(DerEcDuration >= 720, na.rm = TRUE),
    Attendances  = n(),
    .groups = "drop"
  )

# Save 
if(nrow(summary_df_avoidable) >1000){
  saveRDS(summary_df_avoidable, paste0(my_dir,"data/rds/Avoidable_UEC_Discharge.rds"))
  # Message
  cat(crayon::green("✔ Avoidable_UEC_Discharge.rds -  Success!"))
} else {
  # Message
  cat(crayon::red("Error: Avoidable_UEC_Discharge.rds -  Failed."), "\n")
}

# -------------------------------------------------------------------------------------------------------

# WoW - Spot Week changes - ValueBoxes 

a <- summary_df |> 
  filter(Week %in% lkp_box_weeks) |> 
  group_by(Week) |> 
  summarise(Total_Attendances = sum(Attendances, na.rm = TRUE), .groups = "drop") |> 
  arrange(desc(Week)) 

b <- summary_df_avoidable |> 
  filter(Week %in% lkp_box_weeks) |> 
  group_by(Week) |> 
  summarise(Avoidable_Attendances = sum(Attendances, na.rm = TRUE), .groups = "drop")|> 
  arrange(desc(Week)) 

c <- summary_df %>%
  filter(Week %in% lkp_box_weeks) |> 
  mutate(STP = if_else(nchar(Stp) > 32, str_trunc(Stp, width = 32, side = "right", ellipsis = "..."), Stp)) |> 
  group_by(Week) %>%
  summarise(
    pct_4hrs = sum(`4Hours`, na.rm = TRUE) / sum(Attendances, na.rm = TRUE) * 100,
    pct_4_12hrs = sum(`4-12Hours`, na.rm = TRUE) / sum(Attendances, na.rm = TRUE) * 100,
    pct_12hrs = sum(`12Hours`, na.rm = TRUE) / sum(Attendances, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |> 
  arrange(desc(Week)) 

d_sorted <- a |> 
  left_join(b, by = join_by(Week)) |> 
  left_join(c , by = join_by(Week)) |> 
  arrange(desc(Week))


current_row <- d_sorted %>% slice(1)
prev_row    <- d_sorted %>% slice(2)

# Map Structure
metric_name <- c("Total Attendances", "Avoidable Attendances", "A&E 4-Hour Target", "4-12 Hour Waits", "12+ Hour Waits")
current_val <- c(current_row$Total_Attendances, current_row$Avoidable_Attendances, current_row$pct_4hrs, current_row$pct_4_12hrs, current_row$pct_12hrs)
target_val <- c(current_row$Total_Attendances, current_row$Avoidable_Attendances, 76.0, current_row$pct_4_12hrs, current_row$pct_12hrs)
prev_val <- c(prev_row$Total_Attendances, prev_row$Avoidable_Attendances, prev_row$pct_4hrs, prev_row$pct_4_12hrs, prev_row$pct_12hrs)
lower_is_better <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
format_type <- c("number", "number", "percent", "percent", "percent")

# Output 
metrics_data <- data.frame(metric_name, current_val, target_val, prev_val, lower_is_better, format_type) |> 
  mutate(
    across(
      c(current_val, target_val, prev_val),
      ~ case_when(
        metric_name == "12+ Hour Waits" ~ round(.x, 2),
        format_type == "percent"        ~ round(.x, 1),
        TRUE                            ~ round(.x, 0)
      )
    )
  )

# Save 
if(nrow(metrics_data) >3){
  saveRDS(metrics_data, paste0(my_dir,"data/rds/UEC_summary_boxes.rds"))
  # Message
  cat(crayon::green("✔ UEC_summary_boxes -  Success!"))
} else {
  # Message
  cat(crayon::red("Error: UEC_summary_boxes -  Failed."), "\n")
}

# -------------------------------------------------------------------------------------------------------

# Update Run  Log
my_run_log <- my_run_log_lkp |> 
  mutate(Value = if_else(Date <= Sys.Date(), 1, Value))

# Logs
saveRDS(my_run_log, paste0(my_dir,"data/logs/uec_event_df_sql.rds"))
} # End SQL || RDS 

# -------------------------------------------------------------------------------------------------------
# [ END ]
# -------------------------------------------------------------------------------------------------------

# QA Section
df_clean |> 
  filter(ArrivalDate >= sort(unique(df_clean$ArrivalDate), decreasing = TRUE)[7]) |> 
  group_by(ArrivalDate, OrganisationName) |> 
  summarise(vol = n(), .groups = "drop") |> 
  arrange(desc(ArrivalDate)) |> 
  ggplot(aes(x = OrganisationName, y = vol, fill = fct_rev(factor(ArrivalDate)))) +
  geom_col(color = "white", linewidth = 0.3) +
  scale_fill_viridis_d(option = "mako", direction = -1, guide = guide_legend(reverse = FALSE)) +
  coord_flip() +
  labs(x = "Organisation", y = "Volume", title = "QA Checks :: Arrival Date Volumes (Last 7 Days)", fill = "Week") +
  theme_minimal()

# -------------------------------------------------------------------------------------------------------

# # 5. Build Interactive Plotly Object
#
# p <- plot_ly(
#   data = df_summary_rate,
#   x = ~Week,
#   y = ~Rate_per_100k,
#   color = ~ICB,
#   type = "scatter",
#   mode = "lines+markers",
#   marker = list(size = 6),
#   line = list(width = 2)
# ) %>%
#   plotly::layout(
#     title = list(text = "Weekly ED Attendance Rate per 100,000 Population"),
#     yaxis = list(title = list(text = "Rate per 100k")),
#     xaxis = list(
#       title = "Week",
#       range = if (exists("start_date") && exists("end_date")) list(start_date, end_date) else NULL,
#       rangeslider = list(visible = TRUE),
#       rangeselector = list(
#         x = 0.75, 
#         y = 1.0, 
#         xanchor = "right",
#         yanchor = "top",
#         buttons = list(
#           list(count = 14, label = "Last 14 Days", step = "day", stepmode = "backward"),
#           list(count = 30, label = "Last 30 Days", step = "day", stepmode = "backward"),
#           list(count = 90, label = "Last 90 Days", step = "day", stepmode = "backward"),
#           list(step = "all", label = "All")
#         )
#       )
#     ),
#     shapes = list(policy_shape),
#     annotations = list(policy_annotation)
#   )
# 
# p


