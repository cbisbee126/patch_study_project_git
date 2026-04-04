# README --- Patch Engagement Analysis

## Overview

This project looks at how different types of patch content affect player
engagement in free-to-play games.

We treat each patch as an **event** and measure how player activity
behaves: - on the patch day - immediately after (vs. the day before) -
over the following week

We then relate that to what the patch actually contains.

------------------------------------------------------------------------

## Data

### 1. Patch Data

`data_processed/patch_levers_with_controls.csv`

Contains: - patch dates - content variables: - `rel_competitive` -
`rel_cosmetic` - `rel_seasonal` - `rel_difficulty` - controls: -
`new_season_patch` - `days_since_release` - `log_total_chars` -
`patch_number` - `game`

------------------------------------------------------------------------

### 2. Steam Data

`data_raw/*_steam_data.csv`

One file per game: - Apex Legends - Marvel Rivals - Overwatch 2

Raw structure: - `DateTime` (10-min or hourly near present, daily
historically) - `Players` - `Average Players`

------------------------------------------------------------------------

## Data Construction

### Step 1: Collapse Steam data to daily

We create one observation per **game-day**:

-   `avg_players` = mean of **Average Players** for that day\
-   `peak_players` = max of `Players` that day

------------------------------------------------------------------------

### Step 2: Create lag

-   `lag_avg_players` = previous day's average players

------------------------------------------------------------------------

### Step 3: Merge with patches

We merge on: - `game` - `event_date`

Each row becomes one **patch-day observation**

------------------------------------------------------------------------

### Step 4: Outcome variables

#### Engagement level

-   `log_avg_players`

#### Immediate lift

-   `log_lift = log(avg_today) - log(avg_yesterday)`

#### 7-day retention

-   `avg_next_7` = mean of players over days +1 to +7\
-   `log_retention_7d = log(avg_next_7) - log(avg_today)`

------------------------------------------------------------------------

### Step 5: Final sample

We drop rows where: - `avg_players` is missing - `lag_avg_players` is
missing

------------------------------------------------------------------------
### Step 6: Calculating Developer Emphasis of levers
We compute how developers allocate attention across patch types by averaging the relative share of each lever:

Competitive
Cosmetic
Seasonal
Difficulty

This is done:

overall across all patches
separately by game

This produces developer-side emphasis weights used for comparison with consumer preferences.

### Step 7: Visualizing Consumer vs Developer Comparison

Manual input of averages from LDA Analysis and Step 6. Change if necessary.

Provides a bar chart across the four levers:
Competitive progression
Cosmetics & identity
Seasonal systems
Difficulty & balance

This highlights misalignment between what players value and what developers emphasize, providing key managerial insight.

### Step 8: Try an Assortment of Models
We estimate multiple models to ensure robustness:

1. OLS (Baseline)
Predicts engagement using all levers and controls
2. Mixed-Effects Model (HLM)
Accounts for differences across games using random intercepts
Optional random slopes allow lever effects to vary by game
3. Fixed Effects Panel Model
Controls for all time-invariant differences across games
Uses within-game variation over time
4. Fixed Effects with Clustered Standard Errors
Adjusts inference for time dependence and heteroskedasticity
Provides more reliable significance testing

All models include:

lagged engagement
patch content levers
controls (season, time since release, patch size)

Final results are saved for comparison and interpretation.

## Models Tried in Step 5 for different outcomes

### Model 1 --- Engagement Level

Predicts patch-day engagement.

### Model 2 --- Immediate Lift

Predicts change vs the prior day.

### Model 3 --- Retention

Predicts sustained engagement after the patch.

------------------------------------------------------------------------

## Key Design Choices

-   Unit of analysis = patch day
-   Baseline = previous day
-   Retention = average of next 7 days
-   Logs used for stability and % interpretation

------------------------------------------------------------------------

## Final Output

-   `data_processed/final_patch_dataset.csv`
-   `results/final_model_results.csv`
-   `results/final_model_fit_summary.csv`

------------------------------------------------------------------------

## Summary

We model how patch content affects player engagement by looking at what
happens before, during, and after each patch release.
