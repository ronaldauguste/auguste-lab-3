library(carData)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(performance)
library(gt)
library(psych)

#Loading the dataset

dept_data <- attitude |> 
  select(rating, complaints, learning, raises) |> 
  drop_na()

n <- nrow(dept_data)

#Inspections

str(dept_data)
head(dept_data)

#Descriptives
describe(dept_data)

#Distributions
hist(dept_data$rating)
hist(dept_data$complaints)
hist(dept_data$learning)
hist(dept_data$raises)

#Relationships between predictors and outcome
dept_data |> 
  ggplot(aes(x = complaints, y = rating)) +
  geom_jitter() +
  geom_smooth()
#need to review what these variables mean. Complaints and rating have positive relationship??

dept_data |> 
  ggplot(aes(x = learning, y = rating)) +
  geom_jitter() +
  geom_smooth()

dept_data |> 
  ggplot(aes(x = raises, y = rating)) +
  geom_jitter() +
  geom_smooth()

?attitude
#Bird's-eye view: appropriate handling of complaints seems to have the strongest relationship with rating

#Descriptives table
descriptives <- dept_data |>
  summarise(
    across(
      c(rating, complaints, learning, raises),
      list(mean = mean, sd = sd, min = min, max = max)
    )
  )|>
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_sep = "_"
  )

descriptives |>
  gt() |>
  fmt_number(columns = where(is.numeric), decimals = 2)

#Rating Histogram
ggplot(dept_data, aes(x = rating)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 8,
    fill = "#669BBC",
    color = "white",
    alpha = 0.8
  ) +
  geom_density(
    linewidth = 0.9,
    color = "#9A3038"
  ) +
  labs(
    x = "Overall favourable ratings across 30 departments",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)

#Scatterplots
ggplot(dept_data, aes(x = complaints, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A"
  ) +
  labs(
    x = "Favourable handling of employee complaints",
    y = "Overall rating"
  ) +
  theme_minimal(base_size = 12)

ggplot(dept_data, aes(x = raises, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A"
  ) +
  labs(
    x = "Raises based on performance",
    y = "Overall rating"
  ) +
  theme_minimal(base_size = 12)

ggplot(dept_data, aes(x = learning, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A"
  ) +
  labs(
    x = "Opportunity to learn",
    y = "Overall rating"
  ) +
  theme_minimal(base_size = 12)

#Important patterns
#Ratings appear to have a left skew

#Your discussion should consider whether the relationships appear approximately linear, whether
#there are unusual observations, and whether the exploratory results are consistent with your
#expectations.
#From visual inspection of the scatterplots with fitted regression lines, the relationships 
#with overall rating and the individual predictors appear relatively linear.
#In each of the three scatterplots, observations seem randomly dispersed above and below the fitted regression lines. 
#The relationship between overall rating and handling of complaints appears to be 
#the strongest, as observations tend to cluster closely around the line of best fit, whereas the relationships raises and learning show more scatter. 
#There were no particularly unusual observations.
#These exploratory results are consistent with *a priori* expectations as all predictors should have a positive relationship with overall rating.  


#Part 2: Specify competing regression models

#2.1 Develop your predictions
#On an individual level, favourable handling of employee complaints will tend to indicate that the department is considerate of employees' well-being. 
#As this scales up to the departmental level, it will engender a positive corporate culture, reflected in overall workplace ratings. 
#The opposite is also likely true, therefore handling of employee complaints and overall ratings will have a positive relationship—the better
#the department is perceived to handle employee complaints, the better the overall rating. Opportunities for learning may also reflect consideration of 
#employees' well-being, albeit with some distinctiveness from handling of employee complaints. 
#Opportunities for learning is likely to reflect a department's focus on career enhancement and upward mobility for its employees, carrying a more long-term 
#orientation than handling of complaints. For this reason, it may explain additional variation beyond complaint handling. Performance-based pay increases
#are also likely to explain further variation. 
#Performance-based pay structures function fixed-ratio operant conditioning schedules, with clear connections between behaviour and reward. The incentive
#that this provides is likely to have a positive relationship with overall rating that is distinct from handling of employee complaints and opportunities for 
#learning. 

#2.2 Specification of nested models
#We will fit an intercept-only model and three increasingly complex substantive models.

# | Model | Formula | Research rationale |
#  |:--|:--|:--|
#  | M0: Null | `rating ~ 1` | Baseline model |
#  | M1: Complaints | `rating ~ complaints` | Rating varies with complaints |
#  | M2: Complaints + Learning | `rating ~ complaints + learning` | Learning explains additional variation |
#  | M3: Expanded | `rating ~ complaints + learning + raises` | Raises explains additional variation |
  
#  The models are **nested**: each successive model contains all predictors from the preceding model, plus an additional predictor.

#Intercept-only model
mod0 <- lm(rating ~ 1, data = dept_data)

#Complaints model
mod1 <- lm(rating ~ complaints, data = dept_data)

#Complaints + learning model
mod2 <- lm(rating ~ complaints + learning, data = dept_data)

#Complaints + learning + raises model (full model)
mod3 <- lm(rating ~ complaints + learning + raises, data = dept_data)

#2.3 Exploration of fitted models
summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)

#alternative code: lapply(list(mod0, mod1, mod2, mod3), summary)

#In your Quarto report, briefly explain how the interpretation of the complaints coefficient changes between M1, M2, and M3.

#In mod1 the complaints coefficient refers to the estimated difference in overall rating for departments differing by 1 point in complaints,
#unadjusted for any other predictors. 
#In mod2 the complaints coefficient refers to the estimated difference in overall rating for departments differing by 1 point in complaints, 
#when learning is held constant. 
#In mod3 the complaints coefficient refers to the estimated difference in overall rating for departments differing by 1 point in complaints, 
#when learning and raises are held constant. 

#Part 3: Comparing model performance

#3.1 Conducting nested-model F-tests

mod_comps <- anova(mod0, mod1, mod2, mod3)
mod_comps

#nested-model table
mod_comps <- mod_comps |>
  select(Res.Df, Df, F, `Pr(>F)`) |>
  rename(
    Residual.df = Res.Df,
    Nested.Df = Df,
    Nested.F.stat = F,
    Nested.p.value = `Pr(>F)`
  )

#3.2 Extracting model-level performance stats with broom::glance()
glance0 <- glance(mod0)
glance1 <- glance(mod1)
glance2 <- glance(mod2)
glance3 <- glance(mod3)

#combining results into dataframe
mods <- c(
  "M0: Null",
  "M1: Complaints",
  "M2: + Learning",
  "M3: + Raises"
)

glances <- rbind(
  glance0,
  glance1,
  glance2,
  glance3
) |> 
  cbind(Model = mods) |> 
  relocate(Model)

#Calculating RMSEA
comparison_table <- cbind(  #comp table first
  glances,
  mod_comps
)

comparison_table <- comparison_table |>
  mutate(
    Delta.R2 = r.squared - lag(r.squared),
    
    RMSE = c(
      sqrt(mean(residuals(mod0)^2)),
      sqrt(mean(residuals(mod1)^2)),
      sqrt(mean(residuals(mod2)^2)),
      sqrt(mean(residuals(mod3)^2))
    )
  ) |>
  select(
    Model,
    r.squared,
    Delta.R2,
    adj.r.squared,
    RMSE,
    Residual.df,
    Nested.Df,
    Nested.F.stat,
    Nested.p.value
  )

#performance::compare_performance(mod0, mod1....modk, rank = TRUE), provides the correct RMSE instead of only the sigma as in glance()

#3.4 Combined model-comparison table
comparison_table |>
  select(
    Model, r.squared, adj.r.squared, RMSE, Delta.R2,
    Residual.df, Nested.Df, Nested.F.stat, Nested.p.value
  ) |> 
  gt() |>
  cols_label(
    Model = "Model",
    r.squared = "R²",
    adj.r.squared = "Adjusted R²",
    RMSE = "RMSE",
    Delta.R2 = "ΔR²",
    Residual.df = "Residual df",
    Nested.Df = "Nested df",
    Nested.F.stat = "Nested F",
    Nested.p.value = "Nested p"
  ) |>
  tab_spanner(
    label = "Overall performance",
    columns = c(r.squared, adj.r.squared, RMSE)
  ) |>
  tab_spanner(
    label = "Incremental performance",
    columns = c(Delta.R2, Residual.df, Nested.Df, Nested.F.stat, Nested.p.value)
  ) |>
  fmt_number(
    columns = c(
      r.squared,
      Delta.R2,
      adj.r.squared,
      RMSE,
      Nested.F.stat
    ),
    decimals = 3
  ) |>
  fmt(
    columns = Nested.p.value,
    fns = function(x) {
      ifelse(
        is.na(x),
        "—",
        ifelse(x < .001, "< .001", sprintf("%.3f", x))
      )
    }
  ) |>
  sub_missing(
    columns = c(Delta.R2, Nested.Df, Nested.F.stat),
    missing_text = "—"
  )

#Objects for later use
# APA-style formatting helpers
fmt_p  <- function(p) if (p < .001) "< .001" else paste("=", sub("^0", "", sprintf("%.3f", p)))
fmt_r2 <- function(x) sub("^0", "", sprintf("%.3f", x))

# Model-level results for mod1 (from glance1)
mod1_r2     <- fmt_r2(glance1$r.squared)
mod1_r2_pct <- sprintf("%.1f", 100 * glance1$r.squared)
mod1_adjr2  <- fmt_r2(glance1$adj.r.squared)
mod1_F      <- sprintf("%.2f", glance1$statistic)
mod1_df1    <- glance1$df
mod1_df2    <- glance1$df.residual
mod1_p      <- fmt_p(glance1$p.value)
mod1_rmse   <- sprintf("%.2f", comparison_table$RMSE[2])

# Coefficient results for mod1
coefficient_results <- tidy(mod1, conf.int = TRUE, conf.level = 0.95)

mod1_comp <- coefficient_results |> filter(term == "complaints")
mod1_b    <- sprintf("%.2f", mod1_comp$estimate)
mod1_lo   <- sprintf("%.2f", mod1_comp$conf.low)
mod1_hi   <- sprintf("%.2f", mod1_comp$conf.high)
mod1_bp   <- fmt_p(mod1_comp$p.value)

#3.5 Interpreting the comparisons

#Taken together, the model that includes complaints as the only predictor (M1: Complaints) appears to be the most appropriate for modelling overall ratings across departments.
#The following features support this conclusion:
#1. It's R2 is relatively high **(R² = `r mod1_r2`)** and the overall model is statistically significant **F(`r mod1_df1, r mod1_df2`) = `r mod1_F`, p `r mod1_p`**.
#2. Adjusted R2 values for subsequent models yield minimal increases, indicating that they do not materially explain additional variation
#3. Neither of the subsequent models produce statistically significant improvements in fit
#4. The reduction in root mean square error (RMSE) appears substantial from the null (intercept-only) model, showing a marked average reduction 
# in the gap between fitted and observed overall rating values. Reductions in the more complex models are more modest and do not appear to be 
#substantively meaningful. 

#Part 4: Evaluating regression diagnostics

#4.1 Examining assumptions
check_model(mod1)

#4.2 Reporting findings

#On examination of the regression diagnostic plots, all relevant assumptions appear to be reasonably satisfied. N.b. with only one predictor, 
#the non-multicollinearity assumption does not apply. 
#In the linearity plot, the smoothed reference line stays close to zero across the full range of fitted values, which indicates 
#that a straight-line relationship between complaints and rating is appropriate. The homogeneity of variance plot shows no funnel-shaped 
#pattern, so the residual spread is roughly constant across fitted values. All observations fall well within the Cook’s distance contours, 
#so no single case unduly influences the estimates. The posterior predictive check also shows that model-simulated data broadly reproduce 
#the observed distribution of ratings. However, a few minor irregularities are visible. The observed rating distribution has a small shoulder 
#that the model does not capture, and the variance plot shows a slight hump at lower fitted values. The detrended Q-Q plot shows a mild 
#wave pattern in which the most extreme residuals are somewhat smaller than a normal distribution would predict, which suggests slightly light 
#tails. Two observations (e.g., department 21) also have relatively high leverage, but their small residuals mean they have little influence on the fit. 
#These deviations are modest and consistent with sampling variability in a sample of about 30, and light-tailed residuals pose little threat to 
#the validity of OLS standard errors and p-values. They therefore do not meaningfully reduce my confidence in the model’s estimates or inferences.

#Part 5: Select and report your preferred model

#5.1 Select a model

#Based on overall performance, incremental performance and diagnostic findings, the model that includes complaints as the only predictor appears to 
#be the most defensible representation of the relationship between workplace attitude variables and overall ratings across departments. 
#In addition to the rationale stated previously, regression diagnostics also indicate that *M1: Complaints* is appropriate for modelling this relationship.

#5.2 Creating coefficient table

#coefficient_results <- tidy(
#  mod1,
#  conf.int = TRUE,
#  conf.level = 0.95
#)

coefficient_results |>
  select(
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value
  ) |>
  gt() |>
  cols_label(
    term = "Predictor",
    estimate = "Estimate",
    std.error = "SE",
    conf.low = "95% CI lower",
    conf.high = "95% CI upper",
    p.value = "p"
  ) |>
  fmt_number(
    columns = c(
      estimate,
      std.error,
      conf.low,
      conf.high
    ),
    decimals = 3
  ) |>
  fmt(
    columns = p.value,
    fns = function(x) {
      ifelse(
        x < .001,
        "< .001",
        sprintf("%.3f", x)
      )
    }
  )

#5.3 Interpreting coefficients

#Write a paragraph interpreting the regression coefficients in your selected model.
#For each included predictor:
#  1. Describe the direction of the association.
#2. Explain the coefficient in the units of the original variables.
#3. Explain what is being held constant, if the model includes multiple predictors.
#4. Consider the uncertainty reflected in the confidence interval.
#Use inline R code referencing your tidy() results so that the numerical values in your
#written interpretation update automatically when the model is re-estimated.


#The coefficient for complaints is `r mod1_b`. Being the only predictor in the model, it is an unadjusted coefficient. 
#It indicates that there is a positive relationship between favourable handling of complaints and overall ratings. 
#Specifically, the coefficient indicates an estimated `r mod1_b` higher percentage points in overall ratings between 
#departments that differ by 1% in favourable handling of complaints. 
#The 95% confidence interval for this average difference ranged from `r mod1_lo` to `r mod1_hi` percentage points. 
#It excludes zero, so the data are inconsistent with no association,

#5.4. Summarize model performance

comparison_table |>
  slice(2) |> 
  select(
    Model,
    r.squared,
    adj.r.squared,
    RMSE
  ) |>
  gt() |>
  cols_label(
    Model = "Model",
    r.squared = "R²",
    adj.r.squared = "Adjusted R²",
    RMSE = "RMSE"
  ) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 3
  )

#The R² indicates that the selected model (M1: Complaints) explains approximately `r mod1_r2_pct`% of the variation in overall ratings across 
#departments within the sample. The adjusted R² is `r mod1_adjr2`, and the RMSE is `r mod1_rmse` percentage points. The RMSE describes 
#the typical magnitude of the model’s fitted residuals on a root-mean-square scale.
#It should not be interpreted as an estimate of out-of-sample predictive accuracy.

#Part 6: Discuss your findings and limitations

#Interpreting complaints
#A 1-percentage-point higher rating of a department’s complaint handling is associated with an estimated `r mod1_b`-percentage-point higher overall rating, 
#95% CI [`r mod1_lo`, `r mod1_hi`], p `r mod1_bp`.

#This analysis illustrates how overall ratings can be investigated using several competing regression models.

#The model-comparison process allows us to evaluate whether additional predictors explain meaningful variation beyond simpler models.

#The final model provides estimates of the associations between handling of complaints and overall ratings.

#However, several limitations should be recognized:
  
#The observations describe department averages rather than individual people.
#The data are self-report.
#Regression coefficients represent associations rather than causal effects.
#In-sample model performance does not establish out-of-sample predictive accuracy.

