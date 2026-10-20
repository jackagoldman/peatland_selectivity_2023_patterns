library(here)
library(tidyverse)
library(lme4)
library(emmeans)

# read in data
selectivity <- read_csv(here("data", "Dataset_selectivity_jg_0617.csv"))

# set upland as reference category
selectivity <- selectivity |>   mutate(class.bin = relevel(factor(class.bin), ref = "upland"))

# Total peat vs upland only
selectivity_peat_total <- selectivity %>%
  filter(variable %in% c("total_peat", "total_upland"))

# Main analysis dataset
selectivity <- selectivity %>%
  filter(canopy.bin == "total",
         !is.na(class.bin),
         variable != "total_peat")


# total PEat vs upland

model_total <- lmer(jacobs ~ variable + (1 | NFIREID.x),
                    data = selectivity_peat_total)

emm_total <- emmeans(model_total, ~ variable)

contrast(emm_total, "trt.vs.ctrl", ref = "total_upland")


#########baseline model
# Q1: do peatlands differ from uplands in selectivity overall
# Fit baseline model
model_class <- lmer(jacobs ~ class.bin + (1 | NFIREID.x),
                    data = selectivity |> filter(canopy.bin == "total"))

emm_class <- emmeans(model_class, ~ class.bin)

# Contrasts vs upland
contrast(emm_class, "trt.vs.ctrl", ref = "upland")


######
#Q2:does the peatland vs upland selectivity difference depend on canopy cover?
# Read and clean
selectivity_full <- read_csv(here("data", "Dataset_selectivity_jg_0617.csv")) %>%
  filter(!is.na(class.bin),
         variable != "total_peat") %>%
  mutate(class.bin = relevel(factor(class.bin), ref = "upland"))

# Total class level for Q1 Q3 Q4
selectivity_total <- selectivity_full %>%
  filter(canopy.bin == "total")

# Canopy contrast — peatland subclasses vs upland total
selectivity_canopy <- selectivity_full %>%
  filter(
    (class.bin != "upland" & canopy.bin != "total") |
    (class.bin == "upland" & canopy.bin == "total")
  ) %>%
  mutate(
    class_canopy = case_when(
      class.bin == "upland" ~ "upland_total",
      TRUE ~ paste(class.bin, canopy.bin, sep = "_")
    ),
    class_canopy = relevel(factor(class_canopy), ref = "upland_total")
  )

# Model
model_canopy <- lmer(jacobs ~ class_canopy + (1 | NFIREID.x),
                     data = selectivity_canopy)

emm_canopy <- emmeans(model_canopy, ~ class_canopy)
contrast(emm_canopy, "trt.vs.ctrl", ref = "upland_total")


# contrast approach


# Contrasts with confidence intervals
canopy_contrasts <- contrast(emm_canopy, 
                             "trt.vs.ctrl", 
                             ref = "upland_total") %>%
  confint()

print(canopy_contrasts)


library(dplyr)

# Sample sizes per subgroup
sample_sizes <- selectivity_canopy %>%
  count(class_canopy) %>%
  rename(contrast = class_canopy) %>%
  mutate(contrast = paste(contrast, "- upland_total"))

# Join to contrast output
# Get estimates and CIs
canopy_contrasts_ci <- contrast(emm_canopy, 
                                "trt.vs.ctrl", 
                                ref = "upland_total") %>%
  confint() %>%
  as.data.frame()

# Get p-values separately from the summary
canopy_contrasts_p <- contrast(emm_canopy, 
                               "trt.vs.ctrl", 
                               ref = "upland_total") %>%
  summary() %>%
  as.data.frame() %>%
  select(contrast, p.value)

# Join together
canopy_results <- canopy_contrasts_ci %>%
  left_join(canopy_contrasts_p, by = "contrast") %>%
  left_join(sample_sizes, by = "contrast") %>%
  select(contrast, n, estimate, SE, asymp.LCL, asymp.UCL, p.value)

print(canopy_results)

print(canopy_results)

library(gt)
canopy_results <- canopy_results %>%
  mutate(
    class_order = case_when(
      grepl("bog", contrast)      ~ 1,
      grepl("poor fen", contrast) ~ 2,
      grepl("ppc", contrast)      ~ 3,
      grepl("rich fen", contrast) ~ 4
    ),
    canopy_order = case_when(
      grepl("open", contrast)     ~ 1,
      grepl("treed", contrast)    ~ 2,
      grepl("forested", contrast) ~ 3
    )
  ) %>%
  arrange(class_order, canopy_order) %>%
  select(-class_order, -canopy_order)

# Dunnett contrasts vs reference, with CIs, p-values and n of the non-reference group
dunnett_results <- function(emm, ref, sizes) {
  contrast(emm, "trt.vs.ctrl", ref = ref) %>%
    summary(infer = TRUE) %>%
    as.data.frame() %>%
    # small models get df-based CIs; harmonise names with the asymptotic ones
    rename(any_of(c(asymp.LCL = "lower.CL", asymp.UCL = "upper.CL"))) %>%
    left_join(sizes, by = "contrast") %>%
    select(contrast, n, estimate, SE, asymp.LCL, asymp.UCL, p.value)
}

# Panel A: aggregated peatland (M1) and peatland classes (M2) vs upland
total_results <- dunnett_results(
  emm_total, "total_upland",
  selectivity_peat_total %>%
    count(variable) %>%
    mutate(contrast = paste(variable, "- total_upland")) %>%
    select(contrast, n)
)

class_results <- dunnett_results(
  emm_class, "upland",
  selectivity %>%
    count(class.bin) %>%
    mutate(contrast = paste(class.bin, "- upland")) %>%
    select(contrast, n)
) %>%
  mutate(class_order = case_when(
    grepl("bog", contrast)      ~ 1,
    grepl("poor fen", contrast) ~ 2,
    grepl("ppc", contrast)      ~ 3,
    grepl("rich fen", contrast) ~ 4
  )) %>%
  arrange(class_order) %>%
  select(-class_order)

tableS2_results <- bind_rows(
  bind_rows(total_results, class_results) %>%
    mutate(panel = "A. Land cover class vs upland"),
  canopy_results %>%
    mutate(panel = "B. Peatland canopy subclass vs upland")
)

canopy_table <- tableS2_results %>%
  mutate(
    # Clean contrast names
    contrast = gsub(" - upland_total| - total_upland| - upland", "", contrast),
    contrast = ifelse(contrast == "total_peat", "aggregated peatland", contrast),
    contrast = gsub("_", " ", contrast),
    # Round numeric columns
    across(c(estimate, SE, asymp.LCL, asymp.UCL),
           ~round(., 3)),
    p.value = ifelse(p.value < 0.001, "<0.001",
                     round(p.value, 3)),
    # Flag low sample sizes
    flag = ifelse(n < 100, "†", "")
  ) %>%
  gt(groupname_col = "panel")|>
  cols_label(
    contrast   = "Contrast vs uplands",
    n          = "n",
    flag       = "",
    estimate   = "Estimate",
    SE         = "SE",
    asymp.LCL  = "Lower 95% CI",
    asymp.UCL  = "Upper 95% CI",
    p.value    = md("*p*")
  ) %>%
  fmt_number(
    columns  = c(estimate, SE, asymp.LCL, asymp.UCL),
    decimals = 3
  ) %>%
  tab_footnote(
    footnote  = md("Dunnett-adjusted contrasts from linear mixed models with fire as a random intercept (Panel A: M1 aggregated peatland, M2 peatland classes; Panel B: M3). Upland is the reference in all contrasts; n is the number of progression days for the peatland group."),
  ) %>%
  tab_footnote(
    footnote  = md("† n < 100; interpreted descriptively only."),
  )

gtsave(canopy_table,
       filename = here("results", "tables", "supplementary", "tableS2_class_canopy_contrasts.docx"))


#### Q3 does the difference between peatland classes and upland change as drought intensifies?
# Using BUI quartile bins
model_drought <- lmer(jacobs ~ class.bin * Quartile_Bins.BUI.mean + 
                        (1 | NFIREID.x),
                      data = selectivity %>% filter(canopy.bin == "total"))

# Likelihood ratio test
model_drought_main <- lmer(jacobs ~ class.bin + Quartile_Bins.BUI.mean + 
                             (1 | NFIREID.x),
                           data = selectivity %>% filter(canopy.bin == "total"))

anova(model_drought_main, model_drought)

# Contrasts vs upland within each BUI quartile
emm_drought <- emmeans(model_drought, ~ class.bin | Quartile_Bins.BUI.mean)
contrast(emm_drought, "trt.vs.ctrl", ref = "upland")


# Sample sizes per subgroup (class x BUI quartile)
sample_sizes_drought <- selectivity %>%
  filter(canopy.bin == "total", !is.na(Quartile_Bins.BUI.mean)) %>%
  count(class.bin, Quartile_Bins.BUI.mean) %>%
  rename(contrast = class.bin) %>%
  mutate(contrast = paste(contrast, "- upland"))

# Get estimates and CIs
drought_contrasts_ci <- contrast(emm_drought,
                                 "trt.vs.ctrl",
                                 ref = "upland") %>%
  confint() %>%
  as.data.frame()

# Get p-values separately from the summary
drought_contrasts_p <- contrast(emm_drought,
                                "trt.vs.ctrl",
                                ref = "upland") %>%
  summary() %>%
  as.data.frame() %>%
  select(contrast, Quartile_Bins.BUI.mean, p.value)

# Join together
drought_results <- drought_contrasts_ci %>%
  left_join(drought_contrasts_p, by = c("contrast", "Quartile_Bins.BUI.mean")) %>%
  left_join(sample_sizes_drought, by = c("contrast", "Quartile_Bins.BUI.mean")) %>%
  select(contrast, Quartile_Bins.BUI.mean, n, estimate, SE, asymp.LCL, asymp.UCL, p.value)

print(drought_results)

drought_results <- drought_results %>%
  mutate(
    class_order = case_when(
      grepl("bog", contrast)      ~ 1,
      grepl("poor fen", contrast) ~ 2,
      grepl("ppc", contrast)      ~ 3,
      grepl("rich fen", contrast) ~ 4
    )
  ) %>%
  arrange(Quartile_Bins.BUI.mean, class_order) %>%
  select(-class_order)

drought_table <- drought_results %>%
  mutate(
    # Clean contrast names
    contrast = gsub(" - upland", "", contrast),
    # Round numeric columns
    across(c(estimate, SE, asymp.LCL, asymp.UCL),
           ~round(., 3)),
    p.value = ifelse(p.value < 0.001, "<0.001",
                     round(p.value, 3)),
    # Flag low sample sizes
    flag = ifelse(n < 100, "†", "")
  ) %>%
  gt(groupname_col = "Quartile_Bins.BUI.mean") |>
  cols_label(
    contrast   = "Contrast vs uplands",
    n          = "n",
    flag       = "",
    estimate   = "Estimate",
    SE         = "SE",
    asymp.LCL  = "Lower 95% CI",
    asymp.UCL  = "Upper 95% CI",
    p.value    = md("*p*")
  ) %>%
  fmt_number(
    columns  = c(estimate, SE, asymp.LCL, asymp.UCL),
    decimals = 3
  ) %>%
  tab_footnote(
    footnote  = md("† n < 100; *p*-values should be interpreted with caution due to limited statistical power."),
  )

gtsave(drought_table,
       filename = here("results", "tables", "supplementary", "tableS3_drought_contrasts.docx"))


##
model_wind <- lmer(jacobs ~ class.bin * Quartile_Bins.BUI.mean * 
                     Quartile_Bins.ISI.mean + (1 | NFIREID.x),
                   data = selectivity %>% filter(canopy.bin == "total"))

# Build up from simpler models
model_wind_2way <- lmer(jacobs ~ class.bin * Quartile_Bins.BUI.mean + 
                          class.bin * Quartile_Bins.ISI.mean + 
                          (1 | NFIREID.x),
                        data = selectivity %>% filter(canopy.bin == "total"))

anova(model_wind_2way, model_wind)

# Contrasts vs upland within BUI x ISI combinations
emm_wind <- emmeans(model_wind, 
                    ~ class.bin | Quartile_Bins.BUI.mean * Quartile_Bins.ISI.mean)
contrast(emm_wind, "trt.vs.ctrl", ref = "upland")


# Sample sizes per subgroup (class x BUI quartile x ISI quartile)
sample_sizes_wind <- selectivity %>%
  filter(canopy.bin == "total", !is.na(Quartile_Bins.BUI.mean), !is.na(Quartile_Bins.ISI.mean)) %>%
  count(class.bin, Quartile_Bins.BUI.mean, Quartile_Bins.ISI.mean) %>%
  rename(contrast = class.bin) %>%
  mutate(contrast = paste(contrast, "- upland"))

# Get estimates and CIs
wind_contrasts_ci <- contrast(emm_wind,
                              "trt.vs.ctrl",
                              ref = "upland") %>%
  confint() %>%
  as.data.frame()

# Get p-values separately from the summary
wind_contrasts_p <- contrast(emm_wind,
                             "trt.vs.ctrl",
                             ref = "upland") %>%
  summary() %>%
  as.data.frame() %>%
  select(contrast, Quartile_Bins.BUI.mean, Quartile_Bins.ISI.mean, p.value)

# Join together
wind_results <- wind_contrasts_ci %>%
  left_join(wind_contrasts_p, by = c("contrast", "Quartile_Bins.BUI.mean", "Quartile_Bins.ISI.mean")) %>%
  left_join(sample_sizes_wind, by = c("contrast", "Quartile_Bins.BUI.mean", "Quartile_Bins.ISI.mean")) %>%
  select(contrast, Quartile_Bins.BUI.mean, Quartile_Bins.ISI.mean, n, estimate, SE, asymp.LCL, asymp.UCL, p.value)

print(wind_results)

wind_results <- wind_results %>%
  mutate(
    class_order = case_when(
      grepl("bog", contrast)      ~ 1,
      grepl("poor fen", contrast) ~ 2,
      grepl("ppc", contrast)      ~ 3,
      grepl("rich fen", contrast) ~ 4
    ),
    bui_isi_group = paste(Quartile_Bins.BUI.mean, Quartile_Bins.ISI.mean, sep = " / ")
  ) %>%
  arrange(Quartile_Bins.BUI.mean, Quartile_Bins.ISI.mean, class_order) %>%
  select(-class_order)

wind_table <- wind_results %>%
  mutate(
    # Clean contrast names
    contrast = gsub(" - upland", "", contrast),
    # Round numeric columns
    across(c(estimate, SE, asymp.LCL, asymp.UCL),
           ~round(., 3)),
    p.value = ifelse(p.value < 0.001, "<0.001",
                     round(p.value, 3)),
    # Flag low sample sizes
    flag = ifelse(n < 100, "†", "")
  ) %>%
  select(-Quartile_Bins.BUI.mean, -Quartile_Bins.ISI.mean) %>%
  gt(groupname_col = "bui_isi_group") |>
  cols_label(
    contrast   = "Contrast vs uplands",
    n          = "n",
    flag       = "",
    estimate   = "Estimate",
    SE         = "SE",
    asymp.LCL  = "Lower 95% CI",
    asymp.UCL  = "Upper 95% CI",
    p.value    = md("*p*")
  ) %>%
  fmt_number(
    columns  = c(estimate, SE, asymp.LCL, asymp.UCL),
    decimals = 3
  ) %>%
  tab_footnote(
    footnote  = md("† n < 100; *p*-values should be interpreted with caution due to limited statistical power."),
  )

gtsave(wind_table,
       filename = here("results", "tables", "supplementary", "tableS4_wind_contrasts.docx"))



# Residual check on the class model (M2)
resid_df <- data.frame(
  residuals = residuals(model_class),
  fitted    = fitted(model_class)
)
figs1 <- ggplot(resid_df, aes(x = residuals)) +
  geom_histogram(bins = 50, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  labs(
       x = "Residuals", y = "Count") +
  theme_bw()


ggsave(filename = here("plots", "supplementary", "figS1_basemod_resid_hist.png"),
       plot     = figs1,
       width    = 6,
       height   = 4,
       units    = "in",
       dpi      = 300)
# Tukey-adjusted pairwise comparisons
emm <- emmeans(model_class, ~ class.bin)
pairs(emm, adjust = "tukey")


# SUPP models
# Table S1: all fitted models with fit statistics and likelihood ratio tests

# One row of fit statistics per model; AIC/logLik from ML refits so they
# are comparable with the likelihood ratio tests
model_row <- function(mod, id, question, fixed) {
  vc     <- as.data.frame(VarCorr(mod))
  mod_ml <- update(mod, REML = FALSE)
  tibble(
    question  = question,
    model     = id,
    fixed     = fixed,
    random    = "(1 | fire)",
    n_obs     = nobs(mod),
    n_fires   = ngrps(mod)[["NFIREID.x"]],
    var_fire  = vc$vcov[vc$grp == "NFIREID.x"],
    var_resid = vc$vcov[vc$grp == "Residual"],
    AIC       = AIC(mod_ml),
    logLik    = as.numeric(logLik(mod_ml))
  )
}

# LRT statistics for the full model in each nested pair
lrt_row <- function(null_mod, full_mod, id, null_id) {
  lrt <- anova(null_mod, full_mod)
  tibble(
    model      = id,
    comparison = paste("vs", null_id),
    chisq      = lrt$Chisq[2],
    df         = lrt$Df[2],
    p_lrt      = lrt$`Pr(>Chisq)`[2]
  )
}

q_class   <- "Peatland vs upland"
q_canopy  <- "Canopy structure"
q_drought <- "Drought (BUI)"
q_wind    <- "Drought × wind (BUI × ISI)"

model_summary <- bind_rows(
  model_row(model_total,        "M1",  q_class,   "land cover (aggregated peatland, upland)"),
  model_row(model_class,        "M2",  q_class,   "land cover class"),
  model_row(model_canopy,       "M3",  q_canopy,  "peatland class × canopy subclass (upland reference)"),
  model_row(model_drought_main, "M4a", q_drought, "class + BUI"),
  model_row(model_drought,      "M4b", q_drought, "class × BUI"),
  model_row(model_wind_2way,    "M5a", q_wind,    "class × BUI + class × ISI"),
  model_row(model_wind,         "M5b", q_wind,    "class × BUI × ISI")
) %>%
  left_join(bind_rows(
    lrt_row(model_drought_main, model_drought, "M4b", "M4a"),
    lrt_row(model_wind_2way,    model_wind,    "M5b", "M5a")
  ), by = "model")

print(model_summary)

model_table <- model_summary %>%
  mutate(p_lrt = ifelse(p_lrt < 0.001, "<0.001", as.character(round(p_lrt, 3)))) %>%
  gt(groupname_col = "question") |>
  cols_label(
    model      = "Model",
    fixed      = "Fixed effects",
    random     = "Random effects",
    n_obs      = "n",
    n_fires    = "Fires",
    var_fire   = "σ² fire",
    var_resid  = "σ² residual",
    AIC        = "AIC",
    logLik     = "logLik",
    comparison = "LRT",
    chisq      = md("χ²"),
    df         = "df",
    p_lrt      = md("*p*")
  ) %>%
  fmt_number(columns = c(var_fire, var_resid), decimals = 4) %>%
  fmt_number(columns = c(AIC, logLik, chisq), decimals = 1) %>%
  fmt_integer(columns = c(n_obs, n_fires, df)) %>%
  sub_missing(missing_text = "—") %>%
  tab_footnote(
    footnote = md("All models are linear mixed models of Jacobs' *D* fitted with lme4, with fire as a random intercept. BUI and ISI are quartile bins. Variance components are from REML fits; AIC and logLik are from maximum likelihood refits, as used in the likelihood ratio tests (LRT)."),
  )

gtsave(model_table,
       filename = here("results", "tables", "supplementary", "tableS1_models_lrt.docx"))
