library(tidyverse)
library(ggplot2)
library(sf)
library(googledrive)
library(here)

selectivity <- read_csv(here("data", "Dataset_selectivity_jg_0617.csv"))



# Figure 1. ######

selectivity_mean <- selectivity %>%
  dplyr::filter(variable != "total_peat") %>%
  dplyr::filter(!is.na(class.bin)) %>%
  dplyr::filter(!is.na(canopy.bin)) %>%
  dplyr::group_by(class.bin, canopy.bin) %>%
  dplyr::summarise(
    mean   = mean(jacobs, na.rm=TRUE),
    median = median(jacobs, na.rm=TRUE),
    p10    = quantile(jacobs, 0.10, na.rm=TRUE),
    p90    = quantile(jacobs, 0.90, na.rm=TRUE),
    n      = n(),
    .groups = "drop"
  )


canopy_cols <- c("open"     = "yellow1",
                 "treed"    = "orange",
                 "forested" = "red",
                 "total"    = "darkred")

fig_sel_mean_dot <- ggplot(selectivity_mean,
       aes(x    = reorder(class.bin, median),
           y    = median,
           fill = canopy.bin)) +
  geom_hline(yintercept = 0, lty=2, alpha=0.5) +
  # black error bar underneath acts as an outline for the coloured one
  geom_errorbar(aes(ymin = p10,
                    ymax = p90),
                color     = "black",
                linewidth = 1.2,
                width     = 0.2,
                position  = position_dodge(width=0.5),
                show.legend = FALSE) +
  geom_errorbar(aes(ymin  = p10,
                    ymax  = p90,
                    color = canopy.bin),
                linewidth = 1,
                width     = 0.2,
                position  = position_dodge(width=0.5)) +
  geom_point(shape    = 21,
             color    = "black",
             stroke   = 0.2,
             size     = 4,
             position = position_dodge(width=0.5)) +
  geom_text(aes(label = paste0("n=", n),
                y     = p90 + 0.05,   # fixed offset above error bar
                color = "black"),
            size     = 2.5,
            position = position_dodge(width=0.5),
            show.legend = FALSE) +    # this removes the "a" from legend
  coord_flip() +
  scale_color_manual(values = canopy_cols) +
  scale_fill_manual(values = canopy_cols) +
  labs(x     = "",
       y     = expression("Median Jacobs' " ~ italic(D)),
       color = "Canopy cover",
       fill  = "Canopy cover") +
  theme_classic() +
  theme(legend.position = "bottom")

print(fig_sel_mean_dot)
ggsave(here("plots", "main", "fig2_selectivity_median_dot.png"), fig_sel_mean_dot, width = 12, height = 9, dpi = 300)

# Figure 2. selectivity by bui

# Fix class.bin for total_peat and prepare data
selectivity_bui <- selectivity %>%
  dplyr::filter(!is.na(Quartile_Bins.BUI.mean)) %>%
  dplyr::mutate(
    class.bin = as.character(class.bin),
    class.bin = ifelse(variable == "total_peat",
                       "total peat",
                       class.bin),
    class.bin = factor(class.bin,
                       levels = c("bog", "poor fen", "rich fen",
                                  "ppc", "upland", "total peat"))
  )

# Recalculate count_data with fixed class.bin
count_data <- selectivity_bui %>%
  dplyr::group_by(class.bin, Quartile_Bins.BUI.mean) %>%
  dplyr::summarise(
    count = n(),
    y_pos = median(jacobs, na.rm=TRUE),
    .groups = "drop"
  )

# Plot
ggplot(selectivity_bui,
       aes(x    = Quartile_Bins.BUI.mean,
           y    = jacobs,
           fill = Quartile_Bins.BUI.mean)) +
  geom_hline(yintercept=0, lty=2, alpha=0.5) +
  geom_boxplot(alpha=0.7, outlier.alpha=0.1) +
  geom_text(data  = count_data,
          aes(label = count,
              y     = y_pos),
          vjust       = -0.5,
          size        = 3,
          show.legend = FALSE) +
  scale_fill_manual(values=c("yellow","orange","red","darkred")) +
  facet_wrap(~class.bin, ncol=3) +
  labs(x = "", 
       y     = expression("Jacobs' " ~ italic(D)),
       fill  = "BUI quartile",
       ) +
  theme_bw() +
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        legend.position = "bottom", 
              strip.text      = element_text(size = 14, face = "bold"))

ggsave(here("plots", "main", "fig3_selectivity_bui.png"), width = 12, height = 9, dpi = 300)

####### Figure 3.  BUI class ##
# Fix class.bin and canopy.bin for total_peat and prepare data
# Fix class.bin and canopy.bin and prepare data
selectivity_bui <- selectivity %>%
  dplyr::filter(!is.na(Quartile_Bins.BUI.mean)) %>%
  dplyr::filter(!is.na(class.bin)) %>%
  dplyr::filter(!is.na(canopy.bin)) %>%
  dplyr::filter(variable != "total_peat") %>%
  dplyr::mutate(
    class_canopy = paste(class.bin, canopy.bin, sep="\n"),
    # set explicit order: class groups rows, canopy orders within row
    class_canopy = factor(class_canopy,
                          levels = c(
                            # bog row
                            "bog\nopen",
                            "bog\nforested",
                            "bog\ntreed",
                            "bog\ntotal",
                            # poor fen row
                            "poor fen\nopen",
                            "poor fen\nforested",
                            "poor fen\ntreed",
                            "poor fen\ntotal",
                            # rich fen row
                            "rich fen\nopen",
                            "rich fen\nforested",
                            "rich fen\ntreed",
                            "rich fen\ntotal",
                            # ppc row
                            "ppc\nopen",
                            "ppc\nforested",
                            "ppc\ntreed",
                            "ppc\ntotal",
                            # upland row
                            "upland\nopen",
                            "upland\nforested",
                            "upland\ntreed",
                            "upland\ntotal"
                          ))
  )

# count data
count_data <- selectivity_bui %>%
  dplyr::group_by(class_canopy, Quartile_Bins.BUI.mean) %>%
  dplyr::summarise(
    count = n(),
    y_pos = median(jacobs, na.rm=TRUE),
    .groups = "drop"
  )

# plot
figure_canopy_class_bin <- ggplot(selectivity_bui,
       aes(x    = Quartile_Bins.BUI.mean,
           y    = jacobs,
           fill = Quartile_Bins.BUI.mean)) +
  geom_hline(yintercept=0, lty=2, alpha=0.5) +
  geom_boxplot(alpha=0.7, outlier.alpha=0.1) +
  geom_text(data  = count_data,
            aes(label = count,
                y     = y_pos),
            vjust       = -0.5,
            size        = 2.5,
            show.legend = FALSE) +
  scale_fill_manual(values=c("yellow","orange","red","darkred")) +
  facet_wrap(~class_canopy, ncol=4) +
  labs(x     = "",
       y     = expression("Jacobs' " ~ italic(D)),,
       fill  = "BUI quartile") +
  theme_bw() +
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        legend.position = "bottom")


# Fig 4: combined isi bui ######
selectivity_mean_bui_isi <- selectivity %>%
  dplyr::filter(canopy.bin == "total") %>%
  dplyr::filter(!is.na(Quartile_Bins.BUI.mean)) %>%
  dplyr::filter(!is.na(Quartile_Bins.ISI.mean)) %>%
  # fix class.bin for total_peat
  dplyr::mutate(
    class.bin = as.character(class.bin),
    class.bin = ifelse(variable == "total_peat",
                       "total peat",
                       class.bin),
    class.bin = factor(class.bin,
                       levels = c("bog", "poor fen", "rich fen",
                                  "ppc", "upland", "total peat"))
  ) %>%
  dplyr::filter(!is.na(class.bin)) %>%
  dplyr::group_by(class.bin,
                  Quartile_Bins.BUI.mean,
                  Quartile_Bins.ISI.mean) %>%
  dplyr::summarise(
    mean = mean(jacobs, na.rm=TRUE),
    n    = n(),
    .groups = "drop"
  )

ggplot(selectivity_mean_bui_isi,
       aes(x    = class.bin,
           y    = mean,
           fill = class.bin)) +
  geom_hline(yintercept=0, lty=2, alpha=0.5) +
  geom_bar(stat="identity", alpha=0.9) +
  geom_text(aes(label=n),
            vjust=-0.5,
            size=2.5,
            show.legend=FALSE) +
  scale_fill_brewer(palette="Set1") +
  facet_grid(Quartile_Bins.ISI.mean ~ Quartile_Bins.BUI.mean) +
  labs(x     = "",
       y     =  expression("Jacobs' " ~ italic(D)),
       fill  = "Land cover") +
  theme_bw() +
  theme(axis.text.x    = element_text(angle=90),
        legend.position = "bottom")

library(dplyr)
library(ggplot2)

# 1. Prep the raw data (one row per observation)
selectivity_bui_isi <- selectivity %>%
  dplyr::filter(canopy.bin == "total") %>%
  dplyr::filter(!is.na(Quartile_Bins.BUI.mean)) %>%
  dplyr::filter(!is.na(Quartile_Bins.ISI.mean)) %>%
  dplyr::mutate(
    class.bin = as.character(class.bin),
    class.bin = ifelse(variable == "total_peat", "total peat", class.bin),
    class.bin = factor(class.bin,
                        levels = c("bog", "poor fen", "rich fen",
                                   "ppc", "upland", "total peat"))
  ) %>%
  dplyr::filter(!is.na(class.bin))

# 2. n + median table for labels
n_labels <- selectivity_bui_isi %>%
  dplyr::group_by(class.bin, Quartile_Bins.BUI.mean, Quartile_Bins.ISI.mean) %>%
  dplyr::summarise(
    n      = dplyr::n(),
    median = median(jacobs, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Palette
land_cover_cols <- c(
  "bog"        = "#E69138",
  "poor fen"   = "#3D6E9E",
  "rich fen"   = "#3EA377",
  "ppc"        = "#D291BC",
  "upland"     = "#E8B830",
  "total peat" = "#87C4E8"
)

# 4. Facet strip labeller — adjust the paste0 prefixes if your BUI/ISI
#    columns are already labeled differently
facet_labeller <- labeller(
  Quartile_Bins.BUI.mean = function(x) paste0("BUI ", x),
  Quartile_Bins.ISI.mean = function(x) paste0("ISI ", x)
)

ggplot(selectivity_bui_isi, aes(x = class.bin, y = jacobs, fill = class.bin)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_boxplot(alpha = 0.9, outlier.size = 1, outlier.alpha = 0.4) +
  geom_text(data = n_labels,
            aes(x = class.bin, y = median, label = n),
            inherit.aes = FALSE,
            size = 2.5,
            vjust = -0.6,          # <- pushes label above the median line
            fontface = "plain") +
  scale_fill_manual(values = land_cover_cols) +
  facet_grid(Quartile_Bins.ISI.mean ~ Quartile_Bins.BUI.mean,
             labeller = facet_labeller) +
  labs(x = "", y = "Jacob's index", fill = "Land cover") +
  theme_bw() +
  theme(
    axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey90")
  )

n_labels <- selectivity_bui_isi %>%
  dplyr::group_by(class.bin, Quartile_Bins.BUI.mean, Quartile_Bins.ISI.mean) %>%
  dplyr::summarise(
    n      = dplyr::n(),
    median = median(jacobs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(label_y = median + 0.09)   # fixed offset in data units, above the median line

ggplot(selectivity_bui_isi, aes(x = class.bin, y = jacobs, fill = class.bin)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_boxplot(alpha = 0.9, outlier.size = 1, outlier.alpha = 0.4) +
  geom_text(data = n_labels,
            aes(x = class.bin, y = label_y, label = n),
            inherit.aes = FALSE,
            size = 2.5,
            fontface = "plain") +
  scale_fill_manual(values = land_cover_cols) +
  facet_grid(Quartile_Bins.ISI.mean ~ Quartile_Bins.BUI.mean,
             labeller = facet_labeller) +
  labs(x = "", y = expression("Jacobs' " ~ italic(D)), fill = "Land cover") +
  theme_bw() +
  theme(
    axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey90")
  )

ggsave(here("plots", "main", "fig4_selectivity_bui_isi.png"), width = 10, height = 12, dpi = 300)


# Fig  region

# Prepare data
selectivity_region_bui <- selectivity %>%
  dplyr::filter(canopy.bin == "total") %>%
  dplyr::filter(!is.na(Quartile_Bins.BUI.mean)) %>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::mutate(
    class.bin = as.character(class.bin),
    class.bin = ifelse(variable == "total_peat",
                       "total peat",
                       class.bin),
    class.bin = factor(class.bin,
                       levels = c("bog", "poor fen", "rich fen",
                                  "ppc", "upland", "total peat")),
    # Clean BUI quartile labels for x axis
    Quartile_Bins.BUI.mean = factor(Quartile_Bins.BUI.mean,
                                     levels = c("Q1: 0-67",
                                                "Q2: 67-99",
                                                "Q3: 99-125",
                                                "Q4: 125-235"))
  ) %>%
  dplyr::filter(!is.na(class.bin))

# Count data for sample size labels
count_data_region <- selectivity_region_bui %>%
  dplyr::group_by(class.bin, Quartile_Bins.BUI.mean, region) %>%
  dplyr::summarise(
    count = n(),
    y_pos = median(jacobs, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
fig_region_bui <- ggplot(selectivity_region_bui,
       aes(x    = Quartile_Bins.BUI.mean,
           y    = jacobs,
           fill = region)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_boxplot(alpha        = 0.7,
               outlier.alpha = 0.1,
               position     = position_dodge(width = 0.8)) +
  geom_text(data = count_data_region,
            aes(label = count,
                y     = y_pos,
                group = region),
            position    = position_dodge(width = 0.8),
            vjust       = -0.5,
            size        = 2.5,
            show.legend = FALSE) +
  scale_fill_manual(values = c("East" = "#E8756A",
                                "West" = "#6BAED6")) +
  facet_wrap(~ class.bin, ncol = 3) +
  labs(x    = "BUI quartile",
       y    = expression("Jacobs' " ~ italic(D)),
       fill = "Region") +
  theme_bw() +
  theme(axis.text.x     = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Save
ggsave(filename = here("plots", "main", "fig5_selectivity_region_bui.png"),
       plot     = fig_region_bui,
       width    = 10,
       height   = 7,
       units    = "in",
       dpi      = 300)
