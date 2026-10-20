selectivity_clean <- selectivity %>%
  dplyr::mutate(
    class.bin = as.character(class.bin),
    class.bin = ifelse(variable == "total_peat",
                       "total peat",
                       class.bin),
    canopy.bin = as.character(canopy.bin),
    canopy.bin = ifelse(variable == "total_peat",
                        "total",
                        canopy.bin),
    class.bin  = factor(class.bin,
                        levels = c("bog", "poor fen", "rich fen",
                                   "ppc", "upland", "total peat")),
    canopy.bin = factor(canopy.bin,
                        levels = c("open", "treed",
                                   "forested", "total"))
  )



bui_time <- selectivity_clean %>%
  dplyr::filter(canopy.bin == "total") %>%
  dplyr::filter(!is.na(DATE)) %>%
  dplyr::mutate(
    date_real = as.Date(as.character(DATE), format = "%Y%m%d")
  ) %>%
  dplyr::group_by(date_real) %>%
  dplyr::summarise(
    mean_BUI = mean(BUI.mean, na.rm = TRUE),
    n        = n(),
    .groups  = "drop"
  ) %>%
  dplyr::filter(n >= 10)   # same threshold as jacobs_time, for consistency

bui_scale <- 0.01

# Calculate mean Jacob's index per date across all fires
# for each land cover class
jacobs_time <- selectivity_clean %>%
  dplyr::filter(canopy.bin == "total") %>%
  dplyr::filter(!is.na(class.bin)) %>%
  dplyr::filter(!is.na(DATE)) %>%
  dplyr::mutate(
    # convert DATE to actual date format
    date_real = as.Date(as.character(DATE), format="%Y%m%d")
  ) %>%
  dplyr::group_by(date_real, class.bin) %>%
  dplyr::summarise(
    mean_jacobs = mean(jacobs, na.rm=TRUE),
    median_jacobs = median(jacobs, na.rm=TRUE),
    n           = n(),
    mean_BUI    = mean(BUI.mean, na.rm=TRUE),
    .groups     = "drop"
  ) %>%
  # remove dates with very few observations
  dplyr::filter(n >= 10)


##
# Plot 3 - heatmap version
# shows selectivity for each class across all dates
jacobs_heat <- selectivity_clean %>%
  dplyr::filter(canopy.bin == "total") %>%
  dplyr::filter(!is.na(class.bin)) %>%
  dplyr::filter(!is.na(DATE)) %>%
  dplyr::mutate(
    date_real = as.Date(as.character(DATE), format="%Y%m%d"),
    # bin dates by week
    week      = lubridate::floor_date(date_real, "week")
  ) %>%
  dplyr::group_by(week, class.bin) %>%
  dplyr::summarise(
    mean_jacobs = mean(jacobs, na.rm=TRUE),
    n           = n(),
    .groups     = "drop"
  ) %>%
  dplyr::filter(n >= 5)

ggplot(jacobs_heat,
       aes(x    = week,
           y    = class.bin,
           fill = mean_jacobs)) +
  geom_tile() +
  scale_fill_gradient2(
    low      = "darkred",
    mid      = "white",
    high     = "darkblue",
    midpoint = 0,
    name     = "Mean\nJacob's index") +
  scale_x_date(date_breaks="2 weeks",
               date_labels="%b %d") +
  labs(x     = "",
       y     = "",
       title = "Weekly fire selectivity heatmap — 2023 fire season") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))



# Find approximate crossover date where total peat crosses zero
crossover <- jacobs_time %>%
  dplyr::filter(class.bin == "total peat") %>%
  dplyr::arrange(date_real) %>%
  # find where mean_jacobs changes sign
  dplyr::mutate(sign = sign(mean_jacobs)) %>%
  dplyr::filter(sign != lag(sign)) %>%
  dplyr::pull(date_real)

# Add to plot
# custom color palette - more distinguishable than Set1
custom_colors <- c(
  "bog"        = "#D55E00",  # vermillion
  "poor fen"   = "#0072B2",  # blue
  "rich fen"   = "#009E73",  # green
  "ppc"        = "#CC79A7",  # pink
  "upland"     = "#E69F00",  # orange
  "total peat" = "#56B4E9"   # sky blue
)




##### Final plot ALL

time_all <- ggplot() +
  geom_hline(yintercept=0, lty=2, alpha=0.5, color="grey50") +
  geom_smooth(data    = jacobs_time,
              aes(x     = date_real,
                  y     = mean_jacobs,
                  color = class.bin),
              method  = "loess",
              span    = 0.3,
              se      = FALSE,
              size    = 1) +
  geom_line(data  = bui_time,
            aes(x = date_real,
                y = mean_BUI * bui_scale - 1),
            color = "black",
            lty   = 3,
            size  = 0.8) +
  geom_vline(xintercept = as.numeric(as.Date("2023-06-16")),
             lty   = 2,
             color = "black",
             alpha = 0.5,
             size  = 0.5) +
  annotate("text",
           x     = as.Date("2023-06-16"),
           y     = 0.45,
           label = "Early | Late",
           size  = 3,
           hjust = -0.1) +
  scale_color_manual(values = custom_colors) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d") +
  scale_y_continuous(
    name     = expression("Mean Jacobs' " ~ italic(D)),
    sec.axis = sec_axis(
      ~(. + 1) / bui_scale,
      name = "Mean BUI")) +
  labs(x     = "",
       color = "Land cover")+
  theme_bw() +
  theme(axis.text.x        = element_text(angle=45, hjust=1),
            panel.border     = element_rect(colour = "black", fill = NA, linewidth = 1),

          legend.position    = "bottom",
        axis.title.y.right = element_text(color="black"),
        axis.text.y.right  = element_text(color="black"))


# subset

jacobs_time_subset <- jacobs_time %>%
  dplyr::filter(class.bin %in% c("total peat", "upland"))

custom_colors_subset <- custom_colors[c("upland", "total peat")]

time_subset <- ggplot() +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5, color = "grey50") +
  geom_smooth(data    = jacobs_time_subset,
              aes(x     = date_real,
                  y     = mean_jacobs,
                  color = class.bin),
              method  = "loess",
              span    = 0.3,
              se      = FALSE,
              size    = 1) +
  geom_line(data  = bui_time,
            aes(x = date_real,
                y = mean_BUI * bui_scale - 1),
            color = "black",
            lty   = 3,
            size  = 0.8) +
  geom_vline(xintercept = as.numeric(as.Date("2023-06-16")),
             lty   = 2,
             color = "black",
             alpha = 0.5,
             size  = 0.5) +
  annotate("text",
           x     = as.Date("2023-06-16"),
           y     = 0.45,
           label = "Early | Late",
           size  = 5,
           hjust = -0.1) +
  scale_color_manual(values = custom_colors_subset) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d") +
  scale_y_continuous(
    name     = expression("Mean Jacobs' " ~ italic(D)),
    sec.axis = sec_axis(
      ~(. + 1) / bui_scale,
      name = "Mean BUI")) +
  labs(x     = "",
       color = "Land cover") +
  theme_bw() +
  theme(axis.text.x        = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y        = element_text(size = 14),
        axis.title.x       = element_text(size = 16),
        axis.title.y       = element_text(size = 16),
        panel.border       = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position    = "bottom",
        legend.text        = element_text(size = 14),
        legend.title       = element_text(size = 14),
        axis.title.y.right = element_text(color = "black", size = 16),
        axis.text.y.right  = element_text(color = "black", size = 14))



# subset peat only 
jacobs_time_peatland <- jacobs_time %>%
  dplyr::filter(class.bin %in% c("bog", "poor fen", "rich fen", "ppc"))

custom_colors_peatland <- custom_colors[c("bog", "poor fen", "rich fen", "ppc")]

time_peat <- ggplot() +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5, color = "grey50") +
  geom_smooth(data    = jacobs_time_peatland,
              aes(x     = date_real,
                  y     = mean_jacobs,
                  color = class.bin),
              method  = "loess",
              span    = 0.3,
              se      = FALSE,
              size    = 1) +
  geom_line(data  = bui_time,
            aes(x = date_real,
                y = mean_BUI * bui_scale - 1),
            color = "black",
            lty   = 3,
            size  = 0.8) +
  geom_vline(xintercept = as.numeric(as.Date("2023-06-16")),
             lty   = 2,
             color = "black",
             alpha = 0.5,
             size  = 0.5) +
  annotate("text",
           x     = as.Date("2023-06-16"),
           y     = 0.45,
           label = "Early | Late",
           size  = 5,
           hjust = -0.1) +
  scale_color_manual(values = custom_colors_peatland) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d") +
  scale_y_continuous(
    name     = expression("Mean Jacobs' " ~ italic(D)),
    sec.axis = sec_axis(
      ~(. + 1) / bui_scale,
      name = "Mean BUI")) +
  labs(x     = "",
       color = "Land cover") +
  theme_bw() +
  theme(axis.text.x        = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y        = element_text(size = 14),
        axis.title.x       = element_text(size = 16),
        axis.title.y       = element_text(size = 16),
        panel.border       = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position    = "bottom",
        legend.text        = element_text(size = 14),
        legend.title       = element_text(size = 14),
        axis.title.y.right = element_text(color = "black", size = 16),
        axis.text.y.right  = element_text(color = "black", size = 14))

library(patchwork)
combined_plot <- time_subset / time_peat +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

combined_plot

# ggsave
ggsave(here("plots", "main", "fig6_jacobs_bui_time.png"), width = 8, height = 10, dpi = 300)
