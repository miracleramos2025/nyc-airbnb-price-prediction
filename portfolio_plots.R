# load packages
library(tidymodels)
library(broom)
library(ggplot2)
library(dplyr)
library(here)

# load predictions from your model
load(here("results", "final_preds.rda"))


# load model performance results
load(here("results", "model_performance_summary.rda"))

############################################################
# PLOT 1 - predicted vs actual 
#############################################################
predicted_actual_plot <- final_preds_clean |>
  ggplot(aes(x = .pred, y = price)) +
  geom_point(alpha = 0.7, color = "#0F2656") +
  geom_smooth(method = "lm", color = "#5FC6F2", se = FALSE, linewidth = 1.95) +
  
  # tighter cropping
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 500)) +
  
  labs(
    title = "Predicted vs Actual Airbnb Prices",
    x = "Predicted Price ($)",
    y = "Actual Price ($)"
  ) +
  
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 30,
      color = "#0F2656"
    ),
    
    axis.title.x = element_text(
      size = 24,
      face = "bold",
      color = "#0F2656",
      margin = margin(t = 20)
    ),
    
    axis.title.y = element_text(
      size = 24,
      face = "bold",
      color = "#0F2656",
      margin = margin(r = 20)
    ),
    
    axis.text = element_text(
      size = 20,
      face = "bold",
      color = "#0F2656"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    
    # keep margin so portfolio card doesn't crop it
    plot.margin = margin(40, 40, 40, 40)
  )

# save plot to plots folder
ggsave(
  filename = here("plots", "predicted_vs_actual.png"),
  plot = predicted_actual_plot,
  width = 12.5,
  height = 7.1,
  dpi = 300,
  bg = "#F2F3F4"
)


#############################################################
# PLOT 2 - price distribution
#############################################################
price_distribution_plot <- ggplot(airbnb_data, aes(x = price)) +
  geom_density(fill = "#5FC6F2", alpha = 0.6) +
  xlim(0, 800) +
  labs(
    title = "Distribution of Airbnb Prices",
    x = "Price ($)",
    y = "Density"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 30,
      color = "#0F2656"
    ),
    
    axis.title.x = element_text(
      size = 24,
      face = "bold",
      color = "#0F2656",
      margin = margin(t = 20)
    ),
    
    axis.title.y = element_text(
      size = 24,
      face = "bold",
      color = "#0F2656",
      margin = margin(r = 20)
    ),
    
    axis.text = element_text(
      size = 20,
      face = "bold",
      color = "#0F2656"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    plot.margin = margin(40, 40, 40, 40)
  )

ggsave(
  filename = here("plots", "price_distribution.png"),
  plot = price_distribution_plot,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "#F2F3F4"
)


#############################################################
# PLOT 3 - model performance results 
#############################################################

# model comparison plot
# load model performance
# load model performance
load(here("results", "model_performance_summary.rda"))

model_rmse_plot <- model_performance |>
  dplyr::select(Model, RMSE) |>
  dplyr::arrange(RMSE) |>
  
  # add line breaks to long model names
  dplyr::mutate(
    Model = dplyr::case_when(
      Model == "Linear Model" ~ "Linear\nModel",
      Model == "Random Forest" ~ "Random\nForest",
      Model == "Boosted Trees" ~ "Boosted\nTrees",
      Model == "Elastic Net" ~ "Elastic\nNet",
      TRUE ~ Model
    )
  ) |>
  
  dplyr::mutate(Model = factor(Model, levels = Model)) |>
  
  ggplot(aes(x = Model, y = RMSE, group = 1)) +
  
  geom_line(color = "#5FC6F2", linewidth = 1.95) +
  geom_point(size = 6, color = "#0F2656") +
  
  labs(
    title = "Model Performance Comparison",
    x = "",
    y = "RMSE"
  ) +
  
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 30, color = "#0F2656"),
    
    axis.title = element_text(
      size = 24,
      face = "bold",
      color = "#0F2656"
    ),
    
    axis.title.y = element_text(
      size = 24,
      face = "bold",
      color = "#0F2656",
      margin = margin(r = 20)
    ),
    
    axis.text.x = element_text(
      size = 20,
      face = "bold",
      color = "#0F2656",
      hjust = 0.5
    ),
    
    axis.text.y = element_text(
      size = 20,
      face = "bold",
      color = "#0F2656"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    
    plot.margin = margin(40,40,40,40)
  )

# save plot
ggsave(
  filename = here("plots", "model_rmse_comparison.png"),
  plot = model_rmse_plot,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "#F2F3F4"
)

#############################################################
# PLOT 4 - room type by borough heatmap
#############################################################

heatmap_data <- airbnb_data |>
  group_by(neighbourhood_group, room_type) |>
  summarize(
    median_price = median(price, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    neighbourhood_group = factor(
      neighbourhood_group,
      levels = c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
    ),
    room_type = dplyr::case_when(
      room_type == "Shared room" ~ "Shared\nRoom",
      room_type == "Private room" ~ "Private\nRoom",
      room_type == "Entire home/apt" ~ "Entire\nHome/Apt",
      TRUE ~ room_type
    ),
    room_type = factor(
      room_type,
      levels = c("Shared\nRoom", "Private\nRoom", "Entire\nHome/Apt")
    ),
    label_color = if_else(median_price >= 120, "white", "#0F2656")
  )

room_borough_heatmap <- heatmap_data |>
  ggplot(aes(x = neighbourhood_group, y = room_type, fill = median_price)) +
  geom_tile(color = "#F2F3F4", linewidth = 2.2) +
  geom_text(
    aes(label = paste0("$", round(median_price)), color = label_color),
    fontface = "bold",
    size = 8.5
  ) +
  scale_color_identity() +
  scale_fill_gradient(
    low = "#D9F2FD",
    high = "#0F2656"
  ) +
  labs(
    title = "Median Airbnb Price by Borough & Room Type",
    x = "Borough",
    y = "Room Type",
    fill = "Median Price"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 29,
      color = "#0F2656",
      hjust = 0
    ),
    axis.title.x = element_text(
      size = 28,
      face = "bold",
      color = "#0F2656",
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      size = 28,
      face = "bold",
      color = "#0F2656",
      margin = margin(r = 15)
    ),
    axis.text.x = element_text(
      size = 20,
      face = "bold",
      color = "#0F2656",
      angle = 0,
      margin = margin(t = 5)
    ),
    axis.text.y = element_text(
      size = 20,
      face = "bold",
      color = "#0F2656",
      margin = margin(r = 5)
    ),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(35, 35, 35, 35)
  )

ggsave(
  filename = here("plots", "room_type_borough_heatmap.png"),
  plot = room_borough_heatmap,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "#F2F3F4"
)

