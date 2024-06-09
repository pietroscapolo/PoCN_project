# UK Transport Analysis Script

# Load necessary libraries
library(sf)
library(ggplot2)
library(data.table)
library(dplyr)
library(RColorBrewer)
library(gridExtra)

# Load data
counties_data <- st_read("Data base/Counties boundaries/CTYUA_DEC_2023_UK_BUC.shp")
ttwa_data <- st_read("Data base/Travel to work areas/TTWA_2011_UK_BUC_500.shp")
population_data <- fread("Data base/population data.csv")
nodes <- fread("Data base/nodes.csv")
edges <- fread("Data base/edges.csv")
layer_mapping <- fread("Data base/layers.csv")

# Create plots for counties and TTWAs
p1 <- ggplot(data = counties_data) +
  geom_sf(fill = 'cyan', color = 'black') +  # Fill color is cyan and border color is black
  labs(title = "Counties in the UK") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Save the plot
ggsave(plot = p1, filename = "Grafici/counties.pdf", width = 7, height = 5, dpi = 300)

p2 <- ggplot(data = ttwa_data) +
  geom_sf(fill = 'magenta', color = 'black') +  # Fill color is magenta and border color is black
  labs(title = "Travel to Work Areas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Save the plot
ggsave(plot = p2, filename = "Grafici/ttwas.pdf")

# Combine the plots side by side
combined_plot <- plot_grid(p1, p2)

# Display the combined plot
#print(combined_plot)

# Join population data with counties
counties_data_with_pop <- counties_data %>%
  left_join(population_data %>% select(code, population), by = c("CTYUA23CD" = "code"))

# Calculate the intersections and proportional population
intersections <- st_intersection(counties_data_with_pop, ttwa_data)
intersections$intersection_area <- st_area(intersections)
counties_areas <- counties_data_with_pop %>%
  st_set_geometry(NULL) %>%
  mutate(original_area = st_area(st_geometry(counties_data_with_pop))) %>%
  select(CTYUA23CD, original_area)
intersections <- st_sf(intersections) %>%
  left_join(counties_areas, by = "CTYUA23CD") %>%
  mutate(
    area_proportion = intersection_area / original_area,
    proportional_population = population * area_proportion
  )
ttwa_population <- intersections %>%
  group_by(TTWA11CD) %>%
  summarise(Total_Population = sum(proportional_population, na.rm = TRUE))



# Compare with city population data for precision and recall test
pop <- fread("Data base/country-cities-data.csv")
cities_sf <- st_as_sf(pop, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
cities_sf <- st_transform(cities_sf, st_crs(ttwa_population))
cities_ttwa <- st_join(cities_sf, ttwa_population, join = st_within)
cities_ttwa$Total_Population <- as.numeric(units::drop_units(cities_ttwa$Total_Population))
cities_ttwa <- cities_ttwa %>%
  mutate(pop_check = ifelse(pop2024 <= Total_Population, 1, 0))
total_correct = sum(cities_ttwa$pop_check, na.rm = TRUE)
precision = total_correct / nrow(cities_ttwa)
recall = total_correct / sum(!is.na(cities_ttwa$Total_Population))
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")

# Plot population across TTWAs
ttwa_population$Total_Population <- as.numeric(units::drop_units(ttwa_population$Total_Population))
p <- ggplot(data = ttwa_population) +
  geom_sf(aes(fill = Total_Population)) +
  scale_fill_gradientn(colors = brewer.pal(5, "YlOrRd")) +
  labs(title = "Population Distribution Across TTWAs", fill = "Total Population") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8, hjust = 0),
    legend.title = element_text(hjust = 0.5)
  )
ggsave(plot = p, "Grafici/PopulationTTWAs.pdf")
#print(p)

# Discard low populated areas
threshold <- quantile(ttwa_population$Total_Population, 0.25, na.rm = TRUE)
ttwa_population <- ttwa_population %>%
  mutate(Category = ifelse(Total_Population >= threshold, "Above Threshold", "Below Threshold"))
ttwa_population <- st_as_sf(ttwa_population)
ttwa_population$Category_color <- ifelse(ttwa_population$Category == "Above Threshold", ttwa_population$Total_Population, NA)
p <- ggplot(data = ttwa_population) +
  geom_sf(aes(fill = Category_color), color = "black", size = 0.2) +
  scale_fill_gradientn(
    colors = brewer.pal(5, "YlOrRd"),
    na.value = "gray",
    limits = c(min(ttwa_population$Total_Population, na.rm = TRUE), max(ttwa_population$Total_Population, na.rm = TRUE))
  ) +
  labs(
    title = "Population across TTWAs",
    subtitle = "Areas colored by population, gray indicates below threshold",
    fill = "Total Population"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 8),
    legend.title = element_text(hjust = 0.5)
  )
ggsave(plot = p, "Grafici/LowedPopulationTTWAs.pdf")
#print(p)

# Filter nodes and edges based on population threshold
nodes_sf <- st_as_sf(nodes, coords = c("lon", "lat"), crs = 4326, agr = "constant", remove = FALSE)
nodes_sf <- st_transform(nodes_sf, st_crs(ttwa_data))
nodes_with_ttwa <- st_join(nodes_sf, ttwa_data, join = st_within) %>%
  st_set_geometry(NULL) %>%
  select(node, lon, lat, layer, TTWA11CD, TTWA11NM) %>%
  left_join(ttwa_population, by = c("TTWA11CD" = "TTWA11CD")) %>%
  mutate(threshold_category = ifelse(Total_Population >= threshold, "Above", "Below"))
node_degrees <- edges %>%
  select(ori_node, des_node) %>%
  pivot_longer(cols = c(ori_node, des_node), names_to = "type", values_to = "node") %>%
  group_by(node) %>%
  summarise(degree = n())
nodes_with_ttwa <- nodes_with_ttwa %>%
  left_join(node_degrees, by = "node") %>%
  mutate(degree = ifelse(is.na(degree), 0, degree))
nodes_in_threshold_ttwa <- nodes_with_ttwa %>%
  filter(threshold_category == "Above")
filtered_edges <- edges %>%
  filter(ori_node %in% nodes_in_threshold_ttwa$node & des_node %in% nodes_in_threshold_ttwa$node)

# Function to remove list columns
remove_list_columns <- function(df) {
  df <- df %>% mutate(across(where(is.list), ~sapply(., toString)))
  return(df)
}

#Function to calculate the interlayer connection matrix

calculate_interlayer_matrix <- function() {
  all_layers <- layer_mapping$layer
  layer_labels <- layer_mapping$layerLabel
  
  interlayer_matrix <- matrix(0, nrow = length(all_layers), ncol = length(all_layers))
  colnames(interlayer_matrix) <- layer_labels
  rownames(interlayer_matrix) <- layer_labels
  
  for (i in seq_along(all_layers)) {
    for (j in seq_along(all_layers)) {
      ori <- all_layers[i]
      des <- all_layers[j]
      edges_filtered <- edges %>%
        filter(ori_layer == ori & des_layer == des)
      
      connection_count <- nrow(edges_filtered)
      interlayer_matrix[i, j] <- connection_count
    }}
  
  return(interlayer_matrix)
}

#Function to plot the interlayer connection matrix

plot_interlayer_matrix <- function(interlayer_matrix) {
  interlayer_df <- melt(interlayer_matrix, varnames = c("Layer1", "Layer2"), value.name = "Connections")
  
  p <- ggplot(interlayer_df, aes(x = Layer1, y = Layer2, fill = log10(Connections + 1))) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "lightyellow", high = "darkgreen", na.value = "white") +
    labs(title = "Interlayer Connection Matrix for UK", x = "Layer", y = "Layer", fill = "Log(Connections)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  #print(p)
  
  ggsave("Grafici/interlayer_connection_matrix_UK.pdf", plot = p, width = 10, height = 8, dpi = 300)
}

#Calculate the interlayer connection matrix

interlayer_matrix <- calculate_interlayer_matrix()

#Plot the interlayer connection matrix

plot_interlayer_matrix(interlayer_matrix)


# Function to save layer data
save_layer_data <- function(layer, nodes_current_layer, edges_current_layer, ttwa_name) {
  if (nrow(nodes_current_layer) == 0 || nrow(edges_current_layer) == 0) {
    return()
  }
  
  layer_label <- layer_mapping$layerLabel[layer_mapping$layer == layer]
  nodes_to_save <- nodes_current_layer %>%
    select(node, layer, lat, lon, degree, geometry) %>%
    remove_list_columns()
  fwrite(nodes_to_save, paste0("Network_data/", ttwa_name, "/", layer_label, "_nodes.csv"))
  fwrite(edges_current_layer, paste0("Network_data/", ttwa_name, "/", layer_label, "_edges.csv"))
  
  edges_current_layer <- edges_current_layer %>%
    left_join(nodes_current_layer %>% select(node, lon, lat), by = c("ori_node" = "node")) %>%
    rename(x_from = lon, y_from = lat) %>%
    left_join(nodes_current_layer %>% select(node, lon, lat), by = c("des_node" = "node")) %>%
    rename(x_to = lon, y_to = lat) %>%
    filter(!is.na(x_from) & !is.na(y_from) & !is.na(x_to) & !is.na(y_to))
}

#Function to process TTWA data

process_ttwa <- function(ttwa_code) {
  ttwa_name <- unique(ttwa_data$TTWA11NM[ttwa_data$TTWA11CD == ttwa_code])
  if (length(ttwa_name) == 0) {
    warning(paste("TTWA name not found for TTWA code:", ttwa_code))
    return()
  }
  ttwa_name <- as.character(ttwa_name)[1]
  ttwa_name <- gsub("[^a-zA-Z0-9]", "_", ttwa_name)
  dir.create(paste0("Network_data/", ttwa_name), recursive = TRUE, showWarnings = FALSE)
  nodes_ttwa <- nodes_in_threshold_ttwa %>%
    filter(TTWA11CD == ttwa_code) %>%
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(ttwa_data), agr = "constant", remove = FALSE)
  edges_ttwa <- filtered_edges %>%
    filter(ori_node %in% nodes_ttwa$node & des_node %in% nodes_ttwa$node)
  layers <- unique(layer_mapping$layer)
  for (layer in layers) {
    nodes_current_layer <- nodes_ttwa[nodes_ttwa$layer == layer, ]
    edges_current_layer <- edges_ttwa[edges_ttwa$ori_layer == layer & edges_ttwa$des_layer == layer, ]
    save_layer_data(layer, nodes_current_layer, edges_current_layer, ttwa_name)
  }
}

#Process TTWA data

for (ttwa in unique(nodes_in_threshold_ttwa$TTWA11CD)) {
  process_ttwa(ttwa)
}

#Function to process UK data

process_uk <- function() {
  uk_name <- "UK"
  dir.create(paste0("Network_data/", uk_name), recursive = TRUE, showWarnings = FALSE)
  nodes_uk <- nodes_in_threshold_ttwa
  edges_uk <- filtered_edges
  layers <- unique(layer_mapping$layer)
  for (layer in layers) {
    nodes_current_layer <- nodes_uk[nodes_uk$layer == layer, ]
    edges_current_layer <- edges_uk[edges_uk$ori_layer == layer & edges_uk$des_layer == layer, ]
    save_layer_data(layer, nodes_current_layer, edges_current_layer, uk_name)
  }
}

#Process UK data

process_uk()

#Function to plot the network for a specific layer in the UK

plot_network_layer_for_uk <- function(layer) {
  layer_label <- layer_mapping$layerLabel[layer_mapping$layer == layer]
  nodes_file <- paste0("Network_data/UK/", layer_label, "_nodes.csv")
  edges_file <- paste0("Network_data/UK/", layer_label, "_edges.csv")
  
  if (!file.exists(nodes_file) || !file.exists(edges_file)) {
    return()
  }
  
  nodes_uk_layer <- fread(nodes_file)
  edges_uk_layer <- fread(edges_file)
  
  nodes_sf <- st_as_sf(nodes_uk_layer, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  nodes_sf <- st_transform(nodes_sf, st_crs(ttwa_data))
  nodes_uk_layer <- nodes_uk_layer %>%
    mutate(lon_trans = st_coordinates(nodes_sf)[,1],
           lat_trans = st_coordinates(nodes_sf)[,2])
  
  edges_df <- edges_uk_layer %>%
    left_join(nodes_uk_layer %>% select(node, lon_trans, lat_trans), by = c("ori_node" = "node")) %>%
    rename(x_from = lon_trans, y_from = lat_trans) %>%
    left_join(nodes_uk_layer %>% select(node, lon_trans, lat_trans), by = c("des_node" = "node")) %>%
    rename(x_to = lon_trans, y_to = lat_trans)
  
  transport_mode <- layer_label
  colors <- c("Rail" = "#00FF99", "Coach" = "#FFA500", "Air" = "#FF0045", "Ferry" = "#0090FF", "Metro" = "#FF00FF", "Bus" = "#FFFF00")
               edge_color <- colors[transport_mode]
               
               uk_map <- st_geometry(ttwa_data)
               p <- ggplot() +
                 geom_sf(data = uk_map, fill = "gray20", color = "gray40") +
                 geom_segment(data = edges_df, aes(x = x_from, y = y_from, xend = x_to, yend = y_to), color = edge_color, size = 0.5, alpha = 0.6) +
                 geom_point(data = nodes_uk_layer, aes(x = lon_trans, y = lat_trans), color = edge_color, alpha = 0.8) +
                 scale_size_continuous(range = c(0.5, 5)) +
                 theme_minimal() +
                 theme(
                   plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "white"),
                   plot.subtitle = element_text(size = 14, color = "white"),
                   legend.position = "none",
                   panel.background = element_rect(fill = "black"),
                   plot.background = element_rect(fill = "black")
                 ) +
                 ggtitle(paste(transport_mode, "Network for UK"))
               
               #print(p)
               
               ggsave(paste0("Grafici/network_layer_", layer, "_", transport_mode, "_UK.pdf"), plot = p, width = 10, height = 8, dpi = 300)
}

#Plot the network for each layer in the UK

for (layer in unique(layer_mapping$layer)) {
  plot_network_layer_for_uk(layer)
}

#Function to plot the network for a specific layer in a TTWA

plot_network_layer_for_ttwa <- function(ttwa_code, layer) {
  ttwa_name <- unique(ttwa_data$TTWA11NM[ttwa_data$TTWA11CD == ttwa_code])
  
  if (length(ttwa_name) == 0) {
    warning(paste("TTWA name not found for TTWA code:", ttwa_code))
    return()
  }
  
  ttwa_name <- as.character(ttwa_name)[1]
  ttwa_name <- gsub("[^a-zA-Z0-9]", "_", ttwa_name)
  
  layer_label <- layer_mapping$layerLabel[layer_mapping$layer == layer]
  nodes_file <- paste0("Network_data/", ttwa_name, "/", layer_label, "_nodes.csv")
  edges_file <- paste0("Network_data/", ttwa_name, "/", layer_label, "_edges.csv")
  
  if (!file.exists(nodes_file) || !file.exists(edges_file)) {
    return()
  }
  
  nodes_ttwa_layer <- fread(nodes_file)
  edges_ttwa_layer <- fread(edges_file)
  
  ttwa_sf <- ttwa_data %>% filter(TTWA11CD == ttwa_code)
  
  nodes_sf <- st_as_sf(nodes_ttwa_layer, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  nodes_sf <- st_transform(nodes_sf, st_crs(ttwa_sf))
  nodes_ttwa_layer <- nodes_ttwa_layer %>%
    mutate(lon_trans = st_coordinates(nodes_sf)[,1],
           lat_trans = st_coordinates(nodes_sf)[,2])
  
  edges_df <- edges_ttwa_layer %>%
    left_join(nodes_ttwa_layer %>% select(node, lon_trans, lat_trans), by = c("ori_node" = "node")) %>%
    rename(x_from = lon_trans, y_from = lat_trans) %>%
    left_join(nodes_ttwa_layer %>% select(node, lon_trans, lat_trans), by = c("des_node" = "node")) %>%
    rename(x_to = lon_trans, y_to = lat_trans)
  
  transport_mode <- layer_label
  colors <- c("Rail" = "#00FF99", "Coach" = "#FFA500", "Air" = "#FF0045", "Ferry" = "#0000FF", "Metro" = "#FF00FF", "Bus" = "#FFFF00")
               edge_color <- colors[transport_mode]
               
               ttwa_map <- st_geometry(ttwa_sf)
               p <- ggplot() +
                 geom_sf(data = ttwa_map, fill = "gray20", color = "gray40") +
                 geom_segment(data = edges_df, aes(x = x_from, y = y_from, xend = x_to, yend = y_to), color = edge_color, size = 0.5, alpha = 0.6) +
                 geom_point(data = nodes_ttwa_layer, aes(x = lon_trans, y = lat_trans, size = degree), color = edge_color, alpha = 0.8) +
                 scale_size_continuous(range = c(0.5, 5)) +
                 theme_minimal() +
                 theme(
                   plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "white"),
                   plot.subtitle = element_text(size = 14, color = "white"),
                   legend.position = "none",
                   panel.background = element_rect(fill = "black"),
                   plot.background = element_rect(fill = "black")
                 ) +
                 ggtitle(paste(transport_mode, "Network for", ttwa_name))
               
               #print(p)
               
               ggsave(paste0("Grafici/network_layer_", layer, "", transport_mode, "", ttwa_name, ".pdf"), plot = p, width = 10, height = 8, dpi = 300)
}

#Plot the network for each layer in each TTWA

for (ttwa in unique(ttwa_data$TTWA11CD)) {
  for (layer in unique(layer_mapping$layer)) {
    plot_network_layer_for_ttwa(ttwa, layer)
  }
}



#Function to create a single plot for a layer

create_layer_plot <- function(layer) {
  layer_label <- layer_mapping$layerLabel[layer_mapping$layer == layer]
  nodes_file <- paste0("Network_data/UK/", layer_label, "_nodes.csv")
  edges_file <- paste0("Network_data/UK/", layer_label, "_edges.csv")
  
  if (!file.exists(nodes_file) || !file.exists(edges_file)) {
    return(NULL)
  }
  
  nodes_layer <- fread(nodes_file)
  edges_layer <- fread(edges_file)
  
  if (nrow(nodes_layer) == 0 || nrow(edges_layer) == 0) {
    return(NULL)
  }
  
  nodes_sf <- st_as_sf(nodes_layer, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  nodes_sf <- st_transform(nodes_sf, st_crs(ttwa_data))
  nodes_layer <- nodes_layer %>%
    mutate(lon_trans = st_coordinates(nodes_sf)[,1],
           lat_trans = st_coordinates(nodes_sf)[,2])
  
  edges_df <- edges_layer %>%
    left_join(nodes_layer %>% select(node, lon_trans, lat_trans), by = c("ori_node" = "node")) %>%
    rename(x_from = lon_trans, y_from = lat_trans) %>%
    left_join(nodes_layer %>% select(node, lon_trans, lat_trans), by = c("des_node" = "node")) %>%
    rename(x_to = lon_trans, y_to = lat_trans)
  
  transport_mode <- layer_label
  colors <- c("Rail" = "#00FF99", "Coach" = "#FFA500", "Air" = "#FF0045", "Ferry" = "#0090FF", "Metro" = "#FF00FF", "Bus" = "#FFFF00")
               edge_color <- colors[transport_mode]
               
               uk_map <- st_geometry(ttwa_data)
               p <- ggplot() +
                 geom_sf(data = uk_map, fill = "gray20", color = "gray40") +
                 geom_segment(data = edges_df, aes(x = x_from, y = y_from, xend = x_to, yend = y_to), color = edge_color, size = 0.5, alpha = 0.6) +
                 geom_point(data = nodes_layer, aes(x = lon_trans, y = lat_trans), color = edge_color, alpha = 0.8, size=0.01) +
                 theme_void() +
                 theme(
                   plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "white"),
                   plot.subtitle = element_text(size = 14, color = "white"),
                   legend.position = "none",
                   panel.background = element_rect(fill = "black"),
                   plot.background = element_rect(fill = "black")
                 ) +
                 ggtitle(transport_mode)
               
               return(p)
}

#Function to combine plots of different layers

plot_multilayer_network <- function() {
  layers <- unique(layer_mapping$layer)
  plot_list <- list()
  
  for (layer in layers) {
    p <- create_layer_plot(layer)
    if (!is.null(p)) {
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
  
  if (length(plot_list) > 0) {
    combined_plot <- plot_grid(plotlist = plot_list, ncol = length(plot_list), align = "h")
    ggsave("Grafici/multilayer_network_uk.png", combined_plot, width = 15, height = 5, dpi = 300)
    #print(combined_plot)
  } else {
    print("No data available to plot.")
  }
}

#Execute the function to create the multilayer plot

plot_multilayer_network()

