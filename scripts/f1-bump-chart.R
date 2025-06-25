# ---- Load packages and fonts ---- 
# Load packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, rvest, ggbump, sysfonts, showtext, ggimage, plotly, gghighlight, pdftools)

# Load fonts
font_add_google("IBM Plex Sans Condensed", "ibm plex sans condensed")
font_add_google("Poppins", "poppins")
showtext_auto()

font <- "ibm plex sans condensed"
title_font <- "poppins"

# ---- Input relevant Grand Prix info ----
grandPrix <- "Miami"
year <- "2024"
totalLaps <- 57
finisherLaps <- c(57) # lap number of races finishers because race ends before all hit max
DNFs <- c("Logan Sargeant") # Sets DNF drivers
link <-  "https://motorsportstats.com/api/result-statistics?sessionSlug=fia-formula-one-world-championship_2025_canadian-grand-prix_race&sessionFact=LapChart&size=10000"

# Team colours
team_colors <- c(
  "Mercedes" = "#00A19B",
  "Red Bull" = "#00174C",
  "McLaren" = "#FF8000",
  "Ferrari" = "#EF1A2D",
  "Aston Martin" = "#002420",
  "RB" = "#6692FF",
  "Haas" = "#B6BABD",
  "Alpine"= "#0093CC",
  "Williams" = "#64C4FF",
  "Kick Sauber" = "#00E700"
)


# ---- Obtain F1 race data ----
page = read_html(link)

# Extract table
positions <- map(2:20, ~ page |>
                   html_nodes(paste0(".gygLpH:nth-child(", .x, ") span")) |>
                   html_text() |>
                   as.integer() |>
                   bind_cols())

# Extract laps as vector
Lap <- page |> html_nodes(".jlGbof") |> html_text()

# ---- Clean and prepare data ----
# Combine data into single data frame
Race <- tibble(Lap = Lap) |> bind_cols(positions)
colnames(Race) <- c("Lap", paste0("P", 1:20))

# Convert driver numbers in Race to character if they are not already
Race <- Race |> 
  mutate(across(starts_with("P"), ~as.character(.))) 

# Driver lookup vector
driverLookup <- c(
  "1" = "Max Verstappen", 
  "2" = "Logan Sargeant", 
  "3" = "Daniel Ricciardo", 
  "4" = "Lando Norris",
  "10" = "Pierre Gasly", 
  "11" = "Sergio Perez", 
  "14" = "Fernando Alonso", 
  "16" = "Charles Leclerc",
  "18" = "Lance Stroll", 
  "20" = "Kevin Magnussen", 
  "22" = "Yuki Tsunoda", 
  "23" = "Alex Albon",
  "24" = "Zhou Guanyu", 
  "27" = "Nico Hulkenberg", 
  "31" = "Esteban Ocon", 
  "44" = "Lewis Hamilton",
  "55" = "Carlos Sainz", 
  "63" = "George Russell", 
  "77" = "Valtteri Bottas", 
  "81" = "Oscar Piastri"
)

# Replace numbers with driver names
Race <- Race |> 
  mutate(across(starts_with("P"), ~driverLookup[.]))

# Pivot longer and add new columns
Race <- Race |> 
  pivot_longer(cols = P1:P20, names_to = "Position", values_to = "Driver") |> 
  mutate(
    Rank = as.numeric(str_remove(Position, "P")),
    Lap = as.numeric(str_remove(Lap, "Lap ")) |> replace_na(0),
    Last_name = word(Driver, 2),
    Team = case_when(
      Driver == "Fernando Alonso" ~ "Aston Martin", 
      Driver == "Lance Stroll" ~ "Aston Martin",
      Driver == "Pierre Gasly" ~ "Alpine", 
      Driver == "Esteban Ocon" ~ "Alpine",
      Driver == "Charles Leclerc" ~ "Ferrari",
      Driver == "Carlos Sainz" ~ "Ferrari",
      Driver == "Kevin Magnussen" ~ "Haas",
      Driver == "Nico Hulkenberg" ~ "Haas",
      Driver == "Max Verstappen" ~ "Red Bull", 
      Driver == "Sergio Perez" ~ "Red Bull",
      Driver == "Daniel Ricciardo" ~ "RB",
      Driver == "Yuki Tsunoda" ~ "RB", 
      Driver == "Lando Norris" ~ "McLaren",
      Driver == "Oscar Piastri" ~ "McLaren",
      Driver == "Logan Sargeant" ~ "Williams",
      Driver == "Alex Albon" ~ "Williams",
      Driver == "Zhou Guanyu" ~ "Kick Sauber", 
      Driver == "Valtteri Bottas" ~ "Kick Sauber", 
      Driver == "Lewis Hamilton" ~ "Mercedes",
      Driver == "George Russell" ~ "Mercedes"
    )
  )

# Filters out laps of drivers that didn't complete race
Race <- Race |> 
  filter(!is.na(Driver))

# Adds driver name abbreviations
Race <- Race |> 
  mutate(AbbName = case_when(
    Driver == "Alex Albon" ~ "ALB",
    Driver == "Fernando Alonso" ~ "ALO",
    Driver == "Oliver Bearman" ~ "BEA",
    Driver == "Valtteri Bottas" ~ "BOT",
    Driver == "Nyck de Vries" ~ "DEV",
    Driver == "Pierre Gasly" ~ "GAS",
    Driver == "Lewis Hamilton" ~ "HAM",
    Driver == "Nico Hulkenberg" ~ "HUL",
    Driver == "Nicholas Latifi" ~ "LAT",
    Driver == "Liam Lawson" ~ "LAW",
    Driver == "Charles Leclerc" ~ "LEC",
    Driver == "Kevin Magnussen" ~ "MAG",
    Driver == "Lando Norris" ~ "NOR",
    Driver == "Esteban Ocon" ~ "OCO",
    Driver == "Sergio Perez" ~ "PER",
    Driver == "Oscar Piastri" ~ "PIA",
    Driver == "Daniel Ricciardo" ~ "RIC",
    Driver == "George Russell" ~ "RUS",
    Driver == "Carlos Sainz" ~ "SAI",
    Driver == "Logan Sargeant" ~ "SAR",
    Driver == "Mich Schumacher" ~ "SCH",
    Driver == "Lance Stroll" ~ "STR",
    Driver == "Yuki Tsunoda" ~ "TSU",
    Driver == "Max Verstappen" ~ "VER",
    Driver == "Zhou Guanyu" ~ "ZHO",
  )
  )

# Extracts rank of last race finisher
DNF_rank <- Race |> 
  select(Lap, Rank) |> 
  filter(Lap %in% finisherLaps) |>
  filter(Rank == max(Rank)) |> 
  pull(Rank)



# Function to extend lines for drivers who finished the race
extend_lines <- function(data, max_lap) {
  data %>%
    filter(!Driver %in% DNFs) %>%
    group_by(Driver) %>%
    filter(Lap == max(Lap)) %>%
    mutate(Lap = max_lap) %>%
    ungroup()
}

# Extend lines for drivers who finished the race
extended_lines <- extend_lines(Race, max(Race$Lap))

# Combine the extended lines with the original dataset
Race <- Race %>%
  bind_rows(extended_lines) %>%
  arrange(Driver, Lap)

# Function to lighten a color
lighten <- function(color, factor=0.4){
  col <- col2rgb(color)
  col <- col + (255 - col) * factor
  col <- rgb(t(col), maxColorValue = 255)
  return(col)
}

# Determine the line type and color for each driver based on their initial rank (Lap 0)
initial_ranks <- Race |> 
  filter(Lap == 0) |> 
  group_by(Team) |> 
  arrange(Rank) |> 
  mutate(Line = ifelse(row_number() == 1, "solid", "dotted"),
         Colour = ifelse(row_number() == 1, team_colors[Team], lighten(team_colors[Team], factor=0.4))) |> 
  select(Driver, Team, Line, Colour)

# Join the initial ranks back to the main data
Race <- Race |> 
  left_join(initial_ranks, by = c("Driver", "Team"))

# Create a separate dataframe for the DNF drivers
DNF_drivers <- Race |> 
  filter(Driver %in% DNFs) |> 
  group_by(Driver) |> 
  filter(Lap == max(Lap)) |> 
  ungroup() |> 
  arrange(desc(Position)) |> 
  mutate(Lap = max(Race$Lap),
         Rank = DNF_rank + row_number(),
         DNF_label = paste("DNF – ", Last_name))

dnfPoints <- Race |> 
  filter(Driver %in% DNFs) |> 
  group_by(Driver) |> 
  filter(Lap == max(Lap) & Lap > 0)

nonStarters <- Race |> 
  group_by(Driver) |> 
  filter(max(Lap) == 0) |> 
  pull(Driver)


# ---- Plot ----
#Breaks for background rectangles
rects <- data.frame(
  xstart = seq(0, floor(totalLaps), 10), # LAPS ROUNDED DOWN TO NEAREST 10
  xend = c(seq(10, floor(totalLaps), 10), floor(totalLaps) + 3), # LAPS ROUNDED DOWN TO NEAREST 10 + (LAPS +3)
  col = rep(c("a", "b"), length.out = 6)
)
rects_colours <- c("a" = "#FFFFFF", "b" = "#e9e9e9")

# Filter the start and end points for the plot
filtered_points_min_lap <- Race |> 
  group_by(Driver) |> 
  filter(Lap == 0)

filtered_points_max_lap <- Race |> 
  group_by(Driver) |> 
  filter(Lap == max(Lap) & !(Driver %in% DNFs))

# Colour palette
F1_col <- c(
  "bg" = "#FAFAFF",
  "mt" = "#00243f",
  "Mercedes" = "#00A19B",
  "Red Bull" = "#00174C",
  "McLaren" = "#FF8000",
  "Ferrari" = "#EF1A2D",
  "Aston Martin" = "#002420",
  "RB" = "#6692FF",
  "Haas" = "#B6BABD",
  "Alpine"= "#0093CC",
  "Williams" = "#64C4FF",
  "Kick Sauber" = "#00E700"
)

sizes <- c(
  "names" = 9,
  "ranks" = 6.2,
  "points" = 3.5,
  "lines" = 0.8
)

# Plot the bump chart
Miami <- ggplot(Race, aes(Lap, Rank, group = Driver)) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = 0, ymax = 21, fill = col), inherit.aes = FALSE, alpha = 0.4) +
  geom_bump(data = Race |> filter(!(Driver %in% nonStarters)), aes(color = Colour, linetype = ifelse(Line == "Solid", "solid", "dotted")), size = 0.8) +
  geom_point(data = filtered_points_min_lap, aes(color = Colour), size = sizes[["points"]], position = position_nudge(x = -0.7)) + # Nudge points at min lap to the left
  geom_point(data = filtered_points_max_lap, aes(color = Colour), size = sizes[["points"]]) + # Points at max lap without nudging
  geom_point(data = dnfPoints, aes(color = Colour), size = 3, shape = 18) + # Problem with order means dnfPoints dont have Colour column atm
  geom_text(data = Race |> group_by(Driver) |> filter(Lap == min(Lap)),
            aes(x = Lap - 0.7, label = Rank), size = sizes[["ranks"]], hjust = 0.5, colour = F1_col[["bg"]], fontface = "bold") +
  geom_text(data = Race |> group_by(Driver) |> filter(Lap == max(Lap),
                                                      !(Driver %in% DNFs)),
            aes(x = Lap , label = Rank), size = sizes[["ranks"]], hjust = 0.5, colour = F1_col[["bg"]], fontface = "bold") +
  geom_text(data = Race %>% group_by(Driver) |> filter(Lap == min(Lap)),
            aes(x = Lap - 2, label = Last_name, colour = Colour), size = sizes[["names"]], hjust = 1) + # Left hand driver text
  geom_text(data = Race %>% group_by(Driver) |> filter(Lap == max(Lap),
                                                       !(Driver %in% DNFs)),
            aes(x = max(Lap) + 1.5, label = Last_name, colour = Colour), size = sizes[["names"]], hjust = 0) + # Right hand driver text
  geom_text(data = DNF_drivers, aes(x = Lap + 1.5, y = Rank, label = DNF_label, colour = Colour), size = sizes[["names"]], hjust = 0) + # DNF driver text on the right
  scale_x_continuous(limits = c(-2, (totalLaps + 6)),
                     breaks = seq(0, floor(totalLaps), 10)) +
  scale_y_reverse(limits = c(21, 0), breaks = seq(1, 20, by = 1)) + # Increase space between y-axis points
  coord_cartesian(clip = "off") +
  scale_colour_identity() +
  scale_fill_manual(values = rects_colours) +
  theme_minimal() +
  labs(
    title = paste0(grandPrix, " Grand Prix - ", year),
    caption = "Data: Motorsportstats.com | Plot: DoingDataViz"
  ) +
  theme(
    text = element_text(family = font),
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 25),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 26, margin = margin(-5, 0, 0, 0)),
    axis.title.x = element_blank(),
    plot.background = element_rect(fill = F1_col[["bg"]]),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(size = 12, margin = margin (5, 0, 0, 0), family = font),
    plot.title = element_text(size = 55, margin = margin(5, 0, 0, 0), family = title_font, face = "bold", hjust = -0.1),
  )
+ gghighlight(Last_name == "Hamilton",
              use_direct_label = FALSE)


ggsave("images/Miami.png", width = 1920, height = 1080, units = "px")

