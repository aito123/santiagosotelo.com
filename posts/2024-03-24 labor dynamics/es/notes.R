pacman::p_load(tidyverse, gganimate, gapminder)
theme_set(theme_classic())

gap <- gapminder %>%
  filter(continent == "Asia") %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-gdpPercap) * 1) %>%
  ungroup()

p <- ggplot(gap, aes(rank, group = country,
                     fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = gdpPercap/2,
                height = gdpPercap,
                width = 0.9), alpha = 0.8, color = NA) +

  # text in x-axis (requires clip = "off" in coord_*)
  # paste(country, " ")  is a hack to make pretty spacing, since hjust > 1
  #   leads to weird artifacts in text spacing.
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +

  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = "none", fill = "none") +

  labs(title='{closest_state}', x = "", y = "GFP per capita") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +

  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, fps = 25, duration = 20, width = 800, height = 600)

# ---

gap_smoother <- gapminder %>%
  filter(continent == "Asia") %>%
  group_by(country) %>%
  # Do somewhat rough interpolation for ranking
  # (Otherwise the ranking shifts unpleasantly fast.)
  complete(year = full_seq(year, 1)) %>%
  mutate(gdpPercap = spline(x = year, y = gdpPercap, xout = year)$y) %>% #fill missing values
  group_by(year) %>%
  mutate(rank = min_rank(-gdpPercap) * 1) %>% # creo que esto ahora el arrange, y el grouping lo puedo hacer con .by
  ungroup() %>%

  # Then interpolate further to quarter years for fast number ticking.
  # Interpolate the ranks calculated earlier.
  group_by(country) %>%
  complete(year = full_seq(year, .5)) %>% #manya años intermedios
  mutate(gdpPercap = spline(x = year, y = gdpPercap, xout = year)$y) %>% #add missing values
  # "approx" below for linear interpolation. "spline" has a bouncy effect.
  mutate(rank =      approx(x = year, y = rank,      xout = year)$y) %>% #manya ranking intermedio1 // missings
  ungroup()  %>%
  arrange(country,year)

p2 <- ggplot(gap_smoother,
            aes(rank, group = country,
                fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = gdpPercap/2,
                height = gdpPercap,
                width = 0.9), alpha = 0.8, color = NA) +

  # text in x-axis (requires clip = "off" in coord_*)
  # paste(country, " ")  is a hack to make pretty spacing, since hjust > 1
  #   leads to weird artifacts in text spacing.
            # This line for the numbers that tick up
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = gdpPercap,
                label = as.factor(scales::comma(gdpPercap, accuracy = 1))), hjust = 0, nudge_y = 300 ) +
  coord_flip(clip = "off", expand = F) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150000)) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
            labs(title='{closest_state %>% as.numeric %>% floor}',
                 x = "", y = "GFP per capita") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
            transition_states(year, transition_length = 1, state_length = 0, wrap = FALSE) +
              enter_grow() +
              exit_shrink() +
              ease_aes('linear')

p2

animate(p2, fps = 20, duration = 30, width = 800, height = 600, end_pause = 20)
# animate(p, fps = 20, duration = 5, width = 400, height = 600, end_pause = 10)

anim_save(here("es", "plot1.gif"), animation = p2, fps = 25, duration = 30, width = 800, height = 600, end_pause = 20)


df <- bind_rows(
  df_male %>% rename_with(~gsub("Labour force participation rate, male (% ages 15 and older) ", "lfpr_", .x, fixed = T), starts_with("Labour")) %>% mutate(gender = "Male", .before = starts_with("lfpr")),
  df_female %>% rename_with(~gsub("Labour force participation rate, female (% ages 15 and older) ", "lfpr_", .x, fixed = T), starts_with("Labour")) %>% mutate(gender = "Female", .before = starts_with("lfpr"))
) %>%
  clean_names() %>%
  pivot_longer(cols = 7:38,
               names_to = c(NA, "year"),
               names_sep = "\\_"
  ) %>%
  # Getting South America
  mutate(
    continent = case_when(
      country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                     "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname",
                     "Uruguay", "Venezuela") ~ "South America",
      .default = continent
    )
  ) %>%
  # Only South America
  filter(continent %in% "South America") %>%
  # arrange(gender, year, continent, desc(value)) %>%
  mutate(rank = min_rank(-value) * 1.0, .by = c(gender, year, continent)) %>%
  mutate(year = as.numeric(year)) %>%

  group_by(country) %>%
  complete(year = full_seq(year, .5)) %>%
  mutate(
    value = spline(x = year, y = value, xout = year)$y, # valores intermedios
    rank = approx(x = year, y = rank, xout = year)$y # "approx" below for linear interpolation. "spline" has a bouncy effect.
  ) %>%
  ungroup() %>%
  arrange(country,year)

p_female <-
  df %>%
  filter(continent %in% "South America" & gender %in% "Female") %>%
  ggplot(aes(rank, group = country, fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +

  # text in x-axis (requires clip = "off" in coord_*)
  # paste(country, " ")  is a hack to make pretty spacing, since hjust > 1
  #   leads to weird artifacts in text spacing.
  # This line for the numbers that tick up
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = value,
                label = as.factor(scales::percent(value, scale = 1, accuracy = 0.01))),
            hjust = 0, vjust = 0.5 , nudge_y = 2) + #as.factor // , hjust = 0, nudge_y = 300
  coord_flip(clip = "off", expand = F) + #facet_wrap(~year)
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), limits = c(0, 100)) +
  scale_x_reverse() +
  guides(color = "none", fill = "none") +
  labs(title = "Participación de la fuerza laboral femenina (15 años a más)",
       subtitle='{closest_state %>% as.numeric %>% floor()}',
       x = "", y = "Participación de la fuerza laboral (%)") +
  theme(plot.title = element_text(hjust = 0, size = 20),
        plot.subtitle = element_text(hjust = 0, size = 15),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(year, transition_length = 1, state_length = 0, wrap = FALSE) +
  enter_grow() +
  exit_shrink() +
  ease_aes('linear')

animate(p_female, fps = 20, duration = 30, width = 800, height = 600, end_pause = 30)
