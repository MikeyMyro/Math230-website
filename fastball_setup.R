library(tidyverse)
library(ggrepel)
library(knitr)

movement <- read_csv(
  "pitch_movement-3.csv",
  show_col_types = FALSE
)

arsenal <- read_csv(
  "pitch-arsenal-stats-2(1).csv",
  show_col_types = FALSE
)

movement_all_ff <- movement %>%
  filter(
    year == 2021,
    pitch_type == "FF"
  ) %>%
  mutate(
    player = `last_name, first_name`
  )

handed_ivb_averages <- movement_all_ff %>%
  group_by(pitch_hand) %>%
  summarise(
    avg_hand_ivb = mean(
      pitcher_break_z_induced,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

movement_ff <- movement_all_ff %>%
  filter(
    pitches_thrown >= 500
  ) %>%
  left_join(
    handed_ivb_averages,
    by = "pitch_hand"
  ) %>%
  mutate(
    ivb_vs_avg =
      pitcher_break_z_induced -
      avg_hand_ivb,
    
    hb_vs_comparable =
      diff_x,
  )

arsenal_ff <- arsenal %>%
  filter(
    pitch_type == "FF"
  ) %>%
  distinct(
    player_id,
    .keep_all = TRUE
  ) %>%
  transmute(
    player_id,
    arsenal_player =
      `last_name, first_name`,
    run_value_per_100,
    run_value,
    pitches,
    pitch_usage,
    pa,
    ba,
    slg,
    woba,
    whiff_percent,
    k_percent,
    put_away,
    est_ba,
    est_slg,
    est_woba,
    hard_hit_percent
  )

league_fastballs <- movement_ff %>%
  left_join(
    arsenal_ff,
    by = c(
      "pitcher_id" = "player_id"
    )
  )

missing_arsenal <- league_fastballs %>%
  filter(
    is.na(arsenal_player)
  )

if (nrow(missing_arsenal) > 0) {
  stop(
    paste(
      "Missing arsenal data for:",
      paste(
        missing_arsenal$player,
        collapse = ", "
      )
    )
  )
}

velocity_fastballs <-
  league_fastballs %>%
  arrange(
    desc(avg_speed)
  ) %>%
  slice_head(n = 15) %>%
  mutate(
    profile_rank = row_number()
  )

ride_fastballs <-
  league_fastballs %>%
  arrange(
    desc(ivb_vs_avg)
  ) %>%
  slice_head(n = 15) %>%
  mutate(
    profile_rank = row_number()
  )

run_fastballs <-
  league_fastballs %>%
  arrange(
    desc(hb_vs_comparable)
  ) %>%
  slice_head(n = 15) %>%
  mutate(
    profile_rank = row_number()
  )

cut_fastballs <-
  league_fastballs %>%
  arrange(
    hb_vs_comparable
  ) %>%
  slice_head(n = 15) %>%
  mutate(
    profile_rank = row_number()
  )



velocity_color <- "#D55E00"
ride_color <- "#0072B2"
run_color <- "#009E73"
cut_color <- "#CC79A7"


fastball_theme <- function() {
  
  theme_minimal(
    base_size = 13
  ) +
    
    theme(
      plot.title =
        element_text(
          face = "bold",
          size = 17
        ),
      
      plot.subtitle =
        element_text(
          size = 11,
          color = "#555555",
          margin = margin(
            b = 12
          )
        ),
      
      axis.title =
        element_text(
          face = "bold"
        ),
      
      panel.grid.major.y =
        element_blank(),
      
      panel.grid.minor =
        element_blank(),
      
      plot.margin =
        margin(
          10,
          40,
          10,
          10
        )
    )
}

order_players <- function(
    data,
    metric,
    direction = "high"
) {
  
  if (direction == "high") {
    
    levels_order <-
      data$player[
        order(
          data[[metric]]
        )
      ]
    
  } else {
    
    levels_order <-
      data$player[
        order(
          data[[metric]],
          decreasing = TRUE
        )
      ]
  }
  
  data %>%
    mutate(
      player_plot =
        factor(
          player,
          levels = levels_order
        )
    )
}

plot_trait <- function(
    data,
    metric,
    title,
    subtitle,
    y_label,
    color,
    direction = "high",
    accuracy = 0.1
) {
  
  d <- order_players(
    data,
    metric,
    direction
  ) %>%
    
    mutate(
      label_value =
        scales::number(
          .data[[metric]],
          accuracy = accuracy
        )
    )
  
  ggplot(
    d,
    aes(
      x = player_plot,
      y = .data[[metric]]
    )
  ) +
    
    geom_hline(
      yintercept = 0,
      color = "#BBBBBB",
      linewidth = 0.7
    ) +
    
    geom_col(
      fill = color,
      width = 0.72
    ) +
    
    geom_text(
      aes(
        label = label_value
      ),
      hjust = -0.15,
      size = 3.3
    ) +
    
    coord_flip(
      clip = "off"
    ) +
    
    scale_y_continuous(
      expand = expansion(
        mult = c(
          0.10,
          0.16
        )
      )
    ) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = y_label
    ) +
    
    fastball_theme()
}

plot_movement <- function(
    selected,
    title,
    color,
    flat_reference = FALSE
) {
  
  p <- ggplot(
    league_fastballs,
    aes(
      x = pitcher_break_x,
      y = pitcher_break_z_induced
    )
  ) +
    
    geom_point(
      color = "#D0D0D0",
      size = 2,
      alpha = 0.65
    )
  
  if (flat_reference) {
    
    avg_hb <-
      mean(
        league_fastballs$
          pitcher_break_x,
        na.rm = TRUE
      )
    
    avg_ivb <-
      mean(
        league_fastballs$
          pitcher_break_z_induced,
        na.rm = TRUE
      )
    
    p <- p +
      
      geom_vline(
        xintercept = avg_hb,
        linetype = "dashed",
        color = "#999999"
      ) +
      
      geom_hline(
        yintercept = avg_ivb,
        linetype = "dashed",
        color = "#999999"
      )
  }
  
  p +
    
    geom_point(
      data = selected,
      color = color,
      size = 3.7
    ) +
    
    geom_text_repel(
      data = selected,
      aes(
        label = player
      ),
      size = 3,
      max.overlaps = Inf,
      box.padding = 0.4,
      point.padding = 0.25
    ) +
    
    labs(
      title = title,
      subtitle =
        "Highlighted pitchers are the selected top-15 profile",
      x =
        "Horizontal Break (inches)",
      y =
        "Induced Vertical Break (inches)"
    ) +
    
    fastball_theme()
}

plot_profile_space <- function(
    selected,
    title,
    color
) {
  
  ggplot(
    league_fastballs,
    aes(
      x = hb_vs_comparable,
      y = ivb_vs_avg
    )
  ) +
    
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "#AAAAAA"
    ) +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "#AAAAAA"
    ) +
    
    geom_point(
      color = "#D0D0D0",
      size = 2,
      alpha = 0.65
    ) +
    
    geom_point(
      data = selected,
      color = color,
      size = 3.7
    ) +
    
    geom_text_repel(
      data = selected,
      aes(
        label = player
      ),
      size = 3,
      max.overlaps = Inf
    ) +
    
    labs(
      title = title,
      subtitle =
        "0 represents average IVB and comparable horizontal movement",
      x =
        "Horizontal Break vs Comparable (inches)",
      y =
        "IVB vs Average (inches)"
    ) +
    
    fastball_theme()
}

plot_relationship <- function(
    selected,
    metric,
    title,
    x_label,
    color
) {
  
  d <- league_fastballs %>%
    filter(
      !is.na(est_woba),
      !is.na(
        .data[[metric]]
      )
    )
  
  ggplot(
    d,
    aes(
      x = .data[[metric]],
      y = est_woba
    )
  ) +
    
    geom_point(
      color = "#CCCCCC",
      alpha = 0.7,
      size = 2
    ) +
    
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "#777777",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    geom_point(
      data = selected,
      color = color,
      size = 3.7
    ) +
    
    geom_text_repel(
      data = selected,
      aes(
        label = player
      ),
      size = 3,
      max.overlaps = Inf
    ) +
    
    labs(
      title = title,
      subtitle =
        "Highlighted pitchers are the selected profile; lower xwOBA is better",
      x = x_label,
      y = "Expected wOBA"
    ) +
    
    fastball_theme()
}

plot_outcome <- function(
    data,
    metric,
    title,
    y_label,
    color,
    direction = "high",
    percent = FALSE,
    accuracy = 0.1
) {
  
  d <- data %>%
    filter(
      !is.na(
        .data[[metric]]
      )
    )
  
  d <- order_players(
    d,
    metric,
    direction
  )
  
  if (percent) {
    
    d <- d %>%
      mutate(
        label_value =
          paste0(
            scales::number(
              .data[[metric]],
              accuracy = accuracy
            ),
            "%"
          )
      )
    
  } else {
    
    d <- d %>%
      mutate(
        label_value =
          scales::number(
            .data[[metric]],
            accuracy = accuracy
          )
      )
  }
  
  ggplot(
    d,
    aes(
      x = player_plot,
      y = .data[[metric]]
    )
  ) +
    
    geom_segment(
      aes(
        xend = player_plot,
        y = 0,
        yend = .data[[metric]]
      ),
      color = "#D5D5D5",
      linewidth = 0.8
    ) +
    
    geom_point(
      color = color,
      size = 3.7
    ) +
    
    geom_text(
      aes(
        label = label_value
      ),
      hjust = -0.25,
      size = 3.2
    ) +
    
    coord_flip(
      clip = "off"
    ) +
    
    scale_y_continuous(
      expand = expansion(
        mult = c(
          0.15,
          0.20
        )
      )
    ) +
    
    labs(
      title = title,
      x = NULL,
      y = y_label
    ) +
    
    fastball_theme()
}

make_stats_table <- function(
    data,
    caption
) {
  
  data %>%
    
    arrange(
      profile_rank
    ) %>%
    
    transmute(
      Rank =
        profile_rank,
      
      Pitcher =
        player,
      
      Hand =
        pitch_hand,
      
      Velo =
        round(
          avg_speed,
          1
        ),
      
      IVB =
        round(
          pitcher_break_z_induced,
          1
        ),
      
      `IVB vs Avg` =
        round(
          ivb_vs_avg,
          1
        ),
      
      HB =
        round(
          pitcher_break_x,
          1
        ),
      
      `HB vs Comparable` =
        round(
          hb_vs_comparable,
          1
        ),
      
      xwOBA =
        sprintf(
          "%.3f",
          est_woba
        ),
      
      `Whiff %` =
        paste0(
          round(
            whiff_percent,
            1
          ),
          "%"
        ),
      
      `RV/100` =
        round(
          run_value_per_100,
          1
        ),
      
      `Hard-Hit %` =
        paste0(
          round(
            hard_hit_percent,
            1
          ),
          "%"
        ),
      
      xSLG =
        sprintf(
          "%.3f",
          est_slg
        ),
      
      `Usage %` =
        paste0(
          round(
            pitch_usage,
            1
          ),
          "%"
        )
    ) %>%
    
    knitr::kable(
      caption = caption
    )
}

make_page <- function(
    data,
    trait,
    trait_title,
    trait_subtitle,
    trait_axis,
    relationship_title,
    color,
    trait_direction = "high"
) {
  
  list(
    
    leaderboard =
      plot_trait(
        data = data,
        metric = trait,
        title = trait_title,
        subtitle = trait_subtitle,
        y_label = trait_axis,
        color = color,
        direction = trait_direction
      ),
    
    movement =
      plot_movement(
        selected = data,
        title =
          paste0(
            trait_title,
            ": Actual Movement Profile"
          ),
        color = color
      ),
    
    profile_space =
      plot_profile_space(
        selected = data,
        title =
          paste0(
            trait_title,
            ": Movement Relative to Average"
          ),
        color = color
      ),
    
    relationship =
      plot_relationship(
        selected = data,
        metric = trait,
        title = relationship_title,
        x_label = trait_axis,
        color = color
      ),
    
    xwoba =
      plot_outcome(
        data = data,
        metric = "est_woba",
        title =
          "Expected wOBA Allowed",
        y_label =
          "Expected wOBA",
        color = color,
        direction = "low",
        accuracy = 0.001
      ),
    
    whiff =
      plot_outcome(
        data = data,
        metric =
          "whiff_percent",
        title =
          "Whiff Rate",
        y_label =
          "Whiff %",
        color = color,
        direction = "high",
        percent = TRUE
      ),
    
    run_value =
      plot_outcome(
        data = data,
        metric =
          "run_value_per_100",
        title =
          "Run Value per 100 Fastballs",
        y_label =
          "Run Value / 100",
        color = color,
        direction = "high"
      ),
    
    hard_hit =
      plot_outcome(
        data = data,
        metric =
          "hard_hit_percent",
        title =
          "Hard-Hit Rate",
        y_label =
          "Hard-Hit %",
        color = color,
        direction = "low",
        percent = TRUE
      )
  )
}

velocity_page <- make_page(
  data =
    velocity_fastballs,
  
  trait =
    "avg_speed",
  
  trait_title =
    "Highest-Velocity Four-Seam Fastballs",
  
  trait_subtitle =
    "Top 15 average velocities among pitchers with at least 500 four-seam fastballs",
  
  trait_axis =
    "Average Velocity (mph)",
  
  relationship_title =
    "Velocity vs Expected wOBA",
  
  color =
    velocity_color
)

ride_page <- make_page(
  data =
    ride_fastballs,
  
  trait =
    "ivb_vs_avg",
  
  trait_title =
    "Four-Seam Fastballs With the Most Ride",
  
  trait_subtitle =
    "Top 15 IVB values relative to the average four-seam fastball from the same side",
  
  trait_axis =
    "IVB vs Average (inches)",
  
  relationship_title =
    "IVB vs Average and Expected wOBA",
  
  color =
    ride_color
)

run_page <- make_page(
  data =
    run_fastballs,
  
  trait =
    "hb_vs_comparable",
  
  trait_title =
    "Four-Seam Fastballs With the Most Run",
  
  trait_subtitle =
    "Top 15 horizontal-break values relative to comparable fastballs",
  
  trait_axis =
    "HB vs Comparable (inches)",
  
  relationship_title =
    "Horizontal Run vs Expected wOBA",
  
  color =
    run_color
)

cut_page <- make_page(
  data =
    cut_fastballs,
  
  trait =
    "hb_vs_comparable",
  
  trait_title =
    "Four-Seam Fastballs With the Most Cut",
  
  trait_subtitle =
    "The 15 lowest horizontal-break values relative to comparable fastballs",
  
  trait_axis =
    "HB vs Comparable (inches)",
  
  relationship_title =
    "Horizontal Cut vs Expected wOBA",
  
  color =
    cut_color,
  
  trait_direction =
    "low"
)


profile_summary <-
  bind_rows(
    
    velocity_fastballs %>%
      mutate(
        Profile = "Velocity"
      ),
    
    ride_fastballs %>%
      mutate(
        Profile = "Ride"
      ),
    
    run_fastballs %>%
      mutate(
        Profile = "Run"
      ),
    
    cut_fastballs %>%
      mutate(
        Profile = "Cut"
      ),
    
  ) %>%
  
  group_by(
    Profile
  ) %>%
  
  summarise(
    Velo =
      mean(
        avg_speed,
        na.rm = TRUE
      ),
    
    IVB =
      mean(
        pitcher_break_z_induced,
        na.rm = TRUE
      ),
    
    IVB_vs_Avg =
      mean(
        ivb_vs_avg,
        na.rm = TRUE
      ),
    
    HB =
      mean(
        pitcher_break_x,
        na.rm = TRUE
      ),
    
    HB_vs_Comparable =
      mean(
        hb_vs_comparable,
        na.rm = TRUE
      ),
    
    xwOBA =
      mean(
        est_woba,
        na.rm = TRUE
      ),
    
    Whiff =
      mean(
        whiff_percent,
        na.rm = TRUE
      ),
    
    RV100 =
      mean(
        run_value_per_100,
        na.rm = TRUE
      ),
    
    HardHit =
      mean(
        hard_hit_percent,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

make_profile_summary_table <-
  function() {
    
    profile_summary %>%
      
      transmute(
        Profile,
        
        Velo =
          round(
            Velo,
            1
          ),
        
        IVB =
          round(
            IVB,
            1
          ),
        
        `IVB vs Avg` =
          round(
            IVB_vs_Avg,
            1
          ),
        
        HB =
          round(
            HB,
            1
          ),
        
        `HB vs Comparable` =
          round(
            HB_vs_Comparable,
            1
          ),
        
        xwOBA =
          sprintf(
            "%.3f",
            xwOBA
          ),
        
        `Whiff %` =
          paste0(
            round(
              Whiff,
              1
            ),
            "%"
          ),
        
        `RV/100` =
          round(
            RV100,
            2
          ),
        
        `Hard-Hit %` =
          paste0(
            round(
              HardHit,
              1
            ),
            "%"
          )
      ) %>%
      
      knitr::kable(
        caption =
          "Average results for each fastball profile"
      )
  }

plot_league_movement <- function() {
  
  ggplot(
    league_fastballs,
    aes(
      x =
        hb_vs_comparable,
      y =
        ivb_vs_avg
    )
  ) +
    
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "#999999"
    ) +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "#999999"
    ) +
    
    geom_point(
      size = 2.5,
      alpha = 0.7
    ) +
    
    labs(
      title =
        "The 2021 Four-Seam Fastball Landscape",
      
      subtitle =
        "Movement shown relative to average or comparable four-seam fastballs",
      
      x =
        "Horizontal Break vs Comparable (inches)",
      
      y =
        "IVB vs Average (inches)"
    ) +
    
    fastball_theme()
}

make_effectiveness_comparison <- function(data, profile_name) {
  
  comparison <- bind_rows(
    
    league_fastballs %>%
      summarise(
        Group = "Qualified Four-Seam Average",
        xwOBA = mean(est_woba, na.rm = TRUE),
        Whiff = mean(whiff_percent, na.rm = TRUE),
        RV100 = mean(run_value_per_100, na.rm = TRUE),
        HardHit = mean(hard_hit_percent, na.rm = TRUE),
        xSLG = mean(est_slg, na.rm = TRUE),
        K = mean(k_percent, na.rm = TRUE),
        PutAway = mean(put_away, na.rm = TRUE)
      ),
    
    data %>%
      summarise(
        Group = profile_name,
        xwOBA = mean(est_woba, na.rm = TRUE),
        Whiff = mean(whiff_percent, na.rm = TRUE),
        RV100 = mean(run_value_per_100, na.rm = TRUE),
        HardHit = mean(hard_hit_percent, na.rm = TRUE),
        xSLG = mean(est_slg, na.rm = TRUE),
        K = mean(k_percent, na.rm = TRUE),
        PutAway = mean(put_away, na.rm = TRUE)
      )
    
  ) %>%
    
    transmute(
      Group,
      xwOBA = sprintf("%.3f", xwOBA),
      `Whiff %` = paste0(round(Whiff, 1), "%"),
      `RV/100` = round(RV100, 2),
      `Hard-Hit %` = paste0(round(HardHit, 1), "%"),
      xSLG = sprintf("%.3f", xSLG),
      `K %` = paste0(round(K, 1), "%"),
      `Put-Away %` = paste0(round(PutAway, 1), "%")
    )
  
  knitr::kable(
    comparison,
    caption = paste0(
      profile_name,
      " vs Qualified Four-Seam Average"
    )
  )
}