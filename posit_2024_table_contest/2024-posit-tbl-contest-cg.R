# Code to generate the table for the 2024 Posit Table Contest
# More details and documentation found in this Quarto Document
# https://cjg.quarto.pub/bless-your-chart/posts/byc_posit_tbl_contest/table-contest-cg.html

# this is an expensive query, so going to save the ids as a vector
# below is the code to fetch using baseballr
# acc_teams <- baseballr::ncaa_teams(year = 2024, division = 1) |> 
#              dplyr::filter(conference == "ACC")

# acc_team_ids <-  as.numeric(acc_teams$team_id)

acc_team_ids <- c(67, 147, 193, 234, 255, 367, 415, 490, 457, 513, 545, 746, 742, 749)

# function to scrape schedule for league games only
acc_results <- function(id) {
  baseballr::ncaa_schedule_info(team_id = id, year = 2024) |>
    dplyr::filter(home_team_conference == "ACC" &
                    away_team_conference == "ACC") |>
    dplyr::filter(!is.na(home_team_score))
}

# grab acc only games
acc_scores <- lapply(acc_team_ids, acc_results)

# fix an error in how UVA are spelled with spaces
acc_results_2024 <- as.data.frame(do.call(rbind, acc_scores)) |>
  dplyr::distinct(contest_id, .keep_all = TRUE) |>
  dplyr::mutate(away_team = dplyr::if_else(away_team == "Virginia ", "Virginia", away_team))


# find home run differential by team 
home_diffs <- acc_results_2024 |>
  dplyr::mutate(h_diff = home_team_score - away_team_score) |>
  dplyr::group_by(home_team) |>
  dplyr::summarize(home_diff = sum(h_diff)) |>
  dplyr::rename(team = home_team)

# find away run differential by team 
away_diffs <- acc_results_2024 |>
  dplyr::mutate(a_diff = away_team_score - home_team_score) |>
  dplyr::group_by(away_team) |>
  dplyr::summarize(away_diff = sum(a_diff)) |>
  dplyr::rename(team = away_team)

# combine the tables for differentials 
full_diffs <- merge(home_diffs, away_diffs, by = "team")|>
  dplyr::mutate(full_diff = (home_diff + away_diff))|>
  dplyr::select(team, full_diff, home_diff, away_diff)

# find home conference records by team
home_results <- acc_results_2024 |>
  dplyr::mutate(h_result = dplyr::if_else(home_team_score > away_team_score, "W", "L")) |>
  dplyr::group_by(home_team, h_result) |>
  dplyr::count() |>
  tidyr::pivot_wider( 
    names_from = h_result,
    values_from = n)|>
  dplyr::rename(team = home_team, h_w = W, h_l = L)

# find away conference records by team
# some teams have not one a game on the road, so we need to replace NAs with 0s
away_results <- acc_results_2024 |>
  dplyr::mutate(a_result = dplyr::if_else(away_team_score > home_team_score, "W", "L")) |>
  dplyr::group_by(away_team, a_result) |>
  dplyr::count() |>
  tidyr::pivot_wider( 
    names_from = a_result,
    values_from = n) |>
  dplyr::rename(team = away_team, a_w = W, a_l = L) |> 
  dplyr::mutate(across(everything(), ~ replace(.x, is.na(.x), 0))) 

# join the results to compile them together
full_recs <- merge(home_results, away_results, by = "team") |>
  dplyr::mutate(W = (h_w + a_w), L = (h_l + a_l))|>
  dplyr::select(team, W, L, h_w, h_l, a_w, a_l)

# add in the win percentage overall for the league records 
diffs_recs <- merge(full_diffs, full_recs, by ="team") |> 
  dplyr::mutate(win_pct = round(W / (W + L), digits = 3)) |> 
  dplyr::arrange(-win_pct)


# function to get RPI data 
rpi_ranks <- function(url) {
  
  rpi_page <- rvest::read_html(url)
  
  rpi_rk <- rpi_page |> 
    rvest::html_nodes("table") |> 
    rvest::html_table(fill = TRUE)
  
  rpi_table <- as.data.frame(rpi_rk) 
  
  rpi_table <- rpi_table |>
    dplyr::mutate(record = stringr::str_split(Record, "-", simplify = T),
                  wins = record[,1],
                  losses = record[,2]
    )
}

rpi <- rpi_ranks(url = "https://www.ncaa.com/rankings/baseball/d1/rpi")

# make rpi table
acc_rpi <- rpi |>
  dplyr::filter(Conference == "ACC") |>
  dplyr::select(School, Rank, wins, losses) |>
  dplyr::rename(team = School, rpi = Rank, o_w = wins, o_l = losses)




# add pools play data from ACC.com 
pool_play <- tibble::tribble(
  ~team,~pool,
  "North Carolina","Pool A",
  "Wake Forest","Pool A",
  "Pittsburgh","Pool A", 
  "Boston College","Did not qualify",
  "Notre Dame","Did not qualify",
  "Clemson","Pool B",
  "Louisville","Pool B",
  "Miami (FL)","Pool B",
  "NC State","Pool C",
  "Duke","Pool C",
  "Virginia Tech","Pool C",
  "Virginia","Pool D",
  "Florida St.","Pool D",
  "Georgia Tech","Pool D"
) 

total_records <- merge(acc_rpi, diffs_recs, by = "team") |>
  dplyr::left_join(pool_play, by = "team") |>
  dplyr::mutate(
    overall = paste0(o_w, "-", o_l, " | ", "RPI:", rpi),
    acc_rec = paste0(W, "-", L),
    home_rec = paste0(h_w, "-", h_l),
    away_rec = paste0(a_w, "-", a_l),
    
  ) |>
  dplyr::select(
    team,
    overall,
    pool,
    acc_rec,
    win_pct,
    full_diff,
    home_rec,
    home_diff,
    away_rec,
    away_diff
  ) 



# create a custom GT theme
gt_theme_chris <- function(gt_object, ...) {
  stopifnot(
    "'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object)
  )
  
  gt_object |>
    gt::tab_options(
      heading.align = "left",
      table.border.top.style = "none",
      table.border.bottom.color = "black",
      table.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      table_body.border.top.style = "black",
      table_body.border.bottom.color = "black",
      heading.border.bottom.style = "none",
      data_row.padding = gt::px(7),
      column_labels.font.size = gt::px(14),
      column_labels.border.bottom.width = gt::px(2),
      ...
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        color = "#333333",
        font = gt::google_font("Roboto"),
        transform = "uppercase"
      ),
      locations = list(gt::cells_column_labels(),
                       gt::cells_stubhead())
    ) |>
    gt::tab_style(
      style = gt::cell_text(font = gt::google_font("Roboto"),
                            weight = 800),
      locations = gt::cells_title(groups = "title")
    ) |>
    gt::tab_style(
      style = gt::cell_text(font = gt::google_font("Roboto"),
                            weight = 400),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      locations = gt::cells_title(groups = "subtitle"),
      style = gt::cell_text(size = "small")
    ) |>
    gt::tab_style(
      locations = gt::cells_title(groups = "title"),
      style = gt::cell_text(size = "x-large")
    ) |>
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(size = "x-small")
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        color = 'black',
        weight = gt::px(1.5),
        style = 'solid'
      ),
      locations = gt::cells_body(rows = gt::everything())
    )  |>
    gt::tab_style(
      locations = gt::cells_row_groups(groups = gt::everything()),
      style = list(
        gt::cell_text(size = "medium",
                      weight = "bold"),
        gt::cell_fill(color = "#FFFEE0")
      )
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#FFFEE0"),
        gt::cell_text(
          color = "#333333",
          size = gt::px(16),
          weight = "bold"
        ),
        gt::cell_borders(
          sides = "all",
          color = "black",
          weight = gt::px(2)
        )
      ),
      locations = list(gt::cells_column_spanners(spanners = gt::everything()))
    )
} 

# now build the table 
library(cbbplotR) # necessary to add logos

total_records |>
  cbbplotR::gt_cbb_teams(team, team) |>
  dplyr::arrange(-win_pct, -full_diff) |>
  dplyr::mutate(row_number = 1:dplyr::n()) |>
  dplyr::relocate(row_number, .before = team) |>
  gt::gt() |>
  gt::fmt_markdown(team) |>
  gtExtras::gt_merge_stack(
    col1 = team,
    col2 = overall,
    palette = c("black", "#333333"),
    small_cap = FALSE,
    font_weight = c("normal", "bold")
  ) |>
  gt::cols_label(
    # rename columns
    row_number = "",
    team = "",
    overall = "",
    pool = "",
    acc_rec = "W-L",
    win_pct = "Win %",
    full_diff = "+/-",
    home_rec = "W-L",
    home_diff = "+/-",
    away_rec = "W-L",
    away_diff = "+/-",
  ) |>
  gt::tab_spanner(label = "Tournament Seeds & Pools",
                  columns = c(row_number, team, pool)) |>
  gt::tab_spanner(label = "Overall",
                  columns = c(acc_rec, win_pct, full_diff)) |>
  gt::tab_spanner(label = "Home",
                  columns = c(home_rec, home_diff)) |>
  gt::tab_spanner(label = "Away",
                  columns = c(away_rec, away_diff)) |>
  gt::fmt(
    columns = c(full_diff, home_diff, away_diff),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) |>
  gtExtras::gt_hulk_col_numeric(columns = c(win_pct, full_diff, away_diff, home_diff)) |>
  gt::tab_header(title = "2024 ACC Baseball Regular Season Standings",
                 subtitle = "Shows regular season standings used to determine tournament seeding and pools, plus RPI, overall record, and league records by location with run differentials.") |>
  gt::tab_source_note(
    source_note = gt::html(
      "<b>Top 12 teams qualify for ACC Tournament in Charlotte. Pool-play format, winner of each pool plays in a four-team, single-elimination bracket to determine champion. </b><br>
                                             <hr>Table by Chris (@dadgumboxscores) + Bless your chart | data via baseballr and ncaa.org through May 18 games"
    )
  ) |>
  gt_theme_chris() |>
  gt::cols_align(align = "left", columns = c(team)) |>
  gt::cols_align(align = "center", columns = gt::ends_with("rec")) |>
  gtExtras::gt_add_divider(
    columns = c(pool, full_diff, home_diff, away_diff),
    sides = "right",
    color = "black"
  ) |>
  gtExtras::gt_add_divider(
    columns = c(row_number),
    sides = "left",
    color = "black"
  ) |>
  gtExtras::gt_add_divider(
    columns = c(win_pct, full_diff, home_diff, away_diff),
    sides = "left",
    style = "solid",
    weight = gt::px(1),
    color = "grey"
  ) |>
  gt::tab_style(
    style = list(gt::cell_text(
      style = "oblique",
      size = "x-small",
      color = "grey"
    )),
    locations = gt::cells_body(columns = row_number,
                               rows = row_number < 13)
  ) |>
  gt::tab_style(
    style = list(
      gt::cell_text(
        size = "x-small"
      ),
      gt::cell_fill(color = "#fff1f3")
    ),
    locations = gt::cells_body(
      columns = c(row_number, team, pool),
      rows = c(13, 14)
    )
  ) |>
  gt::tab_style(
    style = list(
      gt::cell_text(
        color = "#91bfdb",
        style = "italic",
        size = "small",
        weight = "bolder"
      )
    ),
    locations = gt::cells_body(
      columns = c(row_number, pool),
      rows = c(1, 8, 12)
    )
  ) |>
  gt::tab_style(
    style = list(
      gt::cell_text(
        color = "#fdae61",
        style = "italic",
        size = "small",
        weight = "bolder"
      )
    ),
    locations = gt::cells_body(
      columns = c(row_number, pool),
      rows = c(2, 7, 11)
    )
  ) |>
  gt::tab_style(
    style = list(
      gt::cell_text(
        color = "#2c7bb6",
        style = "italic",
        size = "small",
        weight = "bolder"
      )
    ),
    locations = gt::cells_body(
      columns = c(row_number, pool),
      rows = c(3, 6, 10)
    )
  ) |>
  gt::tab_style(
    style = list(
      gt::cell_text(
        color = "#d7191c",
        style = "italic",
        size = "small",
        weight = "bolder"
      )
    ),
    locations = gt::cells_body(columns = c(row_number, pool),
                               rows = c(4, 5, 9))
  ) -> acc_tbl

acc_tbl

