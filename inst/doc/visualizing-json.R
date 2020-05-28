## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
knitr::opts_chunk$set(fig.width = 7, fig.height = 5)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ---- message = FALSE---------------------------------------------------------
library(jsonlite)
library(dplyr)
library(purrr)
library(magrittr)
library(forcats)
library(ggplot2)
library(igraph)
library(RColorBrewer)
library(wordcloud)
library(viridis)
library(listviewer)
library(tidyjson)

set.seed(1)

## -----------------------------------------------------------------------------
co_length <- companies %>% json_complexity

## -----------------------------------------------------------------------------
co_length %>%
  ggplot(aes(complexity)) +
    geom_density() +
    scale_x_log10() +
    annotation_logticks(side = 'b')

## -----------------------------------------------------------------------------
co_examp_index <- which(co_length$complexity == 20L)[1]

co_examp <- companies[co_examp_index]

co_examp

## -----------------------------------------------------------------------------
co_examp %>% jsonedit(mode = "code")

## -----------------------------------------------------------------------------
co_struct <- companies %>% sample(5) %>% json_structure

print(co_struct)

## -----------------------------------------------------------------------------
co_names <- co_struct %>% 
  filter(type != "null" & !is.na(name)) %>%
  group_by(level, name, type) %>%
  summarize(ndoc = n_distinct(document.id))

co_names

## -----------------------------------------------------------------------------
co_names %$% wordcloud(name, ndoc, scale = c(1.5, .1), min.freq = 100)

## ---- fig.height = 9----------------------------------------------------------
co_names %>%
  ungroup %>%
  group_by(type) %>%
  arrange(desc(ndoc), level) %>%
  mutate(rank = 1:n()) %>%
  ggplot(aes(1, rank)) +
    geom_text(aes(label = name, color = ndoc)) +
    scale_y_reverse() +
    facet_grid(. ~ type) +
    theme_void() +
    theme(legend.position = "bottom") +
    scale_color_viridis(direction = -1)

## -----------------------------------------------------------------------------
# Plots an igraph visualization of a JSON document
#
# @param .x a JSON string or tbl_json object
# @param legend add a type color legend automatically
# @param vertex.size the size of the vertices
# @param edge.color the color for the edges
# @param edge.width the width of the edge lines
# @param show.labels should object names be shown
# @param plot should the plot be rendered?
# @param ... further arguments to igraph::plot.igraph
plot_json_graph <- function(.x, legend = TRUE, vertex.size = 6,
                            edge.color = 'grey70', edge.width = .5,
                            show.labels = TRUE, plot = TRUE,
                            ...) {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  if (nrow(.x) != 1) stop("nrow(.x) not equal to 1")

  structure <- .x %>% json_structure

  type_colors <- RColorBrewer::brewer.pal(6, "Accent")

  graph_edges <- structure %>%
    filter(!is.na(parent.id)) %>%
    select(parent.id, child.id)

  graph_vertices <- structure %>%
    transmute(child.id,
              vertex.color = type_colors[as.integer(type)],
              vertex.label = name)

  if (!show.labels)
    graph_vertices$vertex.label <- rep(NA_character_, nrow(graph_vertices))

  g <- igraph::graph_from_data_frame(graph_edges, vertices = graph_vertices,
                             directed = FALSE)

  if (plot) {
    op <- par(mar = c(0, 0, 0, 0))
    plt <- igraph::plot.igraph(g,
         vertex.color = igraph::V(g)$vertex.color,
         vertex.size  = vertex.size,
         vertex.label = igraph::V(g)$vertex.label,
         vertex.frame.color = NA,
         layout = layout_with_kk,
         edge.color = edge.color,
         edge.width = edge.width,
         ...)

    if (legend)
      legend(x = -1.3, y = -.6, levels(structure$type), pch = 21,
             col= "white", pt.bg = type_colors,
             pt.cex = 2, cex = .8, bty = "n", ncol = 1)

    par(op)
  }

  invisible(g)

}

## -----------------------------------------------------------------------------
'{"object" : {"name": 1},
  "array"  : ["a", "b"],
  "string" : "value", 
  "number" : 1, 
  "logical": true,
  "null"   : null}' %>% 
  plot_json_graph

## -----------------------------------------------------------------------------
co_examp %>% plot_json_graph

## ---- fig.height = 8----------------------------------------------------------
plot_json_graph_panel <- function(json, nrow, ncol, ...) {
  
  # Set up grid
  op <- par(mfrow = c(nrow, ncol))
  
  indices <- seq_along(json) %>% keep(`<=`, nrow * ncol)
  
  for (i in indices) {
    plot_json_graph(json[[i]], ...)
    if ("names" %in% names(attributes(json))) 
      title(names(json)[i], col.main = 'red')
  }
  
  par(op)
  invisible(NULL)
}

## ---- fig.height = 8----------------------------------------------------------
plot_json_graph_panel(companies %>% sample(5), 7, 6, legend = FALSE, show.labels = FALSE,
                      vertex.size = 4)

## -----------------------------------------------------------------------------
most_complex <- companies[which(co_length$complexity == max(co_length$complexity))]

most_complex_name <- most_complex %>% 
  spread_values(name = jstring(name)) %>% 
  extract2("name")

## -----------------------------------------------------------------------------
plot_json_graph(most_complex, show.labels = FALSE, vertex.size = 2)

## -----------------------------------------------------------------------------
most_complex %>% json_schema %>% jsonedit(mode = "code")

## -----------------------------------------------------------------------------
most_complex %>% json_schema(type = "value") %>% plot_json_graph

## -----------------------------------------------------------------------------
most_complex %>% gather_object %>% json_types %>% json_complexity %>%
  filter(type %in% c('array', 'object') & complexity >= 15) %>%
  split(.$name) %>%
  map(json_schema, type = "value") %>%
  plot_json_graph_panel(3, 3, legend = FALSE)

## -----------------------------------------------------------------------------
rounds <- companies %>%
  enter_object(funding_rounds) %>%
  gather_array %>%
  spread_values(
    round = jstring(round_code),
    currency = jstring(raised_currency_code),
    raised = jnumber(raised_amount)
  )
rounds %>% head

## -----------------------------------------------------------------------------
geos <- companies %>%
  enter_object(offices) %>%
  gather_array %>%
  spread_values(
    country = jstring(country_code),
    state = jstring(state_code),
    description = jstring(description)
  )
geos %>% head

## -----------------------------------------------------------------------------
hqs <- geos %>%
  filter(array.index == 1) %>%
  filter(country == "USA") %>%
  select(document.id, state)
  
rounds_usd <- rounds %>%
  filter(currency == "USD") %>%
  filter(!is.na(raised)) %>%
  select(document.id, round, raised)

rounds_by_geo <- inner_join(rounds_usd, hqs, by = "document.id") %>% as_tibble()

## -----------------------------------------------------------------------------
round_prep <- rounds_by_geo %>% 
  dplyr::filter(!is.na(state)) %>%
  mutate(
    round = round %>% forcats::fct_collapse(
      "angel" = c("seed", "angel"),
      "d-f"   = c("d", "e", "f"),
      "other" = c("grant", "partial", "post_ipo_equity", "private_equity", 
                  "debt_round", "unattributed")
    ) %>% forcats::fct_relevel("angel", "a", "b", "c", "d-f", "other")
  ) %>%
  mutate(
    state = state %>% forcats::fct_lump(2)
  ) 

g <- ggplot(round_prep, aes(state, raised, fill = state)) +
  geom_violin() +
  scale_y_log10() + 
  annotation_logticks(side = 'l') +
  facet_grid(. ~ round) +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Amount Raised (USD)")

g

