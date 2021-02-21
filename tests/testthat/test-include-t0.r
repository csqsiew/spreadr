suppressPackageStartupMessages({library(igraph)})

network <- graph_from_adjacency_matrix(matrix(
  c(0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0), nrow=8, byrow=TRUE))
V(network)$name <- 1:8
start_run <- data.frame(
  node=as.factor(1),
  activation=100,
  time=c(0, 2, 4, 6))

test_that("error if include_t0 is not flag", {
  expect_snapshot_error(
    spreadr(network, start_run, time=32, retention=0.1, include_t0=42))
})

test_that("include_t0 works well", {
  layout <- layout_in_circle(network)
  results <- spreadr(
    network, start_run, time=16, retention=0.05, include_t0=TRUE)
  results <- suppressMessages(dplyr::left_join(
    results,
    data.frame(node=as.factor(1:8), x=layout[,1], y=layout[,2])))
  expect_snapshot(results)
})

test_that("include_t0 works well in animated GIF", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gifski")  # gif renderer, requires cargo (as in rust)
  skip_if(
    Sys.getenv("TEST_ANIMATED_GIF") == "",
    paste0(c(
      "The snapshot tests for animated GIFs may fail if you are on a different ",
      "development environment than the one which had generated and committed the ",
      "current GIF snapshots. To continue with this test, set the environment variable",
      "TEST_ANIMATED_GIF to any non-empty string. Then, feel free to overwrite and",
      "commit the current snapshot with the ones you have generated."),
      collapse="\n"))
  suppressPackageStartupMessages({
    library(ggraph)
    library(gganimate)})

  layout <- layout_in_circle(network)
  results <- spreadr(
    network, start_run, time=16, retention=0.05, include_t0=TRUE)
  results <- suppressMessages(dplyr::left_join(
    results,
    data.frame(node=as.factor(1:8), x=layout[,1], y=layout[,2])))

  g <- ggraph(network, layout) +
    # Draw edges with arrows, end_cap is padding between arrow tip and node.
    geom_edge_link(arrow=arrow(), end_cap=circle(5, "mm")) +
    # Draw nodes, including information about activation for transition_manual
    geom_node_point(
      data=results, size=10, mapping=aes(x=x, y=y, colour=activation)) +
    # Label nodes, nudging a bit to the right for aesthetics
    geom_node_label(aes(label=name), nudge_x=.13) +
    # State the parameters of the simulation
    annotate(
      "text", x=-0.65, y=0, hjust=0, label=paste0(
        "data.frame(\n  node=as.factor(1), activation=100, time=c(0,2,4,6))\n",
        "retention=0.05")) +
    # State time point in the title of the plot
    labs(title="t = {current_frame}") +
    transition_manual(time)

  # Silence side-effect prints and messages
  capture.output(
    suppressMessages(
      anim_save("include_t0.gif", g, renderer=gifski_renderer())))

  expect_snapshot_file("include_t0.gif")
})
