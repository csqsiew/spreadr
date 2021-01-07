context("spreadr numeric vector retentions work well")

network <- graph_from_adjacency_matrix(matrix(
  c(0, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0 ,0,
    0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=9, byrow=TRUE))
retention <- c(0, 0, 0, 0, 0.5, 0, 0, 1, 0)
V(network)$name <- 1:9
start_run <- data.frame(node=as.factor(c(1, 4, 7)), activation=100, time=0)

test_that("error if length of retention numeric vector is wrong", {
  local_edition(3)  # expect_snapshot_error requires opt-into 3rd testthat ed.
  expect_snapshot_error(
    spreadr(network, start_run, time=5, retention=c(0.5, 0.7), include_t0=TRUE))
})

test_that("animated gif with per-node retention", {
  local_edition(3)

  layout <- layout_on_grid(network)
  results <- spreadr(
    network, start_run, time=5, retention=retention, include_t0=TRUE)
  results <- suppressMessages(left_join(
    results, data.frame(node=as.factor(1:9), x=layout[,1], y=layout[,2])))
  expect_snapshot(results)

  skip_if_not_installed("ggraph")
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gifski")  # gif renderer, requires cargo (as in rust)
  library(ggraph)
  library(gganimate)

  g <- ggraph(network, layout) +
    # Draw edges with arrows, end_cap is padding between arrow tip and node.
    geom_edge_link(arrow=arrow(), end_cap=circle(5, "mm")) +
    # Draw nodes, including information about activation for transition_manual
    geom_node_point(
      data=results, size=10, mapping=aes(x=x, y=y, colour=activation)) +
    # Label nodes, nudging a bit to the right for aesthetics
    geom_node_label(aes(label=name), nudge_x=.13) +
    # State the parameters of the simulation. Nodes 1, 2, 3 are at the bottom.
    # 7, 8, 9 at the top.
    annotate("text", x=0.1, y=2.1, hjust=0, label="retentions: c(0, 1, 0)") +
    annotate("text", x=0.1, y=1.1, hjust=0, label="retentions: c(0, 0.5, 0)") +
    annotate("text", x=0.1, y=0.1, hjust=0, label="retentions: c(0, 0, 0)") +
    # State time point in the title of the plot
    labs(title="t = {current_frame}") +
    transition_manual(time)

  # Silence side-effect prints and messages
  capture.output(
    suppressMessages(
      anim_save("per-node-retention.gif", g, renderer=gifski_renderer())))

  expect_snapshot_file("per-node-retention.gif")
})
