# Changelog

## spreadr 0.3.0

### bug fixes

- removed the `extrafont` dependency which caused package to be removed
  from CRAN in Sep 2025
- fixed (#7) issue - weighted graphs with negative edge weights will be
  thrown an error
- fixed (#4) issue - weighted igraph objects retain their weights in the
  converted adjacency matrix

### others

- a new hex logo and website added

## spreadr 0.2.0

CRAN release: 2021-05-11

### bug fixes

- fixed (#1) issue related to missing activation values at leaf nodes

### new features

- add activation at any time point
- specify retention values for each node
- decide whether to allow spreadr to run for a fixed number of time
  steps or choose a threshold to stop the process (threshold_to_stop)

### minor changes

- pre-pend activations at t=0 to output
- merged spreadr and spreadr2 functions (fixed time and threshold to
  stop)
