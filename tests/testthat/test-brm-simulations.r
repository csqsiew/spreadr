# Here, we test simulations from:
# Siew, Cynthia S. Q. 2019. ‘Spreadr: An R Package to Simulate Spreading
# Activation in a Network’. Behavior Research Methods 51 (2): 910–29.
# https://doi.org/10.3758/s13428-018-1186-5.

library(fs)

# We download the test datasets and expected output from spreadr's OSF
# repository at https://osf.io/a9bv6/. The downloaded files are excluded from
# the package tarball with .Rbuildignore.
download_urls <- list(
  # Lexical retrieval simulation
  "ego2hopnets_24.RData"    = "https://osf.io/download/vucnf/",
  "study1_rawoutput.zip"    = "https://osf.io/download/vy42b/",
  "24toynets.csv"           = "https://osf.io/download/9wz6s/",
  # False memory simulation
  "study2_rawoutput.zip"    = "https://osf.io/download/6xzgq/",
  # Semantic priming simulation
  "usf.RData"               = "https://osf.io/download/gwyc8/",
  "spp_200_pairs.RData"     = "https://osf.io/download/h42ay/",
  # Because these simulations run for many iterations (800), each on a rather
  # large network (10k nodes, 63k edges), they will take a /very/ long time to
  # test. Instead, we prepare to test only the first pair of words, for one
  # retention value of 0.8, and we really run that test only if the user says
  # to do so, by specifying an environment variable TEST_SEMANTIC_PRIMING to
  # any non-empty string.
  # "spp_retention_0.2.RData" = "https://osf.io/download/yjt4g/",
  # "spp_retention_0.4.RData" = "https://osf.io/download/f48th/",
  # "spp_retention_0.6.RData" = "https://osf.io/download/uk6rz/",
  "spp_retention_0.8.RData" = "https://osf.io/download/ybq6u/")
# Download files
sapply(names(download_urls), function(fn) {
  if (!file_exists(fn)) download.file(download_urls[[fn]], fn, quiet=TRUE)
})
# Extract files
zip_files <- Filter(function(fn) grepl("\\.zip$", fn), names(download_urls))
sapply(zip_files, function(zfn) {
  if (!dir_exists(gsub("\\.zip$", "", zfn))) unzip(zfn)
})

load("ego2hopnets_24.RData")
v2011 <- read.csv('24toynets.csv')
retentions <- seq(0.1, 0.9, by=0.1)

for (retention in retentions) {
  for (i in 1:nrow(v2011)) {
    test_that(sprintf(
      "Lexical retrieval simulation: network %d, retention %f", i, retention), {
        results <- spreadr(
          start_run=data.frame(node=v2011$name[i], activation=100),
          network=ego2hopnets_24[[i]],
          decay=0, retention=retention, suppress=0, time=10)
        expected <- read.csv(
          sprintf(
            "study1_rawoutput/%d_%s_%.1f.csv", i, v2011$name[i], retention),
          stringsAsFactors=TRUE,
          colClasses=c("NULL", NA, NA, NA))  # ignore the first column
        expect_equal(results, expected)
      }
    )
    test_that(sprintf(
      "False memory simulation: network %d, retention %f", i, retention), {
        neighbour_nodes <- igraph::neighbors(
          ego2hopnets_24[[i]], as.character(v2011$name[i]))
        results <- spreadr(
          start_run=data.frame(
            node=as.vector(neighbour_nodes$name),
            activation=100/length(neighbour_nodes),
            stringsAsFactors=FALSE),
          network=ego2hopnets_24[[i]],
          decay=0, retention=retention, suppress=0, time=10)
        expected <- read.csv(
          sprintf(
            "study2_rawoutput/%d_%s_%.1f_sim2.csv", i, v2011$name[i], retention),
          stringsAsFactors=TRUE,
          colClasses=c("NULL", NA, NA, NA))  # ignore the first column
        expect_equal(results, expected)
      }
    )
  }
}

load("usf.RData")
usf <- igraph::simplify(usf)
igraph::E(usf)$weight <- 1
load("spp_200_pairs.RData")
load("spp_retention_0.8.RData")  # loaded into name x
spp_retention_0.8 <- x

test_that("Semantic priming simulation: pair 1, retention 0.8", {
  skip_if(
    Sys.getenv("TEST_SEMANTIC_PRIMING") == "",
    paste0(c(
      "This test takes a few minutes, during which it will use up all",
      "system memory on most personal computers and laptops. If you are sure",
      "you would like to run this test, set the environment variable",
      "TEST_SEMANTIC_PRIMING to any non-empty string."),
      collapse="\n"))
  start_run <- data.frame(
    node=spp_200_pairs$Prime[1], activation=100, stringsAsFactors=FALSE)
  result <- spreadr(
    start_run=start_run, network=usf,
    retention=0.8, time=10, suppress=0, decay=0)
  targets <- subset(result, node == spp_200_pairs$TargetWord[1])
  result_vector <- c(
    spp_200_pairs$Prime[1], spp_200_pairs$TargetWord[1], targets$activation)
  expected_vector <- spp_retention_0.8[1, ]
  expect_equal(result_vector, expected_vector)
  gc()
})
