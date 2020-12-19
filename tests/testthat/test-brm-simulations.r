context("Simulations from Siew 2019 in Behavior Research Methods")
# Siew, Cynthia S. Q. 2019. ‘Spreadr: An R Package to Simulate Spreading
# Activation in a Network’. Behavior Research Methods 51 (2): 910–29.
# https://doi.org/10.3758/s13428-018-1186-5.

library(fs)

# We download the test datasets and expected output from spreadr's OSF
# repository at https://osf.io/a9bv6/. The downloaded files are excluded from
# the package tarball with .Rbuildignore.
setup({
  # Download files
  download_urls <- list(
    "ego2hopnets_24.RData" = "https://osf.io/download/vucnf/",
    "study1_rawoutput.zip" = "https://osf.io/download/vy42b/",
    "study2_rawoutput.zip" = "https://osf.io/download/6xzgq/",
    "24toynets.csv"        = "https://osf.io/download/9wz6s/")
  sapply(names(download_urls), function(fn) {
    if (!file_exists(fn)) download.file(download_urls[[fn]], fn)
  })
  # Extract zipped files
  zip_files <- Filter(function(fn) grepl("\\.zip$", fn), names(download_urls))
  sapply(zip_files, function(zfn) {
    if (!dir_exists(gsub("\\.zip$", "", zfn))) unzip(zfn)
  })
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
            "study1_rawoutput/%d_%s_%.1f.csv",
            i, v2011$name[i], retention),
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
            "study2_rawoutput/%d_%s_%.1f_sim2.csv",
            i, v2011$name[i], retention),
          colClasses=c("NULL", NA, NA, NA))  # ignore the first column
        expect_equal(results, expected)
      }
    )
  }
}
