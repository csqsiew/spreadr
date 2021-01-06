spreadr
=======

The notion of spreading activation is a prevalent metaphor in the cognitive sciences.
This package provides the tools for cognitive scientists and psychologists to conduct computer simulations that implement spreading activation in a network representation.
The algorithmic method implemented in *spreadr* subroutines follows the approach described in Vitevitch, Ercal, and Adagarla (2011, Frontiers), who viewed activation as a fixed cognitive resource that could spread among nodes that were connected to each other via edges or connections (i.e., a network).
See Vitevitch, M. S., Ercal, G., & Adagarla, B. (2011).

Installation
############

You can install the stable version via ::

  > install.packages("spreadr")

or the latest version (on the GitHub master branch) ::

  > remotes::install_github("csqsiew/spreadr")
  > # alternatively
  > # devtools::install_github("csqsiew/spreadr")

If you encounter which looks like ::

  Error: (converted from warning) package 'Rcpp' was built under R version x.y.z

try setting ``Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")`` first [#]_.

Development Notes
#################

Testing the semantic priming simulation
***************************************

By default, running the test suite will replicate most of the simulations in the article describing *spreadr* [#]_ except for the last, *"Simulation Study 3: Semantic priming"*.
That simulation requires 800 iterations each of taking a few minutes, making it impractical for testing.

However, there is an option to include the replication of the first iteration of that simulation, with ``retention = 0.8``.
Simply set the environment variable ``TEST_SEMANTIC_PRIMING`` to any non-empty string.
For example, within an interactive R session: ::

   > Sys.setenv(TEST_SEMANTIC_PRIMING="true")
   > devtools::test()

Or, through ``R CMD``: ::

   $ cd ..
   $ R CMD build spreadr
   $ TEST_SEMANTIC_PRIMING=true R CMD check spreadr_x.y.z.tar.gz

replacing ``x.y.z`` with the version string.

Invalid ELF header
******************

If you encounter an "invalid ELF header" error, as in ::

  > devtools::load_all()
  Loading spreadr
  Error in dyn.load(dllfile) :
    unable to load shared object '/home/ning/github/spreadr/src/spreadr.so':
    /home/ning/github/spreadr/src/spreadr.so: invalid ELF header

Try calling ``devtools::clean_dll`` first [#]_.

.. [#] https://github.com/r-lib/remotes/issues/403#issuecomment-748181946
.. [#] https://doi.org/10.3758/s13428-018-1186-5
.. [#] https://github.com/r-lib/devtools/issues/2027#issuecomment-483691800
