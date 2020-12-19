spreadr
=======

Development Notes
#################

Testing the semantic priming simulation
***************************************

By default, running the test suite will replicate most of the simulations in the article describing *spreadr* [#] except for the last, *"Simulation Study 3: Semantic priming"*.
That simulation requires 800 iterations each of taking a few minutes, making it impractical for testing.

However, there is an option to include the replication of the first iteration of that simulation, with ``retention = 0.8``.
Simply set the environment variable ``TEST_SEMANTIC_PRIMING`` to any non-empty string.
For example, within an interactive R session:

::
   > Sys.setenv(TEST_SEMANTIC_PRIMING="")
   > devtools::test()

Or, through ``R CMD``:

::
   $ cd ..
   $ R CMD build spreadr
   $ R CMD check spreadr_0.1.0.tar.gz

.. [#]: https://doi.org/10.3758/s13428-018-1186-5

Invalid ELF header
******************

If you encounter an "invalid ELF header" error, like ::

  > devtools::load_all()
  Loading spreadr
  Error in dyn.load(dllfile) :
    unable to load shared object '/home/ning/github/spreadr/src/spreadr.so':
    /home/ning/github/spreadr/src/spreadr.so: invalid ELF header

Try calling ``devtools::clean_dll`` first [#]_.

.. [#] https://github.com/r-lib/devtools/issues/2027#issuecomment-483691800
