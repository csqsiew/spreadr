spreadr
=======

Development
###########

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
