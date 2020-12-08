# projectable (development version)

* Implemented a `col_row` class and associated `col_row()` function to allow 
  special handling of dataframe columns representing row labels. (#14).
* Reimplemented the `col_binomial()` function using `asbio::ci.p()` primarily
  to allow the user to select a parameter estimation method, and added a `note`
  field to the `col_binomial` class to store information on how the parameters
  were estimated. (#8, #9).
* Made `projectable_col` class extensible by introducing the `new_col()` 
  function. (#16).
* Added a `NEWS.md` file to track changes to the package.
