# projectable 0.0.6

* Deprecated `spec_col_freq()`. This function was implemented in a fairly unsafe way. It is being dropped without deprecation since it is purely a convenience function, the usefulness of which is limited anyway.

* `prj_gt()` and `prj_flex()` now attempt to add `gt` and `flextable` (respectively) to the path and throw an error if this cannot be done. 

# projectable 0.0.5

* Fixed bug in `col_binomial()` that prevented it from accepting `NA` as an argument to `n` or `N`.

# projectable 0.0.4

* Limited confidence intervals in `col_binomial()` to the range [0, 1] (#27). Previously, it was possible to observe `ci_lower < 0` or `ci_upper > 1` when computing confidence intervals with a finite population correction. However, such values are not sensible. Now, `col_binomial()` will force such values to 0 or 1.

# projectable 0.0.3
* Fixed bug in `col_binomial()` that was turning FPC off.

# projectable 0.0.2

* Implemented `new_col` function to allow users to make their own `col` objects (#16).
* Added rounding to projections (#11).
* Add `weight` argument to `col_freq` to allow for weighted frequencies.
* Implemented `prj_flextable()` for projecting as a `flextable` object (#12).
* Implemented a `col_row` class and associated `col_row()` function to allow 
  special handling of dataframe columns representing row labels. (#14).
* Reimplemented the `col_binomial()` function using `asbio::ci.p()` primarily
  to allow the user to select a parameter estimation method, and added a `note`
  field to the `col_binomial` class to store information on how the parameters
  were estimated. (#8, #9).
* Made `projectable_col` class extensible by introducing the `new_col()` 
  function. (#16).
* Added a `NEWS.md` file to track changes to the package.

# projectable 0.0.1

* Initial release
