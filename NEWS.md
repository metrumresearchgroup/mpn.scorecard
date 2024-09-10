# mpn.scorecard 0.5.0

## New features and change

 - Added support for externally scored packages. `render_scorecard()` can now
   render a scorecard from a set of files created by an external scorer (i.e.
   something other than `score_pkg()`). Note that they do not need to be `R`
   packages (#70).
 - Exports in the traceability matrix that spanned more than one page (usually 
  due to linking to many test files), are now split into <export> and 
  <export (cont.)> to ensure tables do not overflow into the footer (#73).
 - Expanded support for linking methods to their documentation and relevant test
   files (#71).

## Bug Fixes

 - Due to a few text wrapping bugs in the traceability matrix, some packages
   would fail to render a scorecard. (#66)
 - Other columns in the traceability matrix now indent when overflowing to the 
   next line (#71) 

# mpn.scorecard 0.4.1

## Bug Fixes

 - Due to a regression in the 0.4.0 release, an error was triggered when
   formatting the traceability matrix of a package with exports in its namespace
   that couldn't be mapped to a code file. (#65)

# mpn.scorecard 0.4.0

## New features and change

 - Various table formatting improvements. The `traceability matrix`, 
 `R dependency versions`, and `test coverage` tables are now striped (each row  
 alternates in color) to help distinguish between rows for potentially large 
 tables. (#58)

## Bug Fixes

 - Longer file paths and export names would occasionally overlap across columns.
 To address this, a text wrapping method was defined and additional cell padding
 was added. (#57)

# mpn.scorecard 0.3.0

## New features and changes

 - Renamed "Mitigation" section to "Comments" for added clarity. The term 
 "mitigation" may cause misinterpretations, suggesting that a package deviated
 from expected results. This section can be used for any kind of additional 
 explanatory notes. (#54)

# mpn.scorecard 0.2.1

## Bug Fixes

 - The template used by `render_scorecard_summary()` was incompatible with a Pandoc
 change in behavior introduced in version 3.1.7 and reverted in 3.1.10. (#51)

# mpn.scorecard 0.2.0

## New features and changes

 - Added new table to the appendix, displaying package dependency versions (#45).
 - `add_traceability` defaults to `'auto'` instead of `FALSE` in `render_scorecard()`. (#33)
 - New hex sticker (#48)
 
## Bug Fixes

 - Various traceability matrix fixes for capturing R exports and mapping to relevant tests and documentation. (#36, #42, #47)
 - Fixed a bug in `flextable_formatted()` where the number of rows was being appended to the overall scores and metadata tables. This was due to a change in `flextable`, and was introduced in version `0.9.3`: `as_flextable.data.frame()` always shows the number of rows even if less than 10. (#43)
 - Removed unused imports that were used during early development. (#44)

# mpn.scorecard 0.1.0

First official release

Exported functions:

 - `score_pkg`: Generate scorecard metrics for package
 - `render_scorecard`: Take a JSON from `score_pkg()` and render a pdf
 - `make_traceability_matrix`: Create a Traceability Matrix for the purpose of appending to a scorecard
 - `render_scorecard_summary`: Render PDF summary of scorecards
 - `summarize_package_results`: Review important package metrics prior to running `render_scorecard_summary()`
