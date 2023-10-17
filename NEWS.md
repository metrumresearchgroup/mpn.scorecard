
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
