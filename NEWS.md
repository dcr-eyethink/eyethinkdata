# eyethinkdata 1.0.0

## Documentation
* Rewrote the help pages for every function, with descriptions, full parameter
  lists, return values, and examples. `resolve.args()` now has a help page.

## Changes that may affect existing scripts
* `gorilla_eye_processing()` is now the Task Builder 2 version (returns `td`,
  `ed` and `zd`). The previous version, which was the one actually in use, is
  available as `gorilla_eye_processing_old()`.
* `bulk_file_rename()`: `prefix` is now added to the start of file names and
  `suffix` to the end (they were swapped), once per file. It no longer changes
  the working directory, and will not overwrite an existing file.
* `plotscat()`: `bayes_factor` now works for all plot types, and `BF=` only
  appears in the title if it was requested. `condcols` is now used. Results for
  group splits are named `sigyz1.*` and `sigyz2.*` (`sigz.p` and `sigz.bf` are
  kept for group 1).
* `outp = NULL` or `""` now means "do not save" in `pirateye()`, `plot_model()`,
  `plotscat()`, `exclorr()`, `gorilla_eye_plot()` and `gorilla_iatanalysis()`.
* `gorilla_q_parse()` warns about an unrecognised `strip` value.

## Bug fixes
* `mergey()`, `exp_pid_merge()`, `gorilla_q_checkbox()`,
  `gorilla_q_performance_engagement()` and `gorilla_q_rankscore(keepcols = TRUE)`
  no longer fail with an error.
* `gorilla_iatanalysis()` and `plot_model()` no longer leave output diverted to
  a file after an error.
* `gorilla_q_parse()` and `gorilla_q_parse_qb2()` no longer reuse the previous
  questionnaire's table when one has no responses.
* `pirateye(pid_average = TRUE)` uses the `pid` column you name.
* `readin_datalist()` works with several folders.
* `gorilla_eye_plot()` declares its use of `grid` and saves the plot it draws.
* Declared missing dependencies (`scales`, `grid`; suggested `BayesFactor`,
  `clipr`, `fs`, `png`, `rdrop2`) and fixed the author field.
