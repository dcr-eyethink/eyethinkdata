# Changelog

## eyethinkdata 1.0.0

### Documentation

- Rewrote the help pages for every function, with descriptions, full
  parameter lists, return values, and examples.
  [`resolve.args()`](https://dcr-eyethink.github.io/eyethinkdata/reference/resolve.args.md)
  now has a help page.

### Changes that may affect existing scripts

- [`gorilla_eye_processing()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_processing.md)
  is now the Task Builder 2 version (returns `td`, `ed` and `zd`). The
  previous version, which was the one actually in use, is available as
  [`gorilla_eye_processing_old()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_processing_old.md).
- [`bulk_file_rename()`](https://dcr-eyethink.github.io/eyethinkdata/reference/bulk_file_rename.md):
  `prefix` is now added to the start of file names and `suffix` to the
  end (they were swapped), once per file. It no longer changes the
  working directory, and will not overwrite an existing file.
- [`plotscat()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plotscat.md):
  `bayes_factor` now works for all plot types, and `BF=` only appears in
  the title if it was requested. `condcols` is now used. Results for
  group splits are named `sigyz1.*` and `sigyz2.*` (`sigz.p` and
  `sigz.bf` are kept for group 1).
- `outp = NULL` or `""` now means “do not save” in
  [`pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.md),
  [`plot_model()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plot_model.md),
  [`plotscat()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plotscat.md),
  [`exclorr()`](https://dcr-eyethink.github.io/eyethinkdata/reference/exclorr.md),
  [`gorilla_eye_plot()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_plot.md)
  and
  [`gorilla_iatanalysis()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_iatanalysis.md).
- [`gorilla_q_parse()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse.md)
  warns about an unrecognised `strip` value.

### Bug fixes

- [`mergey()`](https://dcr-eyethink.github.io/eyethinkdata/reference/mergey.md),
  [`exp_pid_merge()`](https://dcr-eyethink.github.io/eyethinkdata/reference/exp_pid_merge.md),
  [`gorilla_q_checkbox()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_checkbox.md),
  [`gorilla_q_performance_engagement()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_performance_engagement.md)
  and `gorilla_q_rankscore(keepcols = TRUE)` no longer fail with an
  error.
- [`gorilla_iatanalysis()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_iatanalysis.md)
  and
  [`plot_model()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plot_model.md)
  no longer leave output diverted to a file after an error.
- [`gorilla_q_parse()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse.md)
  and
  [`gorilla_q_parse_qb2()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse_qb2.md)
  no longer reuse the previous questionnaire’s table when one has no
  responses.
- `pirateye(pid_average = TRUE)` uses the `pid` column you name.
- [`readin_datalist()`](https://dcr-eyethink.github.io/eyethinkdata/reference/readin_datalist.md)
  works with several folders.
- [`gorilla_eye_plot()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_plot.md)
  declares its use of `grid` and saves the plot it draws.
- Declared missing dependencies (`scales`, `grid`; suggested
  `BayesFactor`, `clipr`, `fs`, `png`, `rdrop2`) and fixed the author
  field.
