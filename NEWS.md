# BIOdry 0.9.5

- Fixed `frametoLme()`: residual series in `$fluc` could be attached to the
  wrong rows, because row labels were used as row positions when the residual
  frame was built. Residuals are now matched to model-data rows by label, and
  fitting stops with an error if any row cannot be matched. Model fits were not
  affected. The defect was introduced in development versions 0.9.2-0.9.4
  (April 2026); the CRAN release 0.9.1 matched rows with
  merge(..., by = 'row.names') and is not affected.
- Added a regression test checking that fitted plus residual equals log(x) for
  every row of `$fluc`.
- Fixed `wlai()`: with `label_year = "end"`, hydrological years were still
  labeled by the year in which they start, so October 2000 to September 2001
  came out as 2000 instead of 2001 and lag 0 pointed to the year before ring
  formation. The output now has a `year` column: with `"end"` it is the year in
  which each hydrological year ends, with `"start"` the year in which it
  starts. `hydro_year_id` keeps its meaning and is always the start
  year. `label_year` now also applies when the input already has a
  `hydro_year_id` column, as returned by `moveYr()`; if the input `year` column
  follows the other convention, `wlai()` warns and its own `label_year`
  decides. With calendar years `label_year` has no effect. `label_year` was
  added in development version 0.9.2; the CRAN release 0.9.1 is not affected.
- Fixed `frametoLme()`: when the input had more than one time column (for
  example `year` and the `time` column added by `rtimes()`) and the model used
  one other than the first detected, building `$fluc` stopped with
  "object 'year' not found". `$fluc` is now built on the time variable of the
  fitted fixed formula. Fits that use `year` are unchanged. The code that
  picked the first detected time column was introduced in development version
  0.9.2; versions 0.9 and 0.9.1 built `$fluc` differently.
- Fixed `shiftFrame()`: dot-separated wide column names were read in the
  wrong order, so `P16106.17.a` gave `plot = a` and `sample = P16106`, and
  climate columns such as `P16106.Apr` gave `plot = Apr` and
  `month = P16106`. Column names are read from the highest level to the
  lowest again, as in 0.9.1: `P16106.17.a` is core `a` of tree `17` in plot
  `P16106`. This also corrects `modelFrame()` on wide chronologies, which
  fitted the grouping structure on the swapped labels (for `Pchron`, 8 trees
  instead of 4). The defect was introduced in development version 0.9.2; the
  CRAN release 0.9.1 is not affected.
- Fixed the `moveYr()` then `wlai()` route through `ringApply()`: the start
  month recorded by `moveYr()` was lost when `ringApply()` combined the
  blocks, so `wlai()` ordered each hydrological year January to December
  instead of October to September and returned different aridity indexes
  (for `PTclim05`, P44005, October 1998 to September 1999: 1.3774 instead of
  1.5276). `ringApply()` now keeps the start month in the data's metadata and
  `wlai()` uses it, so this route agrees with `wlai(ini.mnt = "Oct")` on every
  block. The defect was introduced in development version 0.9.2; the CRAN
  release 0.9.1 is not affected.
- Changed the default of `label_year` in `wlai()` and `moveYr()` from
  `"start"` to `"end"`, so that by default each hydrological year is labeled
  by the year in which it ends (October 1998 to September 1999 is labeled
  1999), as in the CRAN release 0.9.1. `"start"` remains available.
  `hydro_year_id` still holds the start year.
- Fixed compared with the CRAN release 0.9.1: `scacum()` with a vector of
  reference radii in `sc.c` scaled some cores to another tree's radius (with
  `Prings05` and `Pradii03`, 6 of the 8 cores). Every core is now scaled to its
  own tree's radius at `rf.t`. Fixed since development version 0.9.2; added a
  regression test.
- Restored the default time variable of `tdForm()` to the behavior of the
  CRAN release 0.9.1: with `on.time = TRUE` (the default) the relative time
  column `time` from `rtimes()` is used when present, otherwise a year-like
  column. In development version 0.9.4 the default had switched to `year`
  whenever the data carried metadata, which `ringApply()` adds automatically
  (checked with the 0.9.2, 0.9.3 and 0.9.4 builds). This affects the
  `ringApply()` help route and `modelFrame(Pchron)`, which again fit
  `log(x) ~ log(csx) + time`. A time variable declared with
  `defineDataRoles(time = ...)`, `on.time = FALSE`, or a formula string still
  decides as before.
- Fixed the climate route of the `muleMan()` help example (`modelFrame()` on
  `Prec` and `Temp` with `moveYr()` and `wlai()`, then `lmeForm`) and
  `muleMan()` itself. The route failed in development versions 0.9.2 to 0.9.4
  (checked with their builds), and in later development code the
  aridity-index model used `hydro_year_id`, the start year, instead of the
  hydrological-year label `year`. `lmeForm()` now uses a time variable from
  the data's metadata only when it was declared with
  `defineDataRoles(time = ...)`, so the model is `AI ~ year` as in the CRAN
  release 0.9.1, and `muleMan()` pairs ring year t with the hydrological year
  ending in t. `muleMan()` also no longer fails when the tree-ring data have a
  single time column or when both inputs have a residual column of the same
  name.
- Fixed `muleMan()`: the plot and core labels of its output could be swapped
  (for `Pchron`, core `a` of tree `17` in plot `P16106` came out as
  `plot = a`, `sample = P16106`), because the labels were parsed from block
  names in an order that depended on the column order of the tree-ring data.
  Each correlogram now takes its labels from its own rows, whatever the
  column order. The CRAN release 0.9.1 labeled them correctly; in the
  0.9.2 to 0.9.4 builds the help-example route failed before reaching
  `muleMan()`.
- `plot()` now works on `frametoLme()` fits made with `res.data = FALSE`: the
  residual fluctuations are rebuilt from the model and show the same points
  as the plot of the same fit with `$fluc`. The plot method added in
  development version 0.9.4 stopped with "subscript out of bounds" on such
  fits, and earlier versions, including the CRAN release 0.9.1, had no plot
  method for them.
- Fixed `amod()`: the first increment of each series was the whole
  transformed size, which is wrong when the cumulative size is scaled (with
  `scacum()` and radii in `sc.c`). On the `ringApply()` help route, the first
  basal-area increment of core `a` of tree `17` in plot `P16106` (1897) was
  5545 instead of 183. The first increment is now the change from the size
  before the first ring (`csx` minus the first ring width) to the first
  transformed size; later increments and unscaled series are unchanged. The
  CRAN release 0.9.1 returned `NA` for first increments.
- Fixed `wlai()`: on long calendar data without `ini.mnt`, every 12-month
  block started at the month of the first row of `cd`, so the aridity indexes
  depended on the row order (shuffling the rows of `PTclim05` changed them by
  about 20% on average). Blocks now start at the first month of the earliest
  year. Without a year column the first row's month is still used, so rows
  should then be in time order. Results on time-ordered data are unchanged.
  The CRAN release 0.9.1 computed one index over all rows of long data.
- "health" is no longer guessed as a grouping column name.
- Fixed `levexp()`: its help call `levexp(Pradii03, Prings05)` returned an
  empty vector. The integer `year` column was taken as a grouping column, and
  a name with two levels such as `P16106.17` could not match a core label such
  as `a.17.P16106`. Time columns are no longer taken as grouping columns, and
  a name now matches a split label when its dot-separated parts are a
  contiguous run of the label's parts, read in either direction; each core now
  gets its own tree's radius. Single-part names match as before. The CRAN
  release 0.9.1 returned only the two cores of tree 7 in plot P44005.
- Fixed `ringApply()`: named arguments that vary by block, such as
  `sc.c = Pradii03`, were handed to the blocks by position, so their names were
  ignored and the result depended on the order of the vector's elements (with
  `rev(Pradii03)` every core of the help route was scaled to another tree's
  radius). A named vector whose names match the split labels is now given to
  each block by name through `levexp()`; other arguments are recycled over
  blocks as before. If the names match some blocks but not all, a warning
  names the argument and the unmatched blocks, and the values are given by
  position. The CRAN release 0.9.1 was also affected: it handed `sc.c` out by
  position, and with `rev(Pradii03)` 6 of the 8 cores got another tree's
  radius.
- Column roles are again decided by column type: factor columns are groups,
  as in the CRAN release 0.9.1 (`cClass()`), and character columns are now
  also treated as groups (0.9.1 ignored them); whole-number numeric columns
  are time candidates, and other numeric columns are responses. The month
  column (named `month`, or a factor whose levels are month names) is never a
  group. The development versions 0.9.2-0.9.4 guessed roles from column names
  (lists such as plot, tree, zone, site, year, hydro_year_id); those lists are
  removed. Roles declared with `defineDataRoles()` override the type rule;
  metadata that `ringApply()` creates automatically no longer does. On the
  bundled data the documented results are unchanged, except that `month` in
  `PTclim05` is no longer a grouping column (`lmeForm(PTclim05)` is
  `pr ~ year | plot`, and `slitFrame(PTclim05)` splits by plot only). Year can
  still be a random grouping level: declare it with
  `defineDataRoles(groups = ...)` or add a factor copy of it.
- `rtimes()`, `scacum()`, `moveYr()` and `wlai()` find their time columns by
  the package's column names: `year` (or `hydro_year_id` from `moveYr()`),
  `month` and `time`; a month column may also be a factor whose levels are
  month names. The development versions 0.9.2-0.9.4 also read aliases (`yr`,
  `calendar_year`, `hydro_year`, `water_year`, `mon`, `mnt`, `mo`) and other
  time units (week, day, hour, minute, rel_time, ring_time, cambial_age, age);
  these are no longer read. `moveYr()` stops if there is no column named
  `year`; `scacum()` stops if `rf.t` is given and there is no `year`,
  `hydro_year_id` or `time` column (it used to fall back silently to each
  block's last value); `rtimes()` and `wlai()` warn when none of `year`,
  `hydro_year_id`, `month` or `time` is present. The CRAN release 0.9.1 never
  had the aliases.
- `scacum()` reads its values from the column `x`, as in the CRAN release
  0.9.1; a response declared with `defineDataRoles()` takes precedence, and
  without either `scacum()` stops. `wlai()` reads precipitation from `pr` and
  temperature from `tm`, the package's column names; declared inputs take
  precedence (declared `pr` and `tm` by name, otherwise the declared order,
  precipitation first). Without `pr` or `tm`, `wlai()` uses the first two
  numeric columns and warns which ones; this fallback follows 0.9.1, which
  read columns 1 and 2 by position. The name lists added in development
  versions 0.9.2-0.9.4 (`trw`, `rw`, `rwi`, `abi`; `pm`, `prec`, `ppt`,
  `precip`, `rain`, `temp`, `tmean`, `tavg` and others) are removed.
- Help pages: `frametoLme()` and `modelFrame()` now say that rows with
  missing values in the fitted variables or grouping columns are dropped
  without a message (e.g. the rows where `rtimes()` gives no time), and
  describe the default random part and how to pass `random =`; `rtimes()`
  says which rows get NA time; `moveYr()` has a Value section explaining that
  the calendar year is relabeled and how to recover it.
- Fixed `frametoLme()`: when a column was both a grouping level and a time
  column (for example `year` declared in `groups` with `defineDataRoles()`),
  `$fluc` carried it twice, the second copy as `year.1`. It now carries it
  once.
- The `defineDataRoles()` example uses the bundled data's own columns (it
  named `zone` and `health`, which no bundled dataset has, and stopped with an
  error).
- Runnable examples on bundled data for `frametoLme()`, `summary()`, `print()`
  and the printed summary of `frametoLme` objects.
- Fixed the summary and print of `frametoLme()` and `modelFrame()` fits: the
  "Groups" line also listed nlme's counts for the model matrix and response
  (`X 1`, `y 1`); it now lists only the grouping levels.
- Fixed the printed summary of `frametoLme()` and `modelFrame()` fits: the
  fixed-effects table was printed in scientific notation, including the DF
  column (`9.8200e+02`); it is now formatted column by column as in nlme's
  summary, with whole DF and p-values rounded to 4 decimals.
- Runnable examples on bundled data for `modelFrame()` (fitted model,
  Friedman smoother and processed data only) and for `summary()`, `print()`
  and the printed summary of `modelFrame` objects.
- Fixed `frametoLme()`: refitting a fit's model data (`fit$model$data`) with
  `tdForm` or `lmeForm` reversed the nesting (`sample/tree/plot`), because the
  fitting copy listed the grouping columns in reverse. The copy now keeps the
  input's column order.
- Fixed `frametoLme()`: the model data stored in the fit (`fit$model$data`)
  kept only the time variable of the fitted model, so refitting a year model
  with `tdForm(log.t = TRUE)`, as in the BIOdry 0.8 example, silently used
  `log(year)` instead of `log(time)`. The model data now keep all time
  columns; rows are still dropped only for missing fitted variables and
  grouping columns, and `$fluc` is unchanged.
- The `frametoLme()` examples compare the two Zeide time forms (linear and
  logarithmic time, `log.t`) by maximum likelihood under a random part the
  bundled data support (a tree intercept and a tree-level `log(csx)` slope),
  refit the preferred form with REML, and show the default random part's
  near-zero variance components for comparison; the `frametoLme()`,
  `tdForm()` and `modelFrame()` help pages explain the time forms, singular
  fits and how to pass a simpler random part.
- Runnable `moveYr()` example on the bundled `PTclim05`: hydrological-year
  labels, recovering the calendar year, `label_year` and `ini.mnt`.
- Fixed the residual plots of `frametoLme()` and `modelFrame()` fits: nlme's
  default `aspect = "xy"` collapsed the panels to thin strips, and the axes
  carried internal column names (`.mf_resid__`, `.mf_fitted__`,
  `.model_resid__`). The panels now fill the plotting area and the axes read
  "Pearson residuals" and "Fitted values" (or the time variable). The plot of
  processed data (`modelFrame(form = NULL)`) also fills its panels. `aspect`,
  `xlab` and `ylab` given by the user are kept.
- Runnable examples on bundled data for `plot()` of `frametoLme` and
  `modelFrame` objects (residuals over time; residuals against fitted values,
  by core and by plot; processed series with `form = NULL`).
- Fixed the Friedman plots (`plot()` of `frametoFriedman` objects and of
  `modelFrame()` Friedman fits): the panels now fill the plotting area, and
  the axes read "Ratio index" (or "Difference index" with `difference =
  TRUE`) and "Fitted smooth" or the time variable instead of the column names
  `x.idx` and `x.fit`; `aspect`, `xlab` and `ylab` given by the user are kept.
- `frametoFriedman()` and `modelFrame()` Friedman fits: the printed status
  table no longer carries the note column, which wrapped into a mostly empty
  block; notes are listed below the table, one short line per series with its
  number of NA values (the full note and the count `n_na` are kept in the
  series table). `frametoFriedman()` lists the series in the order of their
  first rows in the data, as the `modelFrame()` path does, instead of
  factor-level order.
- Fixed `plot()` of `frametoFriedman` objects with `levels` coarser than the
  series (e.g. `levels = 'plot'`): one line was drawn through all points of a
  panel, joining different cores; each series now has its own line within the
  panel, with a key. The default plot is unchanged.
- Runnable examples on bundled data for `FriedmanForm()`, `frametoFriedman()`
  and the `summary()`, `print()` and `plot()` methods of `frametoFriedman`
  objects (ratio and difference indices, series status, panels by plot).
- Runnable examples on bundled data for `muleMan()` and its `plot()` method
  (biomass and aridity-index fluctuations, Mantel correlograms per core).
- Runnable `anova()` example for `modelFrame` objects: sequential F tests of
  one biomass model, and AIC/BIC of the time forms t and ln t fitted by ML.
- `plot()` of `muleMan` objects orders the panels by plot, tree (as numbers)
  and core, and labels the axes "Lag" and "Mantel r" unless `xlab` or
  `ylab` are given (they were ecodist's `lag` and `mantelr`).
- Help page for `anova()` of `frametoLme` objects: description, arguments and
  a runnable example (F tests of one fit; ML comparison of the time forms t
  and ln t).
- `update()` of `frametoLme` objects: the refitted object's call shows the
  original data expression instead of the internal placeholder `..rd..`.
  Fits are unchanged.
- `normalizeGroupLevels()` on fitted objects (`frametoLme`, `modelFrame`)
  without `groups` orders the grouping levels of `$fluc`: the grouping
  variables come from the metadata of `$fluc`, else from the model's grouping
  formula. Before, the call left `$fluc` unchanged (tree levels 18, 7, 56, 17
  in the `frametoLme` example fit; now 7, 17, 18, 56).
- `normalizeGroupLevels()` orders a grouping column as numbers only when all
  its non-missing labels read as numbers, and keeps the labels themselves as
  levels (e.g. "07" stays "07" and comes before 17). Before, a label that
  did not read as a number ("T7" next to 17, 18, 56), or that changed when
  read as one ("07", "1.0"), became NA.
- Help page for `update()` of `frametoLme` objects: description, arguments and
  a runnable example (ML refit; the time forms t and ln t).
- Help page for `normalizeGroupLevels()`: description, arguments and a
  runnable example (fitted object and data frame).
- Help page for `plot()` of `wlai` objects: description, details, arguments
  and an example on the bundled `PTclim05` (it simulated random climate data).
- The `wlai()` example uses the bundled `PTclim05` (one calendar year; one
  index per plot and hydrological year; a diagram) instead of random data.
- `moveYr()` help page: descriptions of the arguments `cd`, `ini.mnt`,
  `keep_hydro_id`, `hydro_id_name` and `validate`.
- `plot()` of `wlai` objects: the block label printed after the aridity index
  follows the `year` column of the block, so it follows `label_year`: with
  the default `'end'`, October 2000 to September 2001 of plot P44005 is
  labeled P44005.2001 (it showed the start year, P44005.2000). Block names,
  used by `which`, still hold `hydro_year_id`.
- 'lattice', used by the plot methods of `frametoLme`, `modelFrame` and
  `frametoFriedman` objects, is declared in Imports.
- Help pages: fixed nested `\code` markup (`plot()` and `summary()` of
  `frametoLme` objects), double-escaped markup in `scacum()`, the links to
  the 'nlme' plot methods for grouped data (`plot.nfnGroupedData`,
  `plot.nmGroupedData`), and the `cClass()` title.
- DESCRIPTION: the Description field no longer starts with the package name.
- BIOdry now depends on R (>= 4.1.0): its code and examples use the native
  pipe `|>`.
- NAMESPACE imports `AIC`, `BIC`, `as.formula`, `ave`, `fitted`, `logLik`,
  `resid` and `setNames` from 'stats'.
- `wlai()` help page: the Value section describes the returned data frame
  (it described a numeric index and a plot).
- `frametoLme()` drops only rows with missing values in the fitted variables
  or the grouping columns. The CRAN release 0.9.1 dropped rows with a missing
  value in any column, so fits can now use more rows (20 more in the
  `modelFrame()` biomass example).
- `mUnits()` help page: "milli" spelled correctly.

## Development versions 0.9.2 to 0.9.4

- Strengthened the internal helper chain used by `modelFrame()` and related workflows for multilevel tree-ring and climate data.

- Improved class and hierarchy detection in `cClass()`.
  - Reduced misclassification of integer-coded identifiers as time variables.
  - Added safer handling of character, logical, `Date`, and `POSIXt` columns.
  - Reduced ambiguity when long-format data contain several integer-like columns.

- Improved formula construction in `lmeForm()` and `tdForm()`.
  - `lmeForm()` now resolves response, time covariates, and grouping levels more defensively on multilevel long data.
  - `tdForm()` now preserves the intended formulation `log(x) ~ log(csx) + f(time)`.
  - `tdForm()` now checks tree-growth inputs more explicitly and warns or stops more clearly when the input looks climatic or does not contain the expected growth variables.

- Improved argument and level expansion helpers.
  - `arguSelect()` now validates target functions more safely, handles `MoreArgs` more defensively, and warns when direct arguments or `MoreArgs` entries are dropped.
  - `levexp()` now avoids the previous `slitFrame()` + `grep()` bottleneck, uses token-based matching, and behaves more predictably on multilevel labels.
  - `slitFrame()` now detects grouping columns more robustly and keeps split metadata for downstream helpers.

- Improved hydrological-year handling in `moveYr()`.
  - Replaced the previous drifting year-shift logic with a strict row-wise implementation.
  - Added support for `label_year = c("start", "end")`.
  - Added a stable internal grouping key `hydro_year_id` so downstream functions can group annual climatic blocks independently of the displayed year label.

- Improved Walter--Lieth aridity workflows.
  - `wlai()` now works more safely on long multilevel climate data by splitting into annual blocks instead of treating long concatenated tables as one continuous series.
  - `wlai()` now preserves hydrological month order inside each block.
  - `plot.wlai()` now supports multi-block `wlai` objects more safely, allows block selection without leaving the standard S3 `plot()` interface, and defaults to plotting the first available block when no selector is provided.
  - `plot.wlai()` now uses a black 100 mm line, a blue compressed precipitation band above 100 mm, and automatic top headroom for legends and labels.
  - Updated inlinedocs for `plot.wlai()` to document multilevel block selection, plotting defaults, and Walter--Lieth styling.

- Improved split/apply helpers for multilevel tree-ring and climate data.
  - `ringApply()` now respects the resolved `lv` more consistently, orders rows within each split block on the remaining time variables before applying `fn`, and returns columns in the order: records, time variables, then factor hierarchy.
  - `rtimes()` now works both as a block-level helper and on multilevel data frames, including grouped time reconstruction through `lv`.
  - `scacum()` now works both as a block-level helper and on multilevel data frames, including block-wise ordering before cumulative growth reconstruction.
  - `amod()` now works on block-level series and multilevel data frames, applies the allometric model to cumulative growth internally, returns both allometric `x` and `csx`, and avoids introducing an initial `NA` in the reconstructed increments.

- Improved reshaping in `shiftFrame()`.
  - The function now prefers explicit time columns in wide data for both climate and tree-ring series and uses row names only as a backward-compatibility fallback.
  - Added configurable hierarchy parsing through `split_pattern` while keeping dot splitting as the default.
  - Improved round-trips between wide and long formats for single-response tree-ring data and multi-response climate data.
  - Wide climate data can now preserve separate measured variables such as `tm` and `pm` instead of dropping one variable during reshaping.
  - Long outputs now follow the column-order contract: records, time variables, then factor hierarchy.
  - Updated inlinedocs to document explicit-time wide formats, deterministic reverse conversion through stored metadata, and long-output column ordering.

- Improved model fitting wrappers.
  - `frametoLme()` now orders multilevel long data before fitting, rebuilds residual series from real time and grouping columns more safely, and wraps residual outputs more consistently.
  - `modelFrame()` now distinguishes long data frames from non-data-frame lists more safely, handles staged `fn`/`lv` recursion more defensively, applies `mUnits()` to record-like numeric columns more conservatively, and passes validation settings through the helper chain.
  - Improved `modelFrame()` handling of long multilevel inputs by resolving stage levels against the current hierarchy without silently dropping valid grouping columns.

- Added or expanded inlinedocs details in patched helpers so the internal contracts are documented more explicitly.
  - Updated details for `ringApply()`, `shiftFrame()`, `amod()`, and `plot.wlai()`.
  - Documented split-first/order-second/apply-third behavior where relevant.
  - Documented column-order contracts for internal long-format outputs.

# BIOdry 0.9.1

- Fixed missing Rd \link{} targets by providing package anchors for:  
  - `anova.modelFrame.Rd`: linked `anova.lme` to the **nlme** package.  
  - `frametoLme.Rd`: linked `lme`, `nlme`, and `groupedData` to **nlme**.  
  - `lmeForm.Rd`: linked `groupedData` to **nlme**.  
  - `modelFrame.Rd`: linked `lme`, `nlme`, and `groupedData` to **nlme**.  
  - `muleMan.Rd`: linked `groupedData` to **nlme**, `mgram` to **ecodist**, and `ecodist` correctly.  
  - `shiftFrame.Rd`: linked `groupedData` to **nlme**.  
  - `summary.modelFrame.Rd`: linked `summary.lme` to **nlme**.  

# BIOdry 0.9
- Added labels to items in three files: `Pchron.Rd`, `Prec.Rd`, and `Temp.Rd`.  

# BIOdry 0.8
- **Changes in `shiftFrame`**: The internal implementation of `data.frame()` now uses `stringsAsFactors = TRUE`.  

# BIOdry 0.7
- Fixed convergence errors.  

# BIOdry 0.6
- Updated function examples to resolve convergence errors.  

# BIOdry 0.5
1. Developed five new **S3 methods**:  
   - `anova.modelFrame`  
   - `plot.modelFrame`  
   - `plot.muleMan`  
   - `plot.wlai`  
   - `summary.modelFrame`  
   *(See package documentation.)*  

2. **Changes in `modelFrame`**:  
   - Now supports two formats of dendroclimatic records: dendroclimatic chronologies and multilevel data series.  
   - Improved implementation examples.  

3. **Changes in `muleMan`**:  
   - Improved implementation examples.  

4. **New binary datasets** added to `data/` folder:  
   - `Pchron.RData`, `Prec.RData`, and `Temp.RData` (contain dendroclimatic chronologies).  

5. **New function `mUnits`**: Converts metric units in dendroclimatic chronologies.  

6. **Changes in `amod`**:  
   - Improved routines for allometric scaling.  

7. **New function `cClclass`**:  
   - Replaces the old `colclass` function.  

# BIOdry 0.4
1. **Changes in `tdForm`**:  
   - Fixed incorrect formulation of the time-decline form.  

2. **Changes in `ringLme`**:  
   - This function was replaced by `frametoLme()` to improve `lme` implementation from the **nlme** package.  

# BIOdry 0.3
1. **Changes in `shiftFrame`**:  
   - Updated algorithm to shift ring-data frames into multilevel data frames, fixing an internal error in R < 3.2.0.  
   - Added new default argument:  
     ```r
     lev.nm = c("plot", "tree", "sample")
     ```  
     This argument provides names for the factor-level columns in reshaped multilevel data frames.  

2. **Changes in `muleMan`**:  
   - Output is now a `groupedData` object.  
   - Removed the default plot in the output.  
   - Removed two default plot arguments:  
     - `plot.man = TRUE`  
     - `psig = 0.05`  
   - Users can now plot the `groupedData` object using other trellis-graph functions. *(See function examples.)*  
