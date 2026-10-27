# Score the performance engagement questionnaire

Scores a questionnaire on engagement with a performance, based on Brown
& Novak (2007), "Assessing the intrinsic impacts of a live performance".
The questionnaire template is at
<https://app.gorilla.sc/admin/task/352459/>.

## Usage

``` r
gorilla_q_performance_engagement(data, full = FALSE, taskname = "Engagement")
```

## Arguments

- data:

  The list of data from a gorilla import, or just the questionnaire data
  (`data_q`), in long format.

- full:

  Not currently used.

- taskname:

  Not currently used.

## Value

A data.table with one row per participant, the four scores and the item
scores.

## Details

It looks for questionnaires with "PerformanceExperience" in their name.
The 12 items are averaged to make four scores: `pe_captive`,
`pe_emotion`, `pe_social` and `pe_spirit`. The individual items are also
returned, as `pe_1_absorb` etc.
