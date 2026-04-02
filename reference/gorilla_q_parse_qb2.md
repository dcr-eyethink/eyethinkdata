# Get subject entered, unscored data like age and gender from gorilla. Will give you the qual or quantised versions

Get subject entered, unscored data like age and gender from gorilla.
Will give you the qual or quantised versions

## Usage

``` r
gorilla_q_parse_qb2(
  data,
  qlist = NULL,
  preface = NULL,
  strip = NULL,
  pd = NULL
)
```

## Arguments

- data:

  raw data packet from from gorilla import

- qlist:

  list of Task.Name, ie questionnaire blob names, that you want to
  process

- preface:

  add some text to all variables

- strip:

  get rid of either the quant or qual
