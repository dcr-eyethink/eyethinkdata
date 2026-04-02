# reads in a set of data files and stores them in a list

reads in a set of data files and stores them in a list

## Usage

``` r
readin_datalist(
  folder = "processed",
  flist = NULL,
  data = list(),
  drp = F,
  token = NULL
)
```

## Arguments

- folder:

  where to find the data, defaults to 'processed'

- flist:

  what files do you want from folder? Defaults to all of them

- data:

  already existing list to add data to

- drp:

  is this to be downloaded from dropbox URL?

- token:

  dropbox token
