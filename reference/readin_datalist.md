# Read in a set of data files and store them in a list

Reads all of the .csv files in a folder (or chosen ones) into a list of
data.tables, named after the files. It reverses
[`saveout_datalist()`](https://dcr-eyethink.github.io/eyethinkdata/reference/saveout_datalist.md).

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

  Folder (or vector of folders) to read from. Default is "processed". If
  it does not exist, you will be asked to choose one.

- flist:

  Names of the files to read (including .csv). Default is all of the
  files in the folder.

- data:

  An existing list to add the data to.

- drp:

  Read the files from a dropbox folder? This needs the rdrop2 package.

- token:

  The dropbox token, if using `drp`.

## Value

A list of data.tables, named after the files (without .csv).

## Details

If a list is given in `data` that already has an item with the same name
as a file, the new data is added to the bottom of the existing item.
This is useful for adding more participants.

## See also

[`saveout_datalist()`](https://dcr-eyethink.github.io/eyethinkdata/reference/saveout_datalist.md)
