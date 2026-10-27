# Bulk search and replace in file names

Renames every file in a folder by replacing text in the file name, and
optionally adding text to the start or end of the name. Give either a
single pattern `p` and replacement `r`, or a table of several
replacements in `pr`.

## Usage

``` r
bulk_file_rename(
  datafolder = NULL,
  p = " ",
  r = "",
  suffix = "",
  prefix = "",
  pr = NULL
)
```

## Arguments

- datafolder:

  Folder containing the files to rename. If left as `NULL`, you will be
  asked to choose a folder.

- p:

  Text to search for in each file name (literal match). Default is a
  single space.

- r:

  Text to replace `p` with. Default is "" (i.e. delete `p`).

- suffix:

  Text to add to the end of each file name (after the file extension).

- prefix:

  Text to add to the start of each file name.

- pr:

  A data.table (or data.frame) with columns `p` and `r`, one row for
  each replacement to make. If given, `p` and `r` are ignored.

## Value

Nothing is returned; the function is called for its side effect of
renaming files.

## Details

With the default arguments it simply removes all spaces from the file
names.

- `p` is matched as a literal string (not a regular expression), and
  every occurrence in the name is replaced.

- This function renames files in place, so there is no undo. Try it on a
  copy of your folder first.

- If a file would be renamed to a name that already exists, it is left
  alone and a warning is given.

## Examples

``` r
if (FALSE) { # \dontrun{
# remove spaces from all the file names in a folder
bulk_file_rename("my_data_folder")
# swap "_v1" for "_final"
bulk_file_rename("my_data_folder", p = "_v1", r = "_final")
} # }
```
