# Tidy up the files from a moodle assignment download

Goes through a folder of student submissions downloaded from moodle. It
unzips submissions, brings the files up out of any sub folders, and
checks that each submission has an Excel (.xlsx) worksheet. Submissions
without one are moved to a folder called "missing worksheet", next to
`assignmentfolder`.

## Usage

``` r
lab_assignment_files(assignmentfolder)
```

## Arguments

- assignmentfolder:

  The folder that was downloaded from moodle, containing folders called
  `..._assignsubmission_file_`.

## Value

Nothing is returned, but the folders of the submissions with a missing
worksheet are printed.

## Details

**Take care:** this changes and deletes files. Zip files are deleted
after they are unzipped, and submissions without a worksheet are moved
with the system `rm -R` command, so it only works on Mac and Linux. Run
it on a copy of the download. The fs package is needed.
