# RangeShifter + Genetics Workshop

## RangeShiftR + Genetics

### Installation

Run this line in R:

```r
devtools::install_github("https://github.com/RangeShifter/RangeShiftR-pkg", ref = "transloc_new_genetics_spatial_demog_merge", subdir="RangeShiftR")
```

Note the `ref = "transloc_new_genetics_spatial_demog_merge"`, which makes sure you're installing from the development branch that contains the new genetic features.

If the installation was successful, you should be able to access the genetic object classes, e.g.:

```R
NeutralTraits
```

```
class generator function for class “NeutralTraitsParams” from package ‘RangeShiftR’
function (...) 
new("NeutralTraitsParams", ...)
<bytecode: 0x000001d9bed7c698>
<environment: namespace:RangeShiftR>
```

See the User manual for documentation on the software. 

For inputs, you can use the R help pages for the different classes:

```r
?NeutralTraits
```

## Documentation

The `doc/` folder contains the RangeShifter user manual updated with documentation on the genetics module (see the "Genetics in 3.0" section).

The `batchmode/` folder contains documentation on how to format input files for the batch mode in the form of excel spreadsheets.

## Batchmode

The RangeShifter.exe program can be used to run RangeShifter in batch mode on Windows systems, using one of the examples projects.

The batchmode is also available on MacOS + Linux, but you'll need to build RangeShifter yourself. You can find instructions on how to do so on the `RangeShifter_batch` repo on GitHub.
