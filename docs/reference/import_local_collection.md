# Import a the definition of a collection of concepts in the local environment

Import a the definition of a collection of concepts in the local
environment

## Usage

``` r
import_local_collection(txt, overwrite = FALSE)
```

## Arguments

- txt:

  a JSON string or file

- overwrite:

  a single logical. If TRUE the collection is overwritten if it already
  exists (default: FALSE)

## Value

No return value, called for side effects. The collection will be
available and operations will be possible on its members.
