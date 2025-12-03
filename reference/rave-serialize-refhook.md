# Serialization reference hook generic functions

Serialization reference hook generic functions

## Usage

``` r
rave_serialize_refhook(object)

rave_serialize_impl(object)

# Default S3 method
rave_serialize_impl(object)

# S3 method for class 'RAVESerializable'
rave_serialize_impl(object)

# S3 method for class '`rave-brain`'
rave_serialize_impl(object)

rave_unserialize_refhook(x)

rave_unserialize_impl(x)

# Default S3 method
rave_unserialize_impl(x)

# S3 method for class 'rave_serialized'
rave_unserialize_impl(x)

# S3 method for class 'rave_serialized_r6'
rave_unserialize_impl(x)

# S3 method for class '`rave_serialized_rave-brain`'
rave_unserialize_impl(x)
```

## Arguments

- object:

  Object to serialize (environment or external pointers)

- x:

  raw or string objects that will be passed to
  [`unserialize`](https://rdrr.io/r/base/serialize.html) function before
  further reconstruction

## Value

`rave_serialize_refhook` returns either serialized objects in string
(raw vector converted to char via `rawToChar`), or `NULL` indicating the
object undergoing default serialization; `rave_unserialize_refhook`
returns the reconstructed object.

## Examples

``` r
# This example requires additional `filearray` package
# If you are an RAVE user (installed RAVE via rave.wiki)
# then this package was installed

x0 <- array(rnorm(240000), c(200, 300, 4))
x1 <- filearray::as_filearray(x0)
x2 <- RAVEFileArray$new(x1, temporary = TRUE)

r0 <- serialize(x0, NULL, refhook = rave_serialize_refhook)
r1 <- serialize(x1, NULL, refhook = rave_serialize_refhook)
r2 <- serialize(x2, NULL, refhook = rave_serialize_refhook)

# Compare the serialization sizes
c(length(r0), length(r1), length(r2))
#> [1] 1920074 1027169     461

y0 <- unserialize(r0, refhook = rave_unserialize_refhook)
y1 <- unserialize(r1, refhook = rave_unserialize_refhook)
y2 <- unserialize(r2, refhook = rave_unserialize_refhook)

all(y0 == x0)
#> [1] TRUE
all(y1[] == x0)
#> [1] TRUE
all(y2[] == x0)
#> [1] TRUE

if (FALSE) { # \dontrun{

# 3D Brain, this example needs RAVE installation, not included in
# this package, needs extra installations available at rave.wiki

# 4 MB
brain <- ravecore::rave_brain("demo/DemoSubject")

# 52 KB
rbrain <- serialize(brain, NULL, refhook = rave_serialize_refhook)

brain2 <- unserialize(rbrain, refhook = rave_unserialize_refhook)

brain2$plot()

} # }
```
