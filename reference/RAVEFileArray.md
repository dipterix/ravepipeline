# 'R6' wrapper for `'FileArray'`

Wrapper for better serialization (check 'See also')

## See also

[`RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
[`rave-serialize-refhook`](http://dipterix.org/ravepipeline/reference/rave-serialize-refhook.md)

## Super class

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `RAVEFileArray`

## Public fields

- `temporary`:

  whether this file array is to be upon garbage collection; default is
  false. The file array will be deleted if the temporary flag is set to
  true and the array mode is `'readwrite'`

## Active bindings

- `valid`:

  whether the array is valid and ready to read

- `@impl`:

  the underlying array object

## Methods

### Public methods

- [`RAVEFileArray$@marshal()`](#method-RAVEFileArray-@marshal)

- [`RAVEFileArray$@unmarshal()`](#method-RAVEFileArray-@unmarshal)

- [`RAVEFileArray$new()`](#method-RAVEFileArray-new)

- [`RAVEFileArray$clone()`](#method-RAVEFileArray-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)

------------------------------------------------------------------------

### Method `@marshal()`

Serialization helper, convert the object to a descriptive list

#### Usage

    RAVEFileArray$@marshal(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `@unmarshal()`

Serialization helper, convert the object from a descriptive list

#### Usage

    RAVEFileArray$@unmarshal(object, ...)

#### Arguments

- `object`:

  serialized list

- `...`:

  ignored

------------------------------------------------------------------------

### Method `new()`

Constructor

#### Usage

    RAVEFileArray$new(x, temporary = FALSE)

#### Arguments

- `x`:

  file array or can be converted to
  [`as_filearray`](https://dipterix.org/filearray/reference/filearray.html)

- `temporary`:

  whether this file array is to be deleted once the object is
  out-of-scope; default is false

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVEFileArray$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
