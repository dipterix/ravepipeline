# Abstract class for 'RAVE' serialization

For package inheritance only; do not instantiate the class directly.

## See also

[`RAVEFileArray`](http://dipterix.org/ravepipeline/reference/RAVEFileArray.md)
[`rave-serialize-refhook`](http://dipterix.org/ravepipeline/reference/rave-serialize-refhook.md)

## Methods

### Public methods

- [`RAVESerializable$new()`](#method-RAVESerializable-new)

- [`RAVESerializable$@marshal()`](#method-RAVESerializable-@marshal)

- [`RAVESerializable$@unmarshal()`](#method-RAVESerializable-@unmarshal)

- [`RAVESerializable$@compare()`](#method-RAVESerializable-@compare)

- [`RAVESerializable$clone()`](#method-RAVESerializable-clone)

------------------------------------------------------------------------

### Method `new()`

Abstract constructor

#### Usage

    RAVESerializable$new(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `@marshal()`

Create an atomic list that can be serialized

#### Usage

    RAVESerializable$@marshal(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `@unmarshal()`

Restore an object from an atomic list

#### Usage

    RAVESerializable$@unmarshal(object, ...)

#### Arguments

- `object`:

  a list from `'@marshal'`

- `...`:

  ignored

------------------------------------------------------------------------

### Method `@compare()`

How two object can be compared to each other

#### Usage

    RAVESerializable$@compare(other)

#### Arguments

- `other`:

  another object to compare with self

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVESerializable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
