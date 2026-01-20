# Validate Workflow `YAML` Schema (Internal)

Strictly validates a workflow `YAML` file or parsed `YAML` list against
the expected schema. This is an internal function used to ensure `YAML`
files conform to the agreed-upon structure.

## Usage

``` r
mcpflow_validate_yaml(x)
```

## Arguments

- x:

  Either a file path to a `YAML` file, or a list read from the file

## Value

A list with components:

- `valid`:

  Logical, whether the YAML is valid

- `errors`:

  Character vector of error messages (schema violations)

- `warnings`:

  Character vector of warning messages (recommendations)
