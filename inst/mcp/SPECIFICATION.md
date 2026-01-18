# RAVE MCP Tools YAML Specification

This document describes the YAML format specification for RAVE MCP (Model Context Protocol) tools generated from roxygen2 documentation.

## Overview

MCP tools are automatically generated from R function documentation using roxygen2 comments. The parser extracts metadata, parameters, types, examples, and other information to create JSON Schema-compliant YAML files.

## YAML File Structure

Each MCP tool is defined in a separate YAML file with the following structure:

```yaml
name: function_name
description: Tool description
parameters:
  type: object
  properties:
    param1:
      type: string
      description: Parameter description
      enum: [option1, option2]  # Optional
      default: value            # Optional
      examples: [example1, example2]  # Optional
    param2:
      type: array
      items:
        type: string
      description: Array parameter
  required: [param1]
category: Category Name        # Optional
example_response: "..."        # Optional
returns:                       # Optional
  type: object
  properties:
    field1:
      type: string
      description: Description
```

## Field Descriptions

### Top-Level Fields

- **name** (required): The function name, exactly as defined in R
- **description** (required): Brief description of what the tool does (from `@description` or `@title`)
- **parameters** (required): JSON Schema object defining the function parameters
- **category** (optional): Tool category extracted from `@keywords mcp-category-NAME`
- **example_response** (optional): Example output from `@examples` section
- **returns** (optional): Return value schema from `@return` or `@returns` section

### Parameter Schema Fields

Each parameter in `parameters.properties` contains:

- **type** (required): JSON Schema type (string, number, integer, boolean, object, array)
- **description** (required): Clean parameter description with R type prefixes removed
- **enum** (optional): Array of valid values, extracted from function formals like `c("option1", "option2")`
- **default** (optional): Default value serialized to JSON format (NULL -> null, TRUE -> true, etc.)
- **examples** (optional): Array of example values extracted from `\code{}` blocks in parameter descriptions

### Array Types

For array parameters, use the `items` field:

```yaml
param_name:
  type: array
  items:
    type: string  # or number, integer, boolean, object
  description: Description
```

### Required Parameters

The `required` field is an array listing all mandatory parameters:

```yaml
required: [param1, param2, param3]
```

## Type Inference Rules

The parser infers JSON Schema types from R documentation:

1. **Keywords** (case-insensitive):
   - "character", "string" -> `string`
   - "numeric", "number", "double" -> `number`
   - "integer" -> `integer`
   - "logical", "boolean" -> `boolean`
   - "list" -> `object`

2. **Array Notation**:
   - "Character[]", "character vector", "character array" -> `type: array, items: {type: string}`
   - "Numeric[]", "numeric vector" -> `type: array, items: {type: number}`
   - "Integer[]", "integer vector" -> `type: array, items: {type: integer}`

3. **Default Type**: If no keywords match, defaults to `string`

## Roxygen2 Markup Handling

The parser automatically cleans roxygen2/Rd markup:

- `\code{value}` -> `value`
- `\link{function}` -> `function`
- `\item{label}{text}` -> `label: text`
- LaTeX commands (e.g., `\eqn{}`, `\deqn{}`) are stripped

## Type Prefix Stripping

R-style type prefixes are removed from descriptions for cleaner API documentation:

**Removed prefixes**:
- "Character string,"
- "Character vector,"
- "Character[],"
- "Character,"
- "Logical,"
- "Integer,"
- "Numeric,"
- "Double,"
- "List,"
- "Data frame,"
- "Matrix,"
- "Array[],"
- "Array,"
- "Vector,"
- "Function,"
- "Environment,"

**Note**: Array notation in type field is preserved (e.g., `Character[]` becomes `type: array, items: {type: string}`)

## Keyword Metadata

Keywords are extracted from `@keywords` tags and must use the `mcp-` prefix:

- **Category**: `@keywords mcp-category-NAME` (e.g., `mcp-category-discovery`, `mcp-category-setup`)
- **Dangerous flag**: `@keywords mcp-dangerous`
- **Requires approval**: `@keywords mcp-requires-approval`
- **Tool marker**: `@keywords mcp-tool` (marks function as an MCP tool)

Example:
```r
#' @keywords mcp-tool mcp-category-pipeline_management mcp-requires-approval
```

## Example Extraction

### Per-Parameter Examples

Examples are extracted from parameter descriptions using `\code{}` blocks:

```r
#' @param pipeline_name Character string, the pipeline name.
#'   Example values: \code{"power_explorer"}, \code{"notch_filter"},
#'   \code{"wavelet_module"}
```

Results in:

```yaml
pipeline_name:
  type: string
  description: the pipeline name
  examples: ["power_explorer", "notch_filter", "wavelet_module"]
```

### Example Responses

Full example outputs are extracted from `@examples` sections:

```r
#' @examples
#' \dontrun{
#'   result <- mcp_function()
#'   # Output: "Success"
#' }
```

## Enum Detection

The parser detects enumeration values from function formals:

```r
my_function <- function(mode = c("summary", "details")) {
  # ...
}
```

Generates:

```yaml
mode:
  type: string
  enum: ["summary", "details"]
  default: "summary"
```

**Detection rules**:
- Must be a vector: `c("a", "b", "c")`
- Singular parameter name -> first value is default
- Plural parameter name -> all values are valid, no default

## Default Value Serialization

R values are converted to JSON-compatible format:

| R Value | JSON Value |
|---------|------------|
| `NULL` | `null` |
| `TRUE` | `true` |
| `FALSE` | `false` |
| `"string"` | `"string"` |
| `123` | `123` |
| `c("a", "b")` | `["a", "b"]` (when used as enum) |

## Multi-Line Parameter Support

Parameter descriptions can span multiple lines:

```r
#' @param long_param This is a very long parameter description
#'   that continues on the next line
#'   and even more lines
#' @param next_param Another parameter
```

The parser correctly collects all continuation lines before the next `@tag`.

## File Naming Convention

YAML files are named using the pattern:

```
{package}-{function_name}.yaml
```

Example: `ravepipeline-mcp_load_rave_pipeline.yaml`

Note: The tool name in the YAML uses hyphen format: `pkg-function_name` (e.g., `ravepipeline-mcp_load_rave_pipeline`). The first hyphen separates the package name from the function name.

## Output Directory

All generated YAML files are stored in:

```
inst/mcp/tools/
```

## JSON Schema Compliance

All generated YAML files conform to JSON Schema Draft 7 specification:
- `type` field is required for all properties
- `enum` values must be arrays
- `required` must be an array
- Array types must include `items` specification
