# MCP Tools for RAVE Pipelines

This document describes the MCP (Model Context Protocol) tools system for ravepipeline.

## Overview

The ravepipeline package provides a complete MCP tools specification and runtime system that allows AI agents to interact with RAVE neuroscience analysis pipelines. The system is designed to work without requiring ellmer/mcptools as package dependencies, while still supporting dynamic tool creation when these packages are available.

## Architecture

### 1. Tool Specifications (`inst/mcp-tools.yaml`)

A YAML file containing complete specifications for 8 MCP tools:
- Tool names, descriptions, and categories
- Parameter schemas (types, required, descriptions, examples)
- Return value schemas
- Safety annotations (dangerous tools, approval requirements)
- Workflow recommendations
- Safety guidelines for AI agents

### 2. Implementation Functions (`R/mcp-tool-functions.R`)

Eight exported functions that implement the tool behavior:
- `mcp_list_rave_pipelines()` - Discovery
- `mcp_load_rave_pipeline()` - Setup
- `mcp_get_rave_pipeline_info()` - Information
- `mcp_set_rave_pipeline_settings()` - Configuration
- `mcp_run_rave_pipeline()` - Execution (dangerous)
- `mcp_get_rave_pipeline_progress()` - Monitoring
- `mcp_read_rave_pipeline_results()` - Results
- `mcp_visualize_rave_pipeline()` - Visualization

### 3. Helper Functions (`R/mcp-tools.R`)

Two main helper functions:
- `rave_mcp_tools()` - Load tool specifications (returns list/YAML/JSON)
- `rave_mcp_ellmer_tools()` - Create ellmer tool objects at runtime

## Usage

### For Package Users

#### Query Tool Specifications

```r
# Load all tool specs as R list
specs <- ravepipeline::rave_mcp_tools()
str(specs$tools[[1]])

# Get as JSON (requires jsonlite)
json_specs <- ravepipeline::rave_mcp_tools(format = "json")
cat(json_specs)

# Get specific tools only
discovery_tools <- ravepipeline::rave_mcp_tools(
  names = c("list_rave_pipelines", "load_rave_pipeline")
)
```

#### Create MCP Server with ellmer

```r
library(ellmer)
library(mcptools)
library(ravepipeline)

# Create all tools dynamically
tools <- rave_mcp_ellmer_tools()

# Start MCP server
mcp_server(tools = tools)
```

#### Integration with btw

```r
library(btw)
library(ravepipeline)

# Combine btw tools with RAVE pipeline tools
all_tools <- c(
  btw_tools(),
  rave_mcp_ellmer_tools()
)

mcptools::mcp_server(tools = all_tools)
```

#### Direct Function Use

The implementation functions can be called directly without MCP:

```r
library(ravepipeline)

# List pipelines
result <- mcp_list_rave_pipelines()
print(result$pipelines)

# Load a pipeline
mcp_load_rave_pipeline("power_explorer")

# Get pipeline info
info <- mcp_get_rave_pipeline_info()
str(info)
```

### For AI Agents

AI agents should use the following workflow:

1. **Discovery**: `list_rave_pipelines` to see available pipelines
2. **Load**: `load_rave_pipeline` to initialize a pipeline
3. **Inspect**: `get_rave_pipeline_info` to understand requirements
4. **Configure**: `set_rave_pipeline_settings` to set parameters
5. **Execute**: `run_rave_pipeline` (REQUIRES USER APPROVAL)
6. **Monitor**: `get_rave_pipeline_progress` during execution
7. **Retrieve**: `read_rave_pipeline_results` to get outputs

Safety guidelines:
- Always request user approval before calling `run_rave_pipeline`
- Explain what will be executed before requesting approval
- Show current settings to the user
- Monitor progress during long-running executions

## Tool Categories

- **discovery**: Finding available pipelines
- **setup**: Loading and initializing pipelines
- **info**: Getting pipeline information
- **configuration**: Setting pipeline parameters
- **execution**: Running computations (dangerous)
- **monitoring**: Checking progress
- **results**: Retrieving outputs
- **visualization**: Understanding pipeline structure

## Dependencies

The MCP tools system has NO required dependencies in the ravepipeline package.

Optional dependencies for enhanced functionality:
- `ellmer` (>= 0.3.0): For creating ellmer tool objects
- `mcptools` (>= 0.2.0): For MCP server functionality
- `btw`: For integrated R environment tools
- `jsonlite`: For JSON output format

## Example: Complete MCP Server

```r
# File: rave_mcp_server.R

library(mcptools)
library(ravepipeline)

# Create RAVE pipeline tools
tools <- rave_mcp_ellmer_tools()

# Start MCP server on stdio
mcp_server(tools = tools)
```

Configure in Claude Desktop (`~/Library/Application Support/Claude/claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "rave-pipelines": {
      "command": "Rscript",
      "args": ["-e", "library(mcptools); library(ravepipeline); mcp_server(tools = rave_mcp_ellmer_tools())"]
    }
  }
}
```

## Development Workflow

For package developers who want to modify the MCP tools:

1. Edit implementation functions in `R/mcp-tool-functions.R`
2. Update tool specifications in `inst/mcp-tools.yaml`
3. Rebuild package documentation: `devtools::document()`
4. Test tools: `devtools::load_all(); rave_mcp_ellmer_tools()`

The system is designed to keep specifications and implementations in sync manually. Future versions may include automatic YAML generation from roxygen2 tags.

## Resources

- RAVE Documentation: https://rave.wiki
- MCP Specification: https://modelcontextprotocol.io
- ellmer Package: https://ellmer.tidyverse.org
- mcptools Package: https://posit-dev.github.io/mcptools
- btw Package: https://posit-dev.github.io/btw
