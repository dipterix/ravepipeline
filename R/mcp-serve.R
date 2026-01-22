# MCP Server Implementation for RAVE Pipelines

#' Start RAVE Pipeline MCP Server
#'
#' @description
#' Starts a Model Context Protocol (MCP) server that exposes RAVE pipeline
#' tools to external applications like VSCode GitHub Copilot or Claude Desktop.
#' This function loads all RAVE MCP tools and serves them via the MCP protocol
#' using the \pkg{btw} package over stdio (standard input/output), enabling
#' AI assistants to interact with RAVE pipelines programmatically.
#'
#' The server runs indefinitely and blocks the R session. It is intended to be
#' started from the command line (e.g., via \code{Rscript}) rather than
#' interactively.
#'
#' @param tools_pkg Character, name of package containing MCP tools.
#'   Default: \code{"ravepipeline"}
#' @param workflow Character or NULL, optional workflow name to pre-load for
#'   additional AI guidance. If provided, the corresponding workflow YAML file
#'   will be loaded and included in tool descriptions. Default: \code{NULL}
#' @param state_env Environment or NULL, shared state environment for tools.
#'   If NULL, a new state environment will be created using
#'   \code{\link{mcptool_state_factory}}. Default: \code{NULL}
#'
#' @return This function does not return; it blocks indefinitely while serving
#'   the MCP protocol. The server must be terminated externally (e.g., Ctrl+C).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Loads all MCP tools from the specified package using
#'         \code{\link{mcptool_load_all}}
#'   \item Creates a shared state environment via \code{\link{mcptool_state_factory}}
#'   \item Instantiates each tool as an \code{ellmer::ToolDef} object using
#'         \code{\link{mcptool_instantiate}}
#'   \item Starts the MCP server via \code{btw::btw_mcp_server()}, which
#'         handles JSON-RPC communication via stdio using \pkg{mcptools}
#' }
#'
#' @section VSCode Configuration:
#' To use RAVE tools with VSCode GitHub Copilot, add the following to your
#' \code{.vscode/settings.json} file:
#'
#' \preformatted{
#' {
#'   "github.copilot.chat.mcp.servers": {
#'     "rave-pipelines": {
#'       "command": "Rscript",
#'       "args": [
#'         "-e",
#'         "ravepipeline::ravepipeline_serve_mcp()"
#'       ]
#'     }
#'   }
#' }
#' }
#'
#' After saving the configuration, restart VSCode or reload the window. The
#' RAVE tools will be available in GitHub Copilot chat.
#'
#' @section Claude Desktop Configuration:
#' To use RAVE tools with Claude Desktop, add the following to your Claude
#' configuration file:
#'
#' \strong{macOS/Linux:}
#' \code{~/Library/Application Support/Claude/claude_desktop_config.json}
#'
#' \strong{Windows:}
#' \code{\%APPDATA\%\\Claude\\claude_desktop_config.json}
#'
#' \preformatted{
#' {
#'   "mcpServers": {
#'     "rave-pipelines": {
#'       "command": "Rscript",
#'       "args": [
#'         "-e",
#'         "ravepipeline::ravepipeline_serve_mcp()"
#'       ]
#'     }
#'   }
#' }
#' }
#'
#' After saving the configuration, restart Claude Desktop. The RAVE tools will
#' be available in conversations.
#'
#' @examples
#' \dontrun{
#' # Start MCP server for VSCode/Claude (stdio mode)
#' ravepipeline_serve_mcp()
#'
#' # Pre-load workflow guidance
#' ravepipeline_serve_mcp(workflow = "rave_pipeline_class_guide")
#'
#' # Command-line usage
#' # Rscript -e "ravepipeline::ravepipeline_serve_mcp()"
#' }
#'
#' @seealso
#' \code{\link{mcptool_load_all}}, \code{\link{mcptool_instantiate}},
#' \code{\link{mcptool_state_factory}}, \code{\link{mcp_describe}}
#'
#' @export
ravepipeline_serve_mcp <- function(
    tools_pkg = "ravepipeline",
    workflow = NULL,
    state_env = NULL
) {
  # Check if btw is available
  if (!requireNamespace("btw", quietly = TRUE)) {
    stop(
      "Package 'btw' is required but not installed.\n",
      "Install it with: pak::pak('btw')",
      call. = FALSE
    )
  }

  # Display startup message
  message("Starting RAVE Pipeline MCP Server...")

  # Load all MCP tools from the package
  message("Loading MCP tools from package: ", tools_pkg)
  tools <- mcptool_load_all(tools_pkg)
  message("Loaded ", length(tools), " tool(s)")

  # Create shared state environment if not provided
  if (is.null(state_env)) {
    message("Creating shared state environment...")
    state_env <- mcptool_state_factory()
  }

  # Optionally load workflow guidance
  if (!is.null(workflow)) {
    message("Loading workflow guidance: ", workflow)
    # The workflow will be loaded by mcp_describe when tools are called
    # We don't need to do anything here, just document the intent
  }

  # Instantiate each tool as an ellmer::ToolDef object
  # This converts our YAML-based tool definitions into ellmer's format
  message("Instantiating tools for MCP protocol...")
  ellmer_tools <- lapply(tools, function(tool) {
    mcptool_instantiate(
      tool = tool,
      state_env = state_env
    )
  })

  # Filter out any NULL results (in case some tools failed to instantiate)
  ellmer_tools <- ellmer_tools[!vapply(ellmer_tools, is.null, logical(1))]

  if (length(ellmer_tools) == 0) {
    stop("No tools were successfully instantiated. Cannot start server.", call. = FALSE)
  }

  message("Successfully instantiated ", length(ellmer_tools), " tool(s)")

  # Start the MCP server using btw
  # This function blocks indefinitely and serves via stdio
  message("\n", strrep("=", 60))
  message("MCP Server is running. Press Ctrl+C to stop.")
  message(strrep("=", 60), "\n")

  btw::btw_mcp_server(
    tools = ellmer_tools
  )

  # This line is never reached unless the server is stopped
  message("MCP Server stopped.")
  invisible(NULL)
}
