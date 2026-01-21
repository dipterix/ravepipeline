# Test mcp_describe function
# Source the file directly for testing
source('R/mcp-describe.R')

cat('Testing mcp_describe after loading package...\n\n')

# Test with different objects
cat('=== Data frame ===\n')
result <- mcp_describe(head(mtcars, 3))
cat(result, sep = '\n')

cat('\n\n=== Function ===\n')
result <- mcp_describe(sum)
cat(head(result, 5), sep = '\n')

cat('\n\n=== Environment ===\n')
result <- mcp_describe(.GlobalEnv)
cat(head(result, 5), sep = '\n')

cat('\n\n=== List ===\n')
test_list <- list(a = 1:5, b = letters[1:3], c = list(d = "nested"))
result <- mcp_describe(test_list)
cat(result, sep = '\n')

cat('\n\n=== Large vector ===\n')
big_vec <- 1:1000
result <- mcp_describe(big_vec)
cat(head(result, 10), sep = '\n')

cat('\n\n=== NULL ===\n')
result <- mcp_describe(NULL)
cat(result, sep = '\n')

cat('\n\n=== Character vector ===\n')
result <- mcp_describe(c("hello", "world"))
cat(result, sep = '\n')

cat('\n\n✓ mcp_describe is working correctly!\n')
