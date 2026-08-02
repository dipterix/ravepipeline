# Embed binary data or JSON strings into HTML files

`html_embed_write` encodes JSON strings, plain-text strings, and binary
files as base64 `<script>` tags and injects them into an HTML file.

`html_embed_read` reads `<script>` tags back out of a saved HTML file
and reconstructs the original data.

## Usage

``` r
html_embed_read(path, name = NULL, parse_json = TRUE, update = FALSE)

html_embed_write(
  html_path,
  json_string = list(),
  text_string = list(),
  binary_paths = list(),
  missing_action = c("error", "warning", "ignore")
)
```

## Arguments

- path:

  character path to an HTML file, or a manifest object returned by a
  previous call to `html_embed_read(path, name = NULL)`.

- name:

  character; the `data-for` name of the entry to decode. When `NULL`
  (default) the function returns a manifest object that lists all
  embedded entries without decoding them.

- parse_json:

  logical; when `TRUE` (default) JSON entries are parsed with
  [`jsonlite::fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  before being returned.

- update:

  logical; when `FALSE` (default) already-decoded entries cached in the
  manifest are returned as-is without re-reading the file.

- html_path:

  character; path to the HTML file to write. If the file does not exist,
  behavior is controlled by `missing_action`.

- json_string:

  named list of character strings; each element is a UTF-8 JSON string.
  The list name becomes the `data-for` attribute.

- text_string:

  named list of character strings; each element is an arbitrary UTF-8
  plain-text string. The list name becomes the `data-for` attribute.

- binary_paths:

  named list of character strings; each element is an absolute path to a
  binary file to embed. The list name becomes the `data-for` attribute.

- missing_action:

  character; what to do when `html_path` does not exist. `"error"`
  (default) stops with an error; `"warning"` emits a warning and creates
  the file; `"ignore"` creates the file silently.

## Value

`html_embed_write`: `html_path`, invisibly.

`html_embed_read`: when `name` is `NULL`, a manifest object of class
`ravepipeline_html_embed_manifest` listing all embedded entries. When
`name` is specified the manifest is returned with the requested entry
decoded and cached; access it via `manifest$content[[name]]`: a
character string for JSON/text data, or a raw vector for binary data.

## Details

`html_embed_write` streams data after `</body>` (or before `</html>`, or
appends when neither tag is found). Large inputs are split into
\\\approx 48\\\mathrm{KB}\\ chunks; each chunk gets its own `<script>`
tag with a sequential `data-partition` index.

`html_embed_read`: when `name` is `NULL` it returns a manifest object
that lists all embedded entries; subsequent calls with a specific `name`
use seek positions stored in the manifest to retrieve only the requested
partitions. Files written by compatible tools (e.g. threeBrain) are
handled transparently.

The per-entry `<script>` tag format:


    <script type='text/plain;charset=UTF-8'
            data-for='<name>'
            data-partition='<N>'
            data-type='application/json|text/plain|application/octet-stream'
            data-size='<total bytes>'
            data-start='<byte offset>'
            data-partition-size='<this chunk bytes>'>
    BASE64 (72-character wrapped lines)
    </script>

## Examples

``` r

html_file <- tempfile(fileext = ".html")
writeLines(
  c("<html>", "<head></head>", "<body></body>", "</html>"),
  html_file
)

# ---- Write: embed JSON and binary data into an HTML file --------
tmp <- tempfile(fileext = ".bin")
writeBin(as.raw(0:255), tmp)

html_embed_write(
  html_file,
  json_string  = list(meta = '{"version":1}'),
  text_string  = list(note = "hello world"),
  binary_paths = list(data = tmp)
)

# ---- Read: list all embedded entries ----------------------------
manifest <- html_embed_read(html_file)
print(manifest)
#> <HTML Embed Manifest>
#> Path: /tmp/RtmpvJIxER/file1bf721e7ee3a.html
#> Data Names:
#>   `data`
#>   `meta`
#>   `note`
#> 

# ---- Read: decode a specific entry ------------------------------
manifest <- html_embed_read(html_file, name = "meta")
manifest$content[["meta"]]   # character (JSON string or parsed object)
#> $version
#> [1] 1
#> 

manifest <- html_embed_read(manifest, name = "data")
manifest$content[["data"]]   # raw vector
#>   [1] 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16 17 18
#>  [26] 19 1a 1b 1c 1d 1e 1f 20 21 22 23 24 25 26 27 28 29 2a 2b 2c 2d 2e 2f 30 31
#>  [51] 32 33 34 35 36 37 38 39 3a 3b 3c 3d 3e 3f 40 41 42 43 44 45 46 47 48 49 4a
#>  [76] 4b 4c 4d 4e 4f 50 51 52 53 54 55 56 57 58 59 5a 5b 5c 5d 5e 5f 60 61 62 63
#> [101] 64 65 66 67 68 69 6a 6b 6c 6d 6e 6f 70 71 72 73 74 75 76 77 78 79 7a 7b 7c
#> [126] 7d 7e 7f 80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 90 91 92 93 94 95
#> [151] 96 97 98 99 9a 9b 9c 9d 9e 9f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae
#> [176] af b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf c0 c1 c2 c3 c4 c5 c6 c7
#> [201] c8 c9 ca cb cc cd ce cf d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df e0
#> [226] e1 e2 e3 e4 e5 e6 e7 e8 e9 ea eb ec ed ee ef f0 f1 f2 f3 f4 f5 f6 f7 f8 f9
#> [251] fa fb fc fd fe ff

unlink(c(tmp, html_file))
```
