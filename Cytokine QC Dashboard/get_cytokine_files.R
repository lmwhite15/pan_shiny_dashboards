# List files in a CyVerse WebDAV directory, then download via SFTP
# (WebDAV downloads for some files come back corrupted -- a CyVerse-side
# Varnish cache issue on the authenticated WebDAV path. SFTP hits the
# Data Store directly and avoids it. Listing still uses WebDAV/PROPFIND,
# since that part was never affected -- only file content downloads were.)
#
# Requires: httr, xml2, ssh
# install.packages(c("httr", "xml2", "ssh"))

library(httr)
library(xml2)
library(ssh)

webdav_url  <- Sys.getenv("CYVERSE_URL")
webdav_user <- Sys.getenv("CYVERSE_USER")
webdav_pass <- Sys.getenv("CYVERSE_PASSWORD")

# PROPFIND is the WebDAV method for listing directory contents.
# Depth: 1 means "this directory and its immediate children" (not recursive).
response <- VERB(
  verb = "PROPFIND",
  url = webdav_url,
  authenticate(webdav_user, webdav_pass),
  add_headers(Depth = "1"),
  content_type_xml()
)

stop_for_status(response)

# Parse the multistatus XML response
xml_body <- content(response, as = "text", encoding = "UTF-8")
doc <- read_xml(xml_body)

# Match elements by local-name() so this works regardless of the namespace
# prefix the server happens to use (D:, DAV:, lp1: etc. all map to "DAV:").
entries <- xml_find_all(doc, ".//*[local-name()='response']")
hrefs <- xml_text(xml_find_first(entries, ".//*[local-name()='href']"))

# Decode URL-encoded characters (e.g. %20 -> space) and strip the base path
decoded <- URLdecode(hrefs)
names_only <- basename(sub("/$", "", decoded))

# Drop the entry that refers to the directory itself
names_only <- names_only[nzchar(names_only)]
base_name <- basename(sub("/$", "", URLdecode(webdav_url)))
names_only <- names_only[names_only != base_name]

cat("Files/folders in directory:\n")
print(names_only)

# Filter to just the xlsx files fromlisting
xlsx_files <- names_only[grepl("\\.xlsx$", names_only, ignore.case = TRUE)]

# Remove any duplicated file names with ._ or * or ~ at the beginning
xlsx_files <- xlsx_files[!grepl("^\\._", xlsx_files)]
xlsx_files <- xlsx_files[!grepl("^\\*", xlsx_files)]
xlsx_files <- xlsx_files[!grepl("^\\~", xlsx_files)]

# Remove combined results
xlsx_files <- xlsx_files[!grepl("Combined", xlsx_files)]

# ---------------------------------------------------------------------------
# Download via SFTP instead of WebDAV GET.
#
# The Data Store exposes the same tree under a different root over SFTP:
#   WebDAV: https://data.cyverse.org/dav/iplant/projects/PAN/PAN-Data/...
#   SFTP:   /shared/PAN/PAN-Data/...
# Community/project data lives under /shared/<project>/<name>/... via SFTP,
# not under /iplant/projects/... (that's the internal iRODS path, not what
# the SFTP gateway exposes) and not under your personal /<username> home.
# ---------------------------------------------------------------------------
webdav_path <- sub("^https?://[^/]+/dav(-anon)?", "", URLdecode(webdav_url))
sftp_dir <- sub("^/iplant/projects", "/shared", webdav_path)
cat("\nSFTP directory resolved to:", sftp_dir, "\n")

# Reconnect per file: the ssh package's scp_download() does not reliably
# support multiple downloads over one reused session (libssh SCP channel
# quirk) -- a fresh session per file avoids that at low cost for files
# this size.
xlsx_data <- lapply(xlsx_files, function(fname) {
  remote_path <- paste0(sftp_dir, "/", fname)
  cat("Downloading via SFTP:", remote_path, "\n")
  session <- ssh_connect(host = paste0(webdav_user, "@data.cyverse.org"), passwd = webdav_pass, verbose = 0)
  scp_download(session, remote_path, to = ".", verbose = FALSE)
  ssh_disconnect(session)
  openxlsx::read.xlsx(fname, sheet = "Sample Data")
})

lod_data <- lapply(xlsx_files, function(fname) {
  openxlsx::read.xlsx(fname, sheet = "Imputed ND values")
})

names(lod_data) <- xlsx_files
names(xlsx_data) <- xlsx_files

cat("\nAll files read successfully.\n")

save(lod_data, file = "lod_data.Rdata")
save(xlsx_data, file = "xlsx_data.Rdata")
