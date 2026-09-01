<!--
Copyright © 2026 Michael Shields

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Dead URL or lost web page

1. Canonicalize the URL: remove tracking parameters and fragments; try HTTP/HTTPS,
   `www`/bare host, mobile or AMP variants, trailing slash, and obvious historical domain
   variants. Preserve a copy of the original URL for archive checks.
2. Search the page title, distinctive URL-tail components, author/site name, and short
   quoted fragments. Restrict to the original domain and plausible date range, then
   remove the restriction to find mirrors, quotations, or moved copies.
3. Check the Internet Archive for the exact URL. Inspect snapshots before redirects or
   error pages, alternate URL variants, and the archive's `URL/*` listing or CDX index
   (`web.archive.org/cdx/search/cdx`) for renamed or neighboring paths.
4. Search other reputable web archives, institutional mirrors, repositories, feeds, and
   syndicated copies. RSS text, citations, link posts, and quoted passages may recover
   content or identify a new canonical location.
5. Use internal URL structure, filenames, dates, CMS conventions, and outbound/inbound
   links as clues. If permitted tools can download a relevant public archive or site,
   search the local text with `rg` rather than browsing every file manually.
6. Verify a recovered page by title, author, date, content, and provenance. Clearly label
   partial captures, mirrors, excerpts, or reconstructions.

Prefer durable archive links in the final answer, but do not create snapshots or mirrors
without authorization.
