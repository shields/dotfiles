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

# Known paper, book, document, or dataset

Start with the strongest identifier available: DOI, ISBN, report/accession number,
catalog ID, exact title, author, or distinctive URL tail.

Escalate through these variants:

1. Exact full title; then title plus first author.
2. Remove subtitle and punctuation; split title and subtitle into separate quotes; trim
   words from the end; try a distinctive title fragment without quotes.
3. Add or remove author and year. Search a plausible date window around the claimed year
   because online, issue, conference, and publication dates can differ.
4. Try spelling, transliteration, initials, surname order, OCR, and typographic variants.
   Search DOI/ISBN/report number both with and without punctuation.
5. Search the containing journal volume, conference proceedings, anthology, report
   series, thesis repository, or book. The item may be embedded in a compilation PDF and
   absent as a standalone record.
6. Search specialist indexes, institutional repositories, author or lab pages, library
   catalogs, government archives, and `site:archive.org`; a normal web index and a
   scholarly index have different blind spots.
7. For a journal that changed publisher or platform, recover its old URL pattern from
   bibliographies (Beebe's TeX bibliographies record publisher PDF links), citing papers,
   or the Wayback Machine's CDX index; the old PDF often survives in the archive after the
   live link dies. Use the archive tactics in [lost-page.md](lost-page.md).
8. On OJS sites, `<meta name="citation_pdf_url">` gives the direct PDF, and some journals
   additionally attach a whole-issue scan as an issue galley, linked from the issue's
   table of contents (`/issue/view/<issueId>/<galleyId>`, downloadable at
   `/issue/download/<issueId>/<galleyId>`); downloading either may require a `Referer`
   header.
9. Inspect near matches and metadata-only records for alternate titles, editions,
   identifiers, or a better citation even when they do not provide full text.

Confirm the artifact using multiple discriminating fields: author, title, date, venue,
edition/version, page range, identifier, and content. Do not substitute a preprint,
abstract, later edition, or similarly titled item without labeling the difference.

If only lawful restricted access exists, provide the verified citation and access route
(publisher, catalog, library, interlibrary loan, or author request) without implying the
full text was reviewed.

## Verify the copy

A file that carries the right title is not yet the right copy. Before delivering a
downloaded document, check:

- page count against the cited page range, and the first and last printed folios;
- in a compilation scan, that the items before and after bracket the cited range;
- figure, table, and footnote numbering is complete and text runs continuously across
  page breaks (plates are often unnumbered, so folio gaps alone do not prove missing
  pages);
- edition, version, and preprint status, and producer from `pdfinfo`; judge OCR quality
  by skimming the extracted text for garbling;
- the file's SHA-256 and exact source URL, for the record.

Render the pages in question to images (`pdftoppm`) and look at them when text extraction
is ambiguous.
