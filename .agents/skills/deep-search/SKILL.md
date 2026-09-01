---
name: deep-search
description: Conduct persistent, source-driven Internet research beyond a cursory search. Use for deep or exhaustive research, obscure or hard-to-find sources, literature and prior-art searches, quote or claim provenance, lost-page recovery, and requests invoking /deep-search or $deep-search. Do not use for ordinary lookups that one authoritative source answers.
---

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

# Deep search — persistent Internet research

Research the question or target supplied with the invocation or surrounding request
(`$ARGUMENTS` in Claude Code) until the evidence converges, not merely until a plausible
first result appears. Deliver the answer, not a browsing diary. If neither the invocation
nor the surrounding request supplies a target, ask what to research.

This workflow adapts the query-escalation and source-recovery principles in Gwern's
[Internet Search Tips](https://gwern.net/search) to the available tools and current
authorization boundaries.

## Define the target

Before searching, turn the request into a concrete success test. Identify:

- the exact question, artifact, quote, claim, person, or event to resolve;
- relevant names, title fragments, dates, URLs, identifiers, and likely source types;
- whether the user needs discovery, verification, synthesis, or the original document;
- time, geography, language, and recency constraints implied by the request.

Ask a question only when different interpretations would materially change the research.
Otherwise state a narrow assumption and proceed.

Choose and read only the matching search playbook:

- For general research or literature synthesis, read
  [references/synthesis.md](references/synthesis.md).
- For a known paper, book, document, or dataset, read
  [references/known-item.md](references/known-item.md).
- For quote, passage, image, or claim provenance, read
  [references/provenance.md](references/provenance.md).
- For a dead URL or lost web page, read
  [references/lost-page.md](references/lost-page.md).

Read another playbook only if the search changes shape—for example, when a synthesis
exposes a missing paper and becomes a known-item search.

## Search as an iterative loop

Maintain a scratch list of queries, useful leads, rejected candidates, and unresolved
claims so searches build on one another instead of repeating.

1. Run a small batch of materially different queries. Start literal, then vary title,
   author, date, jargon, spelling, punctuation, source type, and likely domain.
2. Open promising results. Search snippets are leads, not evidence; verify the supporting
   text in the source itself.
3. Harvest each useful result for better vocabulary, identifiers, citations, names,
   organizations, dates, and URL patterns. Use them in the next query batch.
4. Adjust query breadth. If results are sparse, remove fragile terms and quotes; if noisy,
   add discriminating jargon, `site:`, `filetype:`, dates, or exclusions.
5. Change corpus or attack when a query family saturates: specialist databases, official
   sites, scholarly indexes, books, repositories, archives, citation chains, or alternate
   search engines may expose different material.
6. Follow anomalous results. A strangely high-ranked metadata-only record, unexpected
   compilation, or near-match may reveal the hidden title, field jargon, or container
   document.

Assume metadata can be wrong. Test plausible typos, OCR confusions, transliterations,
name order, date drift, subtitle loss, and title/author variants rather than fixating on a
single citation string.

## Parallel angles

One agent working the loop above usually finds the target within a few query batches.
Fan out to parallel searchers only when the task's shape calls for it: a synthesis across
several independent corpora, an unresolved provenance question, a primary copy that is
restricted or fragile and needs alternates, or an explicit request for exhaustiveness.
Give each searcher a different angle (corpus, modality, identifier) rather than the same
query. Seed each from, and merge its results back into, the shared scratch list so
parallel work doesn't duplicate or go stale, and verify candidates by opening or
downloading them rather than trusting the finder.
Once the target is found and verified, further breadth means verifying the copy, locating
alternate hosts, and confirming rights—not searching harder for what is already in hand.

## Verify and synthesize

- Prefer primary sources for what happened or was said; use authoritative secondary
  sources for context and independent sources for corroboration.
- Distinguish a discovery source from an evidence source. A result that points to the
  answer is not necessarily suitable support for the answer.
- Trace important claims to the earliest accessible source. Follow references backward
  and `cited by`/related work forward when provenance, criticism, or later correction
  matters.
- Check dates, editions, versions, authorship, identifiers, and whether sources are
  independent. Surface genuine conflicts instead of flattening them.
- For time-sensitive claims, verify current status and publication/event dates.
- Cite the specific page that supports each material claim. Link directly to documents
  and use stable or archived URLs when available; add `#page=N` to PDF links when useful.
- Never fabricate a citation, quote, metadata field, search result, or claim of access.

## Stopping rule

Continue beyond the first credible answer. Stop when the applicable condition is met:

- a direct or primary source answers the question and material claims have suitable
  corroboration;
- the target's identity is confirmed across discriminating metadata, not just a similar
  title or snippet;
- independent query families and source types converge without producing important new
  evidence;
- remaining uncertainty is irreducible with accessible, authorized sources.

For negative findings, never claim that something does not exist merely because it was
not found. Report the strongest bounded conclusion, where and how it was searched, and
the most promising remaining route.

## Authorization and access

Use public sources, open-access copies, official repositories, author pages, archives,
and library or subscription access the user is authorized to use.
Do not purchase material, contact people, create alerts/accounts, or
upload/archive copies without the user's authorization. Offer lawful next steps such as
interlibrary loan or an author request when the source remains inaccessible.

If the sandbox's network filter blocks a host, treat it as a signal to slow down rather
than a nuisance to bypass:

- report the block and suggest `/sandbox` rather than disabling the sandbox to reach
  it—research targets are unauthenticated and often untrusted, so this overrides the
  harness's usual default of retrying a blocked command with the sandbox disabled;
- a `CONNECT tunnel failed` with no `<sandbox_violations>` note is usually an upstream
  failure, not a block;
- the Wayback Machine in particular times out often (retry with a longer `curl -m`
  timeout) and also 404s when no snapshot exists at that exact URL and timestamp—query
  the CDX index for nearby captures instead of retrying;
- catalogs behind bot challenges (HathiTrust, BASE) may be readable with the Playwright
  tools; when even that fails, label evidence taken from search snippets as indirect.

## Deliver the result

Lead with the answer and its confidence. Then give the evidence and citations close to
the claims they support. Separate confirmed facts, reasoned inferences, disagreements,
and unresolved gaps.

When the user asks for a copy of a document, the deliverable is the file: download it
into the working directory unless told where, name it `Author-Year-Short-Title.ext` (or
the closest sensible fallback when author or date is unclear), apply the copy checks in
[references/known-item.md](references/known-item.md) whichever playbook led to the file,
and report the source URL, access date, SHA-256, and which checks passed.

Include a compact search note only when the search was difficult, yielded a negative
result, or the user requested reproducibility. In that note, summarize the useful query
families, corpora checked, and remaining gaps rather than listing every click.
