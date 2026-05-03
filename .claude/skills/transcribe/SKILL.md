---
name: transcribe
description: Transcribe local audio files to speaker-attributed text using whisperkit-cli on Apple Silicon. Converts to 16kHz mono WAV, runs Whisper large-v3-turbo with diarization, parses the RTTM-format output into `[Speaker A 0:19] ...` paragraphs, fmt-wraps. Trigger when the user asks to transcribe audio, a podcast, an episode, or a recording.
---

# Transcribe audio with whisperkit-cli

Three non-obvious things bite if you skip them, called out below.

These snippets are run via Claude's Bash tool with `$f` set by the caller (an audio basename without extension); they're not standalone shell scripts, so the project's `set -euo pipefail` and absolute-path conventions for `.sh` files don't apply. Section 2 still adds `set -o pipefail` because `sed`/`tr` would otherwise mask a whisperkit-cli failure (see the note there).

## 1. Convert to 16kHz mono WAV

Required even though `--audio-path` accepts MP3 in the help text — AVAudioFile crashes seeking MP3s (`setFramePosition` error 1718449215, exits with an Objective-C trace).

`$f` is used throughout as the basename without extension. Intermediates use distinct extensions (`.16khz.wav`, `.raw.txt`) so the conversion can't clobber a user-supplied WAV with the same basename, and the final output goes to `$f.transcript.txt` so it can't clobber user notes at `$f.txt`. The example uses `.mp3` since that's the common case; for `.m4a`/`.opus`/other inputs, substitute the actual extension. `./` prefixes guard against basenames that start with `-`.

```bash
ffmpeg -i "./$f.mp3" -ar 16000 -ac 1 -c:a pcm_s16le -y "./$f.16khz.wav"
```

## 2. Transcribe with diarization, sandbox off

Run **only the whisperkit-cli call** with **`dangerouslyDisableSandbox: true`** — leave ffmpeg in section 1 sandboxed so its MP3-parsing surface stays contained. WhisperKit writes to several caches the sandbox blocks (ANE bundle cache, MPSGraph T/ and C/). Without this, the model runs to completion and exits 0 but produces token-level gibberish ("char ca foldinch Aust…") with no error — sanity-check the first ~30s of output.

**Trust requirement**: with sandbox off, an exploit in WhisperKit's WAV-decoding path would run unrestricted on the host. Only transcribe audio from sources you trust. ffmpeg's MP3 parsing in section 1 stays inside the default sandbox, so the larger attack surface is contained — that's the only mitigation.

Sequential. `--concurrent-worker-count` with multiple `--audio-path` appears to hang silently.

`set -o pipefail` matters: without it, a whisperkit-cli failure leaves an empty or truncated `$f.raw.txt` while `sed`/`tr` still exit 0. The progress line whisperkit-cli prints to stdout is harmless — the awk parser in section 3 only matches `^SPEAKER ` lines, so non-transcript output is ignored without a `grep` filter.

```bash
set -o pipefail
whisperkit-cli transcribe --audio-path "./$f.16khz.wav" \
  --language en --diarization 2> "$f.err.log" \
  | sed $'s/\x1b\\[[0-9;]*[a-zA-Z]//g' | tr '\r' '\n' \
  > "$f.raw.txt"
```

(Stderr → `$f.err.log` keeps the progress bar out of the conversation but preserves any error text for diagnosis. It's removed in section 3 on success.)

(First run downloads `large-v3-turbo` to `~/Documents/huggingface/models/argmaxinc/whisperkit-coreml/` — Argmax's hardcoded default. Subsequent runs reuse it.)

For batches, wrap in a `for` loop with `=== START $f` / `=== DONE $f` markers and Monitor on those — ~5 min per 1-hour episode with diarization.

## 3. Parse RTTM into speaker paragraphs

Diarization output is stdout-only (not in `--report` JSON). Each turn is one line:

```
SPEAKER <base> 1 <start> <duration> <text...> <NA> <speaker> <NA> <NA>
```

The parser anchors on the `.16khz.wav` field so `$f` can contain spaces (e.g., `Episode 123`) — hardcoded field indices would shift when awk splits the base into multiple fields. After the base comes channel, start, duration, text, then a fixed `<NA> <speaker> <NA> <NA>` suffix.

```bash
awk '
  /^---- Speaker Diarization Results ----/ { in_diar=1; next }
  in_diar && /^SPEAKER / {
    base_end=0
    for (i=2; i<=NF; i++) if ($i ~ /\.16khz\.wav$/) { base_end=i; break }
    if (!base_end) next
    start=$(base_end+2); m=int(start/60); s=int(start)%60
    text=""
    for (i=base_end+4; i<=NF-4; i++) text = text (text==""?"":" ") $i
    spk=$(NF-2)
    printf "[Speaker %s %d:%02d] %s\n\n", spk, m, s, text
  }
' < "$f.raw.txt" | fmt > "$f.transcript.txt"
rm -- "$f.raw.txt" "$f.16khz.wav" "$f.err.log"
```

Speaker labels are anonymous (A/B/C…); diarization doesn't infer identities.
