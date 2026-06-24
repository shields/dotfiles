#!/opt/homebrew/bin/python3

# Copyright © 2025-2026 Michael Shields
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import argparse
import os
import subprocess
import sys
import time
from pathlib import Path

TRASH = "/usr/bin/trash"


def pluralize(count: float, word: str) -> str:
    # argparse passes --days through float(), so a whole number arrives as e.g.
    # 7.0; render it as "7" so messages read "7 days" rather than "7.0 days".
    shown = int(count) if isinstance(count, float) and count.is_integer() else count
    return f"{shown} {word}" if count == 1 else f"{shown} {word}s"


def _trash(item: Path) -> str:
    """Move item to the user trash with /usr/bin/trash, returning the name it
    landed under (the tool renames on collision rather than overwriting).

    The macOS trash tool owns the trash folder and records "Put Back" info, and
    picks a free name itself, so there is no name-collision race to guard here.
    """
    result = subprocess.run(
        [TRASH, "-v", str(item)],
        capture_output=True,
        text=True,
        check=True,
    )
    # Verbose output is `# Moved "<src>" to "<dest>"` on stdout. Anchor on the
    # source path we passed (echoed back verbatim) rather than splitting on the
    # `" to "` separator, so a name that itself contains `" to "` or a trailing
    # quote can't skew the result. Best-effort: fall back to the original name
    # if the tool ever rewrites the path or changes the format.
    line = result.stdout.strip()
    prefix = f'# Moved "{item}" to "'
    if line.startswith(prefix) and line.endswith('"'):
        return Path(line[len(prefix) : -1]).name
    return item.name


def move_old_downloads_to_trash(days: float = 7, *, dry_run: bool = False) -> None:
    downloads_dir = Path.home() / "Downloads"

    if not downloads_dir.exists():
        msg = f"Downloads directory not found: {downloads_dir}"
        raise FileNotFoundError(msg)

    if not os.access(TRASH, os.X_OK):
        msg = f"Trash tool not found or not executable: {TRASH}"
        raise FileNotFoundError(msg)

    cutoff_time = time.time() - (days * 24 * 60 * 60)
    moved_count = 0

    if dry_run:
        print(f"DRY RUN - would move {pluralize(days, 'day')}-old files to trash:\n")

    for item in downloads_dir.iterdir():
        # Use lstat so a broken symlink (whose target is gone) is still
        # considered by its own mtime instead of raising and aborting the run.
        try:
            mtime = item.stat(follow_symlinks=False).st_mtime
        except OSError as exc:
            print(f"Skipping (cannot stat): {item.name}: {exc}", file=sys.stderr)
            continue

        if mtime >= cutoff_time:
            continue

        age_days = (time.time() - mtime) / (24 * 60 * 60)

        try:
            if dry_run:
                print(f"Would move: {item.name} (age: {age_days:.1f} days)")
            else:
                dest_name = _trash(item)
                shown = item.name
                if dest_name != item.name:
                    shown = f"{item.name} -> {dest_name}"
                print(f"Moved to trash: {shown} (age: {age_days:.1f} days)")
        except (OSError, subprocess.CalledProcessError) as exc:
            # A non-zero exit carries trash's own message on stderr; failing to
            # even spawn it (a vanished binary, a fork failure) surfaces as a
            # plain OSError. Either way, skip this item and keep going rather
            # than aborting the whole cleanup.
            if isinstance(exc, subprocess.CalledProcessError):
                stderr: str = exc.stderr or ""  # pyright: ignore[reportAny]
                err = stderr.strip() or f"exit status {exc.returncode}"
            else:
                err = str(exc)
            print(f"Skipping (cannot move): {item.name}: {err}", file=sys.stderr)
            continue

        moved_count += 1

    if moved_count == 0:
        files_text = pluralize(days, "file")
        days_text = pluralize(days, "day")
        print(f"No {files_text} older than {days_text} found in Downloads")
    else:
        action = "Would move" if dry_run else "Moved"
        print(f"\n{action} {pluralize(moved_count, 'item')} to trash")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Move old files from Downloads to Trash",
    )
    _ = parser.add_argument(
        "-d",
        "--days",
        type=float,
        default=7,
        help="Number of days old a file must be to move (default: 7)",
    )
    _ = parser.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="Show what would be moved without actually moving",
    )

    args = parser.parse_args()
    move_old_downloads_to_trash(args.days, dry_run=args.dry_run)  # pyright: ignore[reportAny]
