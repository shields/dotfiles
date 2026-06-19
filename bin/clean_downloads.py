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
import errno
import os
import shutil
import sys
import time
from pathlib import Path


def pluralize(count: float, word: str) -> str:
    # argparse passes --days through float(), so a whole number arrives as e.g.
    # 7.0; render it as "7" so messages read "7 days" rather than "7.0 days".
    shown = int(count) if isinstance(count, float) and count.is_integer() else count
    return f"{shown} {word}" if count == 1 else f"{shown} {word}s"


def _move_to_trash(item: Path, trash_dir: Path) -> Path:
    """Move item into trash_dir under a free name, never overwriting one.

    For regular files this claims the name with os.link — an atomic
    compare-and-swap that raises FileExistsError if the name is already taken
    — then unlinks the source, closing the check-then-move race. Directories,
    symlinks, and cross-device moves cannot be hardlinked, so they fall back to
    a check-then-rename with a small residual window, which is acceptable for a
    single-user local Downloads cleanup.
    """
    base = item.stem
    suffix = item.suffix
    counter = 0
    while True:
        name = item.name if counter == 0 else f"{base}_{counter}{suffix}"
        dest = trash_dir / name
        counter += 1
        if item.is_dir() or item.is_symlink():
            if dest.exists():
                continue
            _ = shutil.move(str(item), str(dest))
            return dest
        try:
            os.link(item, dest)
        except FileExistsError:
            continue
        except OSError as exc:
            if exc.errno != errno.EXDEV:
                raise
            # Different filesystem: hardlinking is impossible, fall back.
            if dest.exists():
                continue
            _ = shutil.move(str(item), str(dest))
            return dest
        item.unlink()
        return dest


def move_old_downloads_to_trash(days: float = 7, *, dry_run: bool = False) -> None:
    downloads_dir = Path.home() / "Downloads"
    trash_dir = Path.home() / ".Trash"

    if not downloads_dir.exists():
        msg = f"Downloads directory not found: {downloads_dir}"
        raise FileNotFoundError(msg)

    if not trash_dir.exists():
        msg = f"Trash directory not found: {trash_dir}"
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
                dest = _move_to_trash(item, trash_dir)
                shown = item.name
                if dest.name != item.name:
                    shown = f"{item.name} -> {dest.name}"
                print(f"Moved to trash: {shown} (age: {age_days:.1f} days)")
        except OSError as exc:
            print(f"Skipping (cannot move): {item.name}: {exc}", file=sys.stderr)
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
