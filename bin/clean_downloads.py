#!/opt/homebrew/bin/python3

import argparse
import shutil
import time
from pathlib import Path


def pluralize(count: float, word: str) -> str:
    return f"{count} {word}" if count == 1 else f"{count} {word}s"


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
        if item.stat().st_mtime >= cutoff_time:
            continue

        dest = trash_dir / item.name

        # Handle name conflicts in trash
        if dest.exists():
            base = dest.stem
            suffix = dest.suffix
            counter = 1
            while dest.exists():
                dest = trash_dir / f"{base}_{counter}{suffix}"
                counter += 1

        age_days = (time.time() - item.stat().st_mtime) / (24 * 60 * 60)

        if dry_run:
            print(f"Would move: {item.name} (age: {age_days:.1f} days)")
        else:
            _ = shutil.move(str(item), str(dest))
            print(f"Moved to trash: {item.name} (age: {age_days:.1f} days)")

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
