#!/usr/bin/env python3
"""Remove repo-internal pages from a built pkgdown site.

pkgdown renders every ``*.md`` in the package root into the site (see
``pkgdown:::package_mds``) and does not consult ``.Rbuildignore``, so files like
``CLAUDE.md`` — guidance for people and agents working in the repo, not
documentation for users of the package — end up published. This drops those
pages and the sitemap and search-index entries that point at them, so the
published site has no dangling references.

Usage: strip_internal_pages.py <site-dir> <stem> [<stem> ...]
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path


def strip(site: Path, stems: list[str]) -> int:
    removed: list[str] = []

    for stem in stems:
        for suffix in (".html", ".md"):
            page = site / f"{stem}{suffix}"
            if page.exists():
                page.unlink()
                removed.append(page.name)

    sitemap = site / "sitemap.xml"
    if sitemap.exists():
        pattern = re.compile(
            r"^\s*<url><loc>[^<]*/(?:%s)\.html</loc></url>\s*$"
            % "|".join(re.escape(s) for s in stems)
        )
        kept = [ln for ln in sitemap.read_text().splitlines() if not pattern.match(ln)]
        sitemap.write_text("\n".join(kept) + "\n")

    search = site / "search.json"
    if search.exists():
        entries = json.loads(search.read_text())
        wanted = tuple(f"/{stem}.html" for stem in stems)
        kept_entries = [
            e for e in entries if not str(e.get("path", "")).endswith(wanted)
        ]
        search.write_text(json.dumps(kept_entries))
        print(
            f"search.json: dropped {len(entries) - len(kept_entries)} of {len(entries)} entries"
        )

    print("removed: " + (", ".join(removed) if removed else "nothing"))
    return 0


if __name__ == "__main__":
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    sys.exit(strip(Path(sys.argv[1]), sys.argv[2:]))
