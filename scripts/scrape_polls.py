"""
Scrape POLITICO's "Poll of Polls" (https://www.politico.eu/europe-poll-of-polls/<country>/)
for a set of European countries and build a party-level panel with:
  - polling numbers closest to April 2022 / April 2023 / April 2024 / April 2025 / April 2026
  - results of any national election held since Jan 2022
  - ches_id: matched against the party-abbreviation tables on pages 3-17 of
    data_raw/1999-2024_CHES_codebook.pdf (parsed with pdftotext), by comparing POLITICO's
    polling code / native name / English name to each country's abbreviations (tier 1,
    exact/near-exact) and, failing that, to the tables' native/English party names
    (tier 2, token-overlap, same threshold/margin scheme as new_q59 below).
  - eu_russia, joined on ches_id == party_id: primarily from
    data_raw/1999-2024_CHES_dataset_meansV2.csv (year == 2024, the only year the
    variable was fielded); if a matched party has no value there, fall back to the
    EU_Russia column of data_raw/CHES_Ukraine_March_2024.dta. If still missing, "NA"
    -- never guessed via name/abbreviation matching.
  - new_q59, matched against the [New_Q59] party-choice table embedded in
    data_raw/Copy of script 29.04.2025.docx (a two-column Word table interleaving two
    countries' party lists per row; parsed from the raw document.xml so the two columns
    aren't flattened into one interleaved stream), by the same name-matching scheme.

POLITICO does not expose the underlying poll-of-polls data in the static page HTML.
Each country page loads a small D3 widget (".pop-d3") that fetches its data at run time
from a JSON endpoint:

    https://www.politico.eu/wp-json/politico/v1/poll-of-polls/<CODE>

where <CODE> is e.g. "DE-parliament", "FR-parliament", "GB-parliament", etc. (found by
inspecting the `data-code` attribute of the .pop-d3 <div> on each country page, and cross
-referenced against the theme's bundled JS, which calls
`window.pollOfPollData.endpoint + code`). That JSON contains the full poll history
(`polls`), a Kalman-smoothed daily trend (`trends.kalmanSmooth`) and past election
results (`results`), all keyed by a short internal party code (e.g. "AfD", "Union").

The page additionally renders a `<ul class="polls-details">` block with the party's
English name, native-language name and logo file slug, but only for parties currently
shown on the chart (not necessarily using the same code as the polling data, since
POLITICO renames/relabels coalitions over time). We recover English/native names by
matching the JSON party codes to the HTML party entries in two passes:
  1. exact/substring match between the polling code and the logo-image slug
  2. positional match, ranking both lists by most-recent vote share (handles rebrands
     such as Poland's PiS -> "Law and Justice/United Right" logo slug "ZP")
Any code that still can't be matched keeps the (short) label from the JSON `parties`
dict as its English name and an empty native name.

Output: data_clean/politico_polls.csv
"""
from __future__ import annotations

import html
import re
import subprocess
import time
import unicodedata
import zipfile
from bisect import bisect_right
from datetime import date
from pathlib import Path

import pandas as pd
import requests

ROOT = Path(__file__).resolve().parent.parent
DATA_RAW = ROOT / "data_raw"
DATA_CLEAN = ROOT / "data_clean"
CODEBOOK_PDF = DATA_RAW / "1999-2024_CHES_codebook.pdf"
Q59_DOCX = DATA_RAW / "Copy of script 29.04.2025.docx"

HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; academic-research-scraper/1.0)"}
SESSION = requests.Session()
SESSION.headers.update(HEADERS)

# country display name -> (URL slug, POLITICO poll-of-polls chart code)
# codes were identified from the `data-code` attribute of the .pop-d3 div on each
# country's page (the "main" national-parliament vote-intention chart; countries also
# carry an "EU-parliament" widget and, in a few cases, presidential/regional widgets
# which are not used here).
COUNTRIES = {
    "Croatia": ("croatia", "HR-parliament"),
    "Denmark": ("denmark", "DK-parliament"),
    "Finland": ("finland", "FI-parliament"),
    "France": ("france", "FR-parliament"),
    "Germany": ("germany", "DE-parliament"),
    "Greece": ("greece", "GR-parliament"),
    "Hungary": ("hungary", "HU-parliament"),
    "Italy": ("italy", "IT-parliament"),
    "Lithuania": ("lithuania", "LT-parliament"),
    "Netherlands": ("netherlands", "NL-parliament-p"),
    "Poland": ("poland", "PL-parliament"),
    "Romania": ("romania", "RO-parliament"),
    "Slovakia": ("slovakia", "SK-parliament"),
    "Spain": ("spain", "ES-parliament"),
    "Sweden": ("sweden", "SE-parliament"),
    "UK": ("united-kingdom", "GB-parliament"),
    "Austria": ("austria", "AT-parliament"),
    "Belgium": ("belgium", "BE-parliament"),
    "Bulgaria": ("bulgaria", "BG-parliament"),
    "Czech Republic": ("czech-republic", "CZ-parliament"),
    "Ireland": ("ireland", "IE-parliament"),
    "Portugal": ("portugal", "PT-parliament"),
}

# our country name -> country abbreviation used in the CHES codebook's party table
# (data_raw/1999-2024_CHES_codebook.pdf, pages 3-17) and in the [New_Q59] table of
# data_raw/Copy of script 29.04.2025.docx
COUNTRY_TO_CODEBOOK = {
    "Croatia": "CRO",
    "Denmark": "DK",
    "Finland": "FIN",
    "France": "FR",
    "Germany": "GE",
    "Greece": "GR",
    "Hungary": "HUNG",
    "Italy": "IT",
    "Lithuania": "LITH",
    "Netherlands": "NL",
    "Poland": "POL",
    "Romania": "ROM",
    "Slovakia": "SLO",
    "Spain": "ESP",
    "Sweden": "SV",
    "UK": "UK",
    "Austria": "AUS",
    "Belgium": "BE",
    "Bulgaria": "BUL",
    "Czech Republic": "CZECH",
    "Ireland": "IRL",
    "Portugal": "POR",
}

TARGET_MONTHS = {
    "poll_2022_04": date(2022, 4, 15),
    "poll_2023_04": date(2023, 4, 15),
    "poll_2024_04": date(2024, 4, 15),
    "poll_2025_04": date(2025, 4, 15),
    "poll_2026_04": date(2026, 4, 15),
}
MAX_GAP_DAYS = 45  # don't extrapolate a trend value further than this from a target date

ELECTION_WINDOW = (date(2022, 1, 1), date.today())

POP_ENDPOINT = "https://www.politico.eu/wp-json/politico/v1/poll-of-polls/"


# ---------------------------------------------------------------------------
# CHES codebook (party-ID <-> abbreviation table), pages 3-17
# ---------------------------------------------------------------------------

_CODEBOOK_HEADER_RE = re.compile(r"Country\s+Party ID\s+Party Abbrev\s+Party Name")
_CODEBOOK_TOKEN_RE = re.compile(r"\S+(?: \S+)*")
_CODEBOOK_FUDGE = 3  # tolerance (chars) for column-boundary drift on some pages


def parse_ches_codebook(pdf_path: Path = CODEBOOK_PDF, first: int = 3, last: int = 17) -> dict[str, list[dict]]:
    """Parse the "PARTY = party abbreviation" table (pages 3-17) into
    {country_abbrev: [{party_id, abbrevs, native, english}, ...]}.

    pdftotext -layout renders the table as fixed-width columns, but the column widths
    are recomputed per page (and drift further on lines containing combining-diacritic
    characters), so header-derived column boundaries are only approximate. We tokenize
    each line on runs of 2+ spaces (a field never contains a run of 2+ spaces itself,
    since multi-word phrases are single-spaced) and classify each token by the zone its
    *start position* falls in, with a small tolerance -- this survives the column drift
    that breaks naive fixed-width slicing.
    """
    text = subprocess.run(
        ["pdftotext", "-layout", "-f", str(first), "-l", str(last), str(pdf_path), "-"],
        capture_output=True, text=True, check=True,
    ).stdout

    countries: dict[str, list[dict]] = {}
    current_country = None
    current_party = None

    def flush():
        nonlocal current_party
        if current_party is not None:
            countries.setdefault(current_party["country"], []).append(current_party)
        current_party = None

    for page in text.split("\x0c"):
        lines = page.split("\n")
        header_idx = next((i for i, l in enumerate(lines) if _CODEBOOK_HEADER_RE.search(l)), None)
        if header_idx is None:
            continue
        header = lines[header_idx]
        boundaries = [
            header.index("Country"), header.index("Party ID"),
            header.index("Party Abbrev"), header.index("Party Name"), header.rindex("Party Name"),
        ]
        zone_names = ["country", "pid", "abbrev", "name", "english"]

        def zone_of(pos: int) -> str:
            idx = max(0, min(bisect_right(boundaries, pos + _CODEBOOK_FUDGE) - 1, len(zone_names) - 1))
            return zone_names[idx]

        for line in lines[header_idx + 1:]:
            if not line.strip() or "Continued on next page" in line or "PARTY = party abbreviation" in line:
                continue
            if re.fullmatch(r"\s*\d+\s*", line):
                continue  # page-number footer

            row = {"country": None, "pid": None, "abbrev": [], "name": [], "english": []}
            for m in _CODEBOOK_TOKEN_RE.finditer(line):
                tok, pos = m.group(0), m.start()
                zone = zone_of(pos)
                if zone == "country":
                    row["country"] = tok
                elif zone == "pid":
                    row["pid"] = tok
                else:
                    row[zone].append(tok)

            if row["pid"] and row["pid"].isdigit():
                flush()
                # a page-number footer occasionally lands in-line with the next row's
                # "country" zone (a pdftotext quirk); only real (alphabetic) country
                # labels should ever start a new country group
                if row["country"] and row["country"].isalpha():
                    current_country = row["country"]
                current_party = {
                    "country": current_country,
                    "party_id": int(row["pid"]),
                    "abbrevs": list(row["abbrev"]),
                    "native": list(row["name"]),
                    "english": list(row["english"]),
                }
            elif current_party is not None:
                current_party["abbrevs"].extend(row["abbrev"])
                current_party["native"].extend(row["name"])
                current_party["english"].extend(row["english"])
        flush()
    return countries


def normalize_code(s: str) -> str:
    """Strip trailing parenthetical annotations POLITICO adds to codes/labels,
    e.g. "RV (B)" / "NB (D)(-2024)" / "KH (-2024)" -> "RV" / "NB" / "KH"."""
    return re.sub(r"\s*\(.*", "", str(s)).strip()


def normalize_abbrev(s: str) -> str:
    """Fold to bare uppercase alphanumerics, stripping accents (NFKD) so e.g.
    "KDU-ČSL" and the codebook's unaccented "KDU-CSL" compare equal."""
    s = unicodedata.normalize("NFKD", str(s))
    s = "".join(c for c in s if not unicodedata.combining(c))
    return re.sub(r"[^A-Za-z0-9]", "", s).upper()


def build_codebook_index(countries: dict[str, list[dict]]) -> dict[str, dict]:
    """Per codebook country: {abbrev_index: {normalized_abbrev: party_id}, entries: [(party_id, combined_text)]}.

    A handful of abbreviations (e.g. Lithuania's "NS") were reused by two unrelated
    parties at different points in time; those are dropped from abbrev_index rather
    than guessed at, falling back to tier-2 name matching instead."""
    index: dict[str, dict] = {}
    for country, parties in countries.items():
        abbrev_to_ids: dict[str, set[int]] = {}
        entries = []
        for p in parties:
            for field in p["abbrevs"]:
                for a in field.split(";"):
                    na = normalize_abbrev(a)
                    if len(na) >= 2:
                        abbrev_to_ids.setdefault(na, set()).add(p["party_id"])
            combined = normalize_name(" ".join(p["native"] + p["english"]))
            entries.append((p["party_id"], combined))
        abbrev_index = {na: next(iter(ids)) for na, ids in abbrev_to_ids.items() if len(ids) == 1}
        index[country] = {"abbrev_index": abbrev_index, "entries": entries}
    return index


def _expand_candidates(candidates: list[str]) -> list[str]:
    """Alnum-normalize each candidate, plus its slash/comma/semicolon-separated
    sub-tokens (e.g. "EAJ/PNV" also yields "EAJ" and "PNV" individually)."""
    out: list[str] = []
    for cand in candidates:
        for part in [cand, *re.split(r"[/,;]", str(cand))]:
            na = normalize_abbrev(part)
            if len(na) >= 2 and na not in out:
                out.append(na)
    return out


def match_codebook_party_id(
    codebook_country_index: dict, exact_candidates: list[str], prefix_candidates: list[str],
    native: str, english: str, threshold: float = 0.5, margin: float = 0.05,
) -> int | None:
    """Tier 1: exact match of any candidate (see _expand_candidates) against the
    country's abbreviation table. Tier 2: unambiguous prefix match (candidate is a
    strict extension/contraction of a table abbreviation, e.g. "EH Bildu" -> "EHBILDU"
    against table abbreviation "EHB"), only if exactly one table abbreviation qualifies
    -- restricted to `prefix_candidates` (code-derived only) since running this on a
    full descriptive party name risks a false hit whenever that name happens to start
    with an unrelated party's short code (e.g. Slovakia's "OLaNO a priatelia" coalition
    name starting with "OLaNO" itself). Tier 3 (fallback): token-overlap of
    native/English name against the table's native/English text, same threshold/margin
    scheme as new_q59 matching."""
    abbrev_index = codebook_country_index["abbrev_index"]

    for na in _expand_candidates(exact_candidates):
        if na in abbrev_index:
            return abbrev_index[na]

    for na in _expand_candidates(prefix_candidates):
        if len(na) < 3:
            continue
        hits = {abbrev_index[k] for k in abbrev_index if len(k) >= 3 and (na.startswith(k) or k.startswith(na))}
        if len(hits) == 1:
            return next(iter(hits))

    entries = codebook_country_index["entries"]
    if not entries:
        return None
    query = normalize_name(f"{native} {english}")
    if not query.strip():
        return None
    scored = sorted(
        ((token_jaccard(query, text), pid) for pid, text in entries),
        key=lambda x: -x[0],
    )
    best_score, best_pid = scored[0]
    second_score = scored[1][0] if len(scored) > 1 else 0.0
    if best_score >= threshold and (best_score - second_score) >= margin:
        return best_pid
    return None


# ---------------------------------------------------------------------------
# [New_Q59] party-choice table, data_raw/Copy of script 29.04.2025.docx
# ---------------------------------------------------------------------------

# The table renders two countries' party lists side by side (4 cells/row: code, name,
# code, name). Boundaries below are the table-row index (0-based, counting from the
# first 4-cell row) at which each column switches to a new country -- located by
# anchoring on each country's first-listed party. Static: this is a fixed document.
_Q59_LEFT_BOUNDARIES = [
    (0, "UK"), (19, "France"), (29, "Germany"), (36, "Spain"), (47, "Italy"),
    (56, "Netherlands"), (71, "Denmark"), (83, "Sweden"), (91, "Finland"),
    (100, "Poland"), (107, "Romania"),
]
_Q59_RIGHT_BOUNDARIES = [
    (0, "Hungary"), (10, "Lithuania"), (25, "Greece"), (33, "Bulgaria"), (42, "Slovakia"),
    (53, "Croatia"), (62, "Belgium"), (75, "Austria"), (84, "Czech Republic"),
    (97, "Portugal"), (107, "Ireland"),
]


def _country_for_row(i: int, boundaries: list[tuple[int, str]]) -> str:
    idx = bisect_right([b for b, _ in boundaries], i) - 1
    return boundaries[max(idx, 0)][1]


def parse_q59_table(docx_path: Path = Q59_DOCX) -> dict[str, list[tuple[int, str]]]:
    with zipfile.ZipFile(docx_path) as z:
        xml = z.read("word/document.xml").decode("utf-8", errors="replace")

    marker = xml.find("New_Q59")
    tbl_start = xml.rfind("<w:tbl>", 0, marker)
    tbl_end = xml.find("</w:tbl>", marker) + len("</w:tbl>")
    seg = xml[tbl_start:tbl_end]

    row_re = re.compile(r"<w:tr\b.*?</w:tr>", re.S)
    cell_re = re.compile(r"<w:tc>.*?</w:tc>", re.S)
    text_re = re.compile(r"<w:t(?:>|\s[^>]*>)(.*?)</w:t>", re.S)

    party_rows = []
    for r in row_re.findall(seg):
        cells = cell_re.findall(r)
        if len(cells) != 4:
            continue
        texts = [html.unescape("".join(text_re.findall(c))) for c in cells]
        party_rows.append(texts)

    table: dict[str, list[tuple[int, str]]] = {}
    for i, (code_l, name_l, code_r, name_r) in enumerate(party_rows):
        for code_raw, name, boundaries in (
            (code_l, name_l, _Q59_LEFT_BOUNDARIES), (code_r, name_r, _Q59_RIGHT_BOUNDARIES),
        ):
            if "fixed" in code_raw:
                continue  # fixed "None of these" / "Don't know" options, not a party
            code = re.sub(r"[^0-9]", "", code_raw.split()[0]) if code_raw.strip() else ""
            if not code.isdigit():
                continue
            country = _country_for_row(i, boundaries)
            table.setdefault(country, []).append((int(code), name.strip()))
    return table


def build_q59_index(q59_table: dict[str, list[tuple[int, str]]]) -> dict[str, list[tuple[int, str]]]:
    return {country: [(code, normalize_name(name)) for code, name in entries] for country, entries in q59_table.items()}


def match_q59_code(
    q59_country_index: list[tuple[int, str]], native: str, english: str,
    threshold: float = 0.5, margin: float = 0.05,
) -> int | None:
    if not q59_country_index:
        return None
    query = normalize_name(f"{native} {english}")
    if not query.strip():
        return None
    scored = sorted(
        ((token_jaccard(query, text), code) for code, text in q59_country_index),
        key=lambda x: -x[0],
    )
    best_score, best_code = scored[0]
    second_score = scored[1][0] if len(scored) > 1 else 0.0
    if best_score >= threshold and (best_score - second_score) >= margin:
        return best_code
    return None


def fetch_country_page(slug: str) -> str:
    url = f"https://www.politico.eu/europe-poll-of-polls/{slug}/"
    r = SESSION.get(url, timeout=30)
    r.raise_for_status()
    return r.text


def fetch_poll_json(code: str) -> dict:
    r = SESSION.get(POP_ENDPOINT + code, timeout=60)
    r.raise_for_status()
    return r.json()


def parse_party_details(page_html: str) -> list[dict]:
    """Parse the <ul class="polls-details"> block: one entry per party currently
    shown on the chart, in on-page (= current ranking) order."""
    start = page_html.find('class="polls-details"')
    if start == -1:
        return []
    end = page_html.find("</ul>", start)
    block = page_html[start:end]
    items = re.findall(r'<li class="polls-details__item">(.*?)</li>', block, re.S)
    out = []
    for it in items:
        src = re.search(r'src="([^"]+)"', it)
        title = re.search(r'polls-details__title">([^<]+)<', it)
        subs = re.findall(r'polls-details__subtitle">([^<]*)<', it)
        slug = None
        if src:
            fname = src.group(1).split("/")[-1]
            slug = fname.rsplit(".", 1)[0]
        native = subs[0] if subs and not subs[0].startswith(("Leadership:", "EP affiliation:")) else ""
        out.append({
            "slug": slug or "",
            "english": html.unescape(title.group(1).strip()) if title else "",
            "native": html.unescape(native.strip()),
        })
    return out


def rank_codes_by_latest_value(data: dict) -> list[str]:
    """Order party codes by most recent known vote share, descending."""
    latest: dict[str, float] = {}
    for poll in data.get("polls", []):
        for code, val in poll.get("parties", {}).items():
            if val != "NA" and val is not None:
                latest[code] = val  # later polls overwrite earlier ones -> keeps most recent
    if not latest:
        trend = data.get("trends", {}).get("kalmanSmooth", [])
        if trend:
            latest = trend[-1].get("parties", {})
    return [c for c, _ in sorted(latest.items(), key=lambda kv: -kv[1])]


def match_party_names(html_items: list[dict], ranked_codes: list[str], labels: dict[str, str]) -> dict[str, dict]:
    """Best-effort match between POLITICO's short polling codes and the
    (English name, native name) shown in the page's party-details list."""
    unclaimed_html = list(range(len(html_items)))
    result: dict[str, dict] = {}

    # pass 1: slug <-> code textual match (case-insensitive substring either way)
    for code in ranked_codes:
        c = code.lower()
        best = None
        for i in unclaimed_html:
            slug = html_items[i]["slug"].lower()
            if not slug:
                continue
            if slug == c or slug.startswith(c) or c.startswith(slug) or slug in c or c in slug:
                best = i
                break
        if best is not None:
            result[code] = html_items[best]
            unclaimed_html.remove(best)

    # pass 2: positional match (both lists ranked by current vote share) for leftovers
    remaining_codes = [c for c in ranked_codes if c not in result]
    for code, i in zip(remaining_codes, list(unclaimed_html)):
        result[code] = html_items[i]
        unclaimed_html.remove(i)

    # fallback: whatever is still unmatched keeps the short JSON label, no native name
    for code in ranked_codes:
        if code not in result:
            result[code] = {"slug": "", "english": labels.get(code, code), "native": ""}
    return result


def nearest_trend_value(trend: list[dict], code: str, target: date, max_gap_days: int = MAX_GAP_DAYS):
    best_val, best_gap = None, None
    for point in trend:
        pval = point.get("parties", {}).get(code)
        if pval is None or pval == "NA":
            continue
        pdate = date.fromisoformat(point["date"])
        gap = abs((pdate - target).days)
        if best_gap is None or gap < best_gap:
            best_gap, best_val = gap, pval
    if best_val is not None and best_gap is not None and best_gap <= max_gap_days:
        return best_val
    return None


def elections_in_window(data: dict) -> list[tuple[str, dict]]:
    out = []
    for res in data.get("results", []):
        d = date.fromisoformat(res["date"])
        if ELECTION_WINDOW[0] <= d <= ELECTION_WINDOW[1]:
            out.append((res["date"], res["parties"]))
    out.sort(key=lambda x: x[0])
    return out


def build_country_rows(country: str, slug: str, code: str) -> list[dict]:
    page_html = fetch_country_page(slug)
    html_items = parse_party_details(page_html)
    data = fetch_poll_json(code)
    labels = data.get("parties", {})
    trend = data.get("trends", {}).get("kalmanSmooth", [])
    ranked_codes = rank_codes_by_latest_value(data)
    # include any code that appears in an in-window election but not in the current ranking
    for _, parties in elections_in_window(data):
        for c in parties:
            if c not in ranked_codes:
                ranked_codes.append(c)
    name_map = match_party_names(html_items, ranked_codes, labels)
    elections = elections_in_window(data)

    rows = []
    for code_p in ranked_codes:
        row = {
            "country": country,
            "politico_code": code_p,
            "party_name_english": name_map[code_p]["english"],
            "party_name_native": name_map[code_p]["native"],
        }
        for col, target in TARGET_MONTHS.items():
            row[col] = nearest_trend_value(trend, code_p, target)
        for i, (edate, eparties) in enumerate(elections, start=1):
            row[f"election_date_{i}"] = edate
            row[f"election_result_{i}"] = eparties.get(code_p)
        rows.append(row)

    # drop parties with no polling info at all in our window and no election result
    poll_cols = list(TARGET_MONTHS.keys())
    election_cols = [c for c in rows[0].keys() if c.startswith("election_result_")] if rows else []
    rows = [
        r for r in rows
        if any(r.get(c) is not None for c in poll_cols)
        or any(r.get(c) is not None for c in election_cols)
    ]
    return rows


def scrape_all() -> pd.DataFrame:
    all_rows = []
    for country, (slug, code) in COUNTRIES.items():
        print(f"Scraping {country} ({code}) ...")
        try:
            rows = build_country_rows(country, slug, code)
        except Exception as exc:  # noqa: BLE001
            print(f"  FAILED: {exc}")
            continue
        print(f"  {len(rows)} parties")
        all_rows.extend(rows)
        time.sleep(1)  # be polite

    max_elections = max(
        (len([c for c in r if c.startswith("election_result_")]) for r in all_rows),
        default=0,
    )
    election_cols = []
    for i in range(1, max_elections + 1):
        election_cols += [f"election_date_{i}", f"election_result_{i}"]

    cols = ["country", "politico_code", "party_name_english", "party_name_native"] + \
        list(TARGET_MONTHS.keys()) + election_cols
    df = pd.DataFrame(all_rows)
    for c in cols:
        if c not in df.columns:
            df[c] = pd.NA
    return df[cols]


GENERIC_STOPWORDS = {"party", "of", "the", "and", "for"}


def normalize_name(s: str) -> str:
    s = str(s).lower().strip()
    s = re.sub(r"[^a-z0-9 ]", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def token_jaccard(a: str, b: str, extra_stop: set[str] = frozenset()) -> float:
    stop = GENERIC_STOPWORDS | extra_stop
    sa, sb = set(a.split()) - stop, set(b.split()) - stop
    if not sa or not sb:
        return 0.0
    return len(sa & sb) / len(sa | sb)


def assign_ches_id(df: pd.DataFrame) -> pd.DataFrame:
    """ches_id, matched against the CHES codebook's party-abbreviation tables (see
    parse_ches_codebook/match_codebook_party_id above). Matching candidates for each
    scraped party are its POLITICO polling code (annotation stripped, e.g. "RV (B)" ->
    "RV") and its native-language name; unmatched parties get pd.NA.

    POLITICO tags multi-party electoral coalitions with a "coal" suffix in their internal
    code (e.g. "ODScoal_2023" for the ODS+TOP09+KDU-CSL alliance, "OLaNOKUZLcoal" for
    "OL'aNO & Friends"). Abbreviation matching would otherwise happily prefix-match those
    onto their lead party (e.g. "ODScoal_2023" -> "ODS"), which would misattribute a
    single member party's eu_russia score to the whole coalition's polling entry -- a
    guess we're not willing to make, so the code candidate is dropped for those rows.
    The prefix tier is further restricted to the (short, code-shaped) polling code only
    -- never the full native-language name -- since a full coalition name can itself
    happen to start with one member's own short code (e.g. Slovakia's "OLaNO a
    priatelia" coalition name starting with "OLaNO")."""
    codebook_index = build_codebook_index(parse_ches_codebook())
    df["ches_id"] = pd.NA
    for idx, row in df.iterrows():
        country_code = COUNTRY_TO_CODEBOOK.get(row["country"])
        cbi = codebook_index.get(country_code)
        if cbi is None:
            continue
        code_candidates = [] if "coal" in str(row["politico_code"]).lower() else [normalize_code(row["politico_code"])]
        exact_candidates = code_candidates + [row["party_name_native"]]
        party_id = match_codebook_party_id(
            cbi, exact_candidates, code_candidates, row["party_name_native"], row["party_name_english"],
        )
        if party_id is not None:
            df.at[idx, "ches_id"] = party_id
    return df


def assign_new_q59(df: pd.DataFrame) -> pd.DataFrame:
    """new_q59, matched against the [New_Q59] party-choice table in
    data_raw/Copy of script 29.04.2025.docx (see parse_q59_table/match_q59_code above)."""
    q59_index = build_q59_index(parse_q59_table())
    df["new_q59"] = pd.NA
    for idx, row in df.iterrows():
        q59_country_index = q59_index.get(row["country"])
        if not q59_country_index:
            continue
        code = match_q59_code(q59_country_index, row["party_name_native"], row["party_name_english"])
        if code is not None:
            df.at[idx, "new_q59"] = code
    return df


def _mean_by_ches_id(ches_id, values_by_id: dict):
    """ches_id in the crosswalk is occasionally semicolon-separated (merged parties);
    average the values of whichever of those ids are present in `values_by_id`."""
    if pd.isna(ches_id):
        return None
    ids = []
    for part in str(ches_id).split(";"):
        part = part.strip()
        try:
            ids.append(float(part))
        except ValueError:
            continue
    vals = [values_by_id[i] for i in ids if i in values_by_id and pd.notna(values_by_id[i])]
    return sum(vals) / len(vals) if vals else None


def join_eu_russia(df: pd.DataFrame) -> pd.DataFrame:
    """eu_russia, joined on ches_id == party_id. Primary source: the 2024 wave of
    1999-2024_CHES_dataset_meansV2.csv (the only year the variable was fielded). If a
    party has a ches_id but no eu_russia value there, fall back to the EU_Russia column
    of CHES_Ukraine_March_2024.dta. Never guessed by name/abbreviation matching:
    anything still unmatched is left as "NA"."""
    means = pd.read_csv(DATA_RAW / "1999-2024_CHES_dataset_meansV2.csv")
    means_2024 = means[means["year"] == 2024]
    eu_russia_by_id = means_2024.set_index("party_id")["eu_russia"].to_dict()

    ches_ukraine = pd.read_stata(DATA_RAW / "CHES_Ukraine_March_2024.dta", convert_categoricals=False)
    eu_russia_fallback_by_id = ches_ukraine.set_index("party_id")["EU_Russia"].to_dict()

    values = []
    for _, row in df.iterrows():
        val = _mean_by_ches_id(row["ches_id"], eu_russia_by_id)
        if val is None:
            val = _mean_by_ches_id(row["ches_id"], eu_russia_fallback_by_id)
        values.append(val if val is not None else "NA")
    df["eu_russia"] = values
    return df


def main():
    DATA_CLEAN.mkdir(exist_ok=True)
    df = scrape_all()
    df = assign_ches_id(df)
    df = assign_new_q59(df)
    df = join_eu_russia(df)
    out_path = DATA_CLEAN / "politico_polls.csv"
    df.to_csv(out_path, index=False)
    print(f"\nWrote {len(df)} rows to {out_path}")


if __name__ == "__main__":
    main()
