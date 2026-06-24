#requires -Version 5
<#
.SYNOPSIS
    Regenerate breadcrumb + prev/next navigation blocks in every doc page,
    and emit Just-the-Docs YAML frontmatter for the gh-pages-built sidebar.

.DESCRIPTION
    Parses SUMMARY.md at the repo root, walks its tree depth-first to build a
    linear reading order, and rewrites three sections of every .md file
    referenced:

    1. YAML frontmatter at the very top of the file driving the
       Just-the-Docs sidebar nav (`title`, `parent`, `grand_parent`,
       `nav_order`, `has_children`).
    2. A marker-fenced nav block (`<!-- nav-top -->`) below the frontmatter
       with breadcrumbs (Home > Section > Page) and a prev/next bar.
    3. A marker-fenced bottom nav block (`<!-- nav-bottom -->`) with a
       horizontal rule and another prev/next bar.

    All three are idempotent: rerunning the script after SUMMARY.md changes
    cleanly strips and rewrites without accumulating cruft.

    Also strips the legacy "(this page is part of...)" preamble lines from
    Tutorial pages, since the new breadcrumb supersedes them.

.NOTES
    Re-run after editing SUMMARY.md (adding pages, reordering, renaming).
    The frontmatter is consumed by Jekyll + just-the-docs; the marker-fenced
    nav blocks serve readers viewing the markdown raw on github.com.
#>
[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'
$repoRoot = Resolve-Path (Join-Path $PSScriptRoot '..')
$summaryPath = Join-Path $repoRoot 'SUMMARY.md'
if (-not (Test-Path $summaryPath)) {
    throw "Couldn't find SUMMARY.md at $summaryPath"
}

# ---- Parse SUMMARY.md ----

$entries = New-Object System.Collections.Generic.List[object]
$summary = Get-Content $summaryPath
$lineRegex = '^(?<indent>\s*)\*\s+\[(?<title>[^\]]+)\]\((?<path>[^)]+)\)\s*$'
foreach ($line in $summary) {
    if ($line -match $lineRegex) {
        $indent = $Matches.indent.Length
        $depth = [int]($indent / 4)  # SUMMARY.md uses 4-space indents
        $rel = $Matches.path -replace '\\', '/'
        $entries.Add([pscustomobject]@{
            Depth = $depth
            Title = $Matches.title
            RelPath = $rel
            AbsPath = Resolve-Path (Join-Path $repoRoot $rel) -ErrorAction SilentlyContinue
        })
    }
}

if ($entries.Count -lt 2) {
    throw "Parsed fewer than 2 entries from SUMMARY.md; aborting before damage."
}

# ---- Compute parents (for breadcrumbs) and prev/next ----

# Walk the linear list, tracking the parent stack: index of the most recent
# entry at each depth less than current. Parent = stack[depth - 1].
$parentStack = @{}
for ($i = 0; $i -lt $entries.Count; $i++) {
    $e = $entries[$i]
    $parents = New-Object System.Collections.Generic.List[object]
    for ($d = 0; $d -lt $e.Depth; $d++) {
        if ($parentStack.ContainsKey($d)) {
            $parents.Add($parentStack[$d])
        }
    }
    $e | Add-Member -NotePropertyName Parents -NotePropertyValue $parents
    $parentStack[$e.Depth] = $e
    # Clear deeper entries so they don't bleed across siblings.
    foreach ($k in @($parentStack.Keys | Where-Object { $_ -gt $e.Depth })) {
        $parentStack.Remove($k) | Out-Null
    }
}
for ($i = 0; $i -lt $entries.Count; $i++) {
    $prev = if ($i -gt 0) { $entries[$i - 1] } else { $null }
    $next = if ($i -lt $entries.Count - 1) { $entries[$i + 1] } else { $null }
    $entries[$i] | Add-Member -NotePropertyName Prev -NotePropertyValue $prev
    $entries[$i] | Add-Member -NotePropertyName Next -NotePropertyValue $next
}

# ---- Compute SiblingOrder + HasChildren (for Just-the-Docs frontmatter) -

# SiblingOrder: 1-based position among entries sharing the same immediate
# parent. Top-level entries (depth 0) are siblings of each other under a
# synthetic '<root>' key.
$siblingCounter = @{}
foreach ($e in $entries) {
    $parentKey =
        if ($e.Parents.Count -gt 0) {
            $e.Parents[$e.Parents.Count - 1].RelPath
        } else { '<root>' }
    if (-not $siblingCounter.ContainsKey($parentKey)) {
        $siblingCounter[$parentKey] = 0
    }
    $siblingCounter[$parentKey]++
    $e | Add-Member -NotePropertyName SiblingOrder -NotePropertyValue $siblingCounter[$parentKey]
}

# HasChildren: true if any other entry's immediate parent is this entry.
# Just-the-Docs uses this to expand the page as a parent node in the sidebar.
foreach ($e in $entries) {
    $hasChildren = $false
    foreach ($other in $entries) {
        if ($other.Parents.Count -gt 0 -and
            $other.Parents[$other.Parents.Count - 1].RelPath -eq $e.RelPath) {
            $hasChildren = $true
            break
        }
    }
    $e | Add-Member -NotePropertyName HasChildren -NotePropertyValue $hasChildren
}

# ---- Helpers ----

function Get-Rel($fromFile, $toFile) {
    # Manual relative-path because [System.IO.Path]::GetRelativePath isn't in
    # Windows PowerShell 5.1's .NET Framework 4.x BCL.
    $fromDir = Split-Path -Parent $fromFile
    $fromParts = $fromDir.Replace('\', '/').TrimEnd('/').Split('/')
    $toParts = $toFile.Replace('\', '/').Split('/')

    $common = 0
    while ($common -lt $fromParts.Length -and $common -lt $toParts.Length -and
           $fromParts[$common] -ieq $toParts[$common]) {
        $common++
    }

    $upCount = $fromParts.Length - $common
    $ups = if ($upCount -gt 0) { ,'..' * $upCount } else { @() }
    $remainder =
        if ($common -lt $toParts.Length) { $toParts[$common..($toParts.Length - 1)] }
        else { @() }
    return (@($ups) + @($remainder)) -join '/'
}

function Build-Breadcrumb($entry) {
    # Home link + each ancestor link + current page (not a link).
    $homePath = (Resolve-Path (Join-Path $repoRoot 'README.md')).Path
    $homeRel = Get-Rel $entry.AbsPath.Path $homePath
    $parts = @("[Home]($homeRel)")
    foreach ($p in $entry.Parents) {
        $rel = Get-Rel $entry.AbsPath.Path $p.AbsPath.Path
        $parts += "[$($p.Title)]($rel)"
    }
    $parts += $entry.Title
    return ($parts -join ' &gt; ')
}

function Build-Frontmatter($entry, $isRoot) {
    # Just-the-Docs YAML frontmatter. Drives the sidebar nav structure.
    # On rerun, the strip regex below removes any existing frontmatter
    # block at the top of the file and this writes a fresh one.
    if ($isRoot) {
        # The root README is the site landing page. Just-the-Docs treats
        # it as the home; nav_order: 0 keeps it first if it ever ends up
        # rendered in the sidebar.
        return "---`ntitle: $($entry.Title)`nnav_order: 0`n---"
    }
    $lines = @("title: $($entry.Title)")
    if ($entry.Parents.Count -ge 1) {
        $immediate = $entry.Parents[$entry.Parents.Count - 1]
        $lines += "parent: $($immediate.Title)"
    }
    if ($entry.Parents.Count -ge 2) {
        # Just-the-Docs requires grand_parent for any page nested three
        # levels deep so the sidebar knows where to slot it.
        $grand = $entry.Parents[0]
        $lines += "grand_parent: $($grand.Title)"
    }
    $lines += "nav_order: $($entry.SiblingOrder)"
    if ($entry.HasChildren) {
        $lines += "has_children: true"
    }
    return "---`n" + ($lines -join "`n") + "`n---"
}

function Build-PrevNext($entry) {
    $prevPart =
        if ($entry.Prev) {
            $rel = Get-Rel $entry.AbsPath.Path $entry.Prev.AbsPath.Path
            "[&larr; $($entry.Prev.Title)]($rel)"
        } else { '' }
    $nextPart =
        if ($entry.Next) {
            $rel = Get-Rel $entry.AbsPath.Path $entry.Next.AbsPath.Path
            "[$($entry.Next.Title) &rarr;]($rel)"
        } else { '' }
    if ($prevPart -and $nextPart) {
        return "$prevPart | $nextPart"
    } elseif ($prevPart) {
        return $prevPart
    } else {
        return $nextPart
    }
}

# ---- Rewrite each page ----

$rootEntry = $entries[0]
$topLevelSections = $entries | Where-Object { $_.Depth -eq 0 -and $_ -ne $rootEntry }

$navTopMarker = '<!-- nav-top -->'
$navTopEnd = '<!-- /nav-top -->'
$navBottomMarker = '<!-- nav-bottom -->'
$navBottomEnd = '<!-- /nav-bottom -->'

# Regex with the `s` flag (single-line mode) so . matches newlines. The
# bottom regex also consumes a preceding `---` horizontal rule (with optional
# blank lines around it) so the separator we emit alongside the bottom nav
# doesn't accumulate as an orphan on each rerun.
$navTopRegex = "(?s)$([regex]::Escape($navTopMarker)).*?$([regex]::Escape($navTopEnd))\r?\n?"
$navBottomRegex = "(?s)(?:\r?\n\s*---[ \t]*)*\r?\n?$([regex]::Escape($navBottomMarker)).*?$([regex]::Escape($navBottomEnd))\r?\n?"
$legacyPreamble = "^\(this page is part of \[[^\]]+\]\([^)]+\)\)\s*\r?\n"
# Trailing orphan `---` lines left over from earlier runs of this script before
# the bottom-regex started consuming them. Allows blank lines between adjacent
# orphans (which is exactly the pattern previous buggy runs produced).
$trailingOrphanSep = "(?:\r?\n\s*---[ \t]*)+\s*$"
# Existing YAML frontmatter at the very top of the file. The keyed-content
# requirement (`(?:[a-zA-Z_]\w*: [^\r\n]*\r?\n)+` between the delimiters)
# keeps the regex from misfiring on a file that genuinely opens with a bare
# `---` horizontal rule and no frontmatter, AND uses [^\r\n]* (not .*) so the
# value portion can't span newlines — important because on the second run
# the file does have frontmatter, and a greedy .* would otherwise extend
# down to the next `---` separator and consume the entire body. Trailing
# blank lines are also consumed so successive runs don't drift the body.
$frontmatterRegex = "\A---\r?\n(?:[a-zA-Z_]\w*: [^\r\n]*\r?\n)+---\r?\n\r?\n?"

$touched = 0
foreach ($e in $entries) {
    if (-not $e.AbsPath) {
        Write-Host "Skipping missing file: $($e.RelPath)" -ForegroundColor Yellow
        continue
    }
    $path = $e.AbsPath.Path
    $body = [System.IO.File]::ReadAllText($path)

    # Strip existing frontmatter at the very top of the file FIRST so the
    # nav-top strip below sees the actual `<!-- nav-top -->` marker at the
    # head of the body.
    $body = [regex]::Replace($body, $frontmatterRegex, '')
    # Strip existing nav blocks (anywhere in the file, idempotent).
    $body = [regex]::Replace($body, $navTopRegex, '')
    $body = [regex]::Replace($body, $navBottomRegex, '')
    # Strip legacy "(this page is part of ...)" preamble.
    $body = [regex]::Replace($body, $legacyPreamble, '')
    # Strip any trailing orphan `---` lines left behind by previous script runs.
    $body = [regex]::Replace($body, $trailingOrphanSep, '')

    if ($e -eq $rootEntry) {
        # Root README: no breadcrumb (it's Home), no prev/next bar. Instead,
        # link out to each top-level section so readers can jump directly into
        # the docs from the landing page.
        $sectionLinks = foreach ($s in $topLevelSections) {
            $rel = Get-Rel $e.AbsPath.Path $s.AbsPath.Path
            "[$($s.Title)]($rel)"
        }
        $topBlock = "$navTopMarker`n**Documentation:** $($sectionLinks -join ' | ')`n$navTopEnd`n`n"
        $bottomBlock = ''
    } else {
        $crumb = Build-Breadcrumb $e
        $bar = Build-PrevNext $e
        $topBlock =
            if ($bar) { "$navTopMarker`n$crumb`n`n$bar`n$navTopEnd`n`n" }
            else      { "$navTopMarker`n$crumb`n$navTopEnd`n`n" }
        $bottomBlock =
            if ($bar) { "`n`n---`n$navBottomMarker`n$bar`n$navBottomEnd`n" }
            else      { '' }
    }

    # Trim leading blank lines that may have been left by the strip, and
    # trailing whitespace, so the output is tidy.
    $body = $body.TrimStart("`r","`n"," ","`t").TrimEnd()
    $frontmatter = Build-Frontmatter $e ($e -eq $rootEntry)
    $newBody = "$frontmatter`n`n$topBlock$body$bottomBlock`n"

    [System.IO.File]::WriteAllText($path, $newBody, [System.Text.UTF8Encoding]::new($false))
    $touched++
}

Write-Host "Rewrote nav blocks in $touched files." -ForegroundColor Green
