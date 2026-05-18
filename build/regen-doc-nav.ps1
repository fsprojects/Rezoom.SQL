#requires -Version 5
<#
.SYNOPSIS
    Regenerate breadcrumb + prev/next navigation blocks in every doc page.

.DESCRIPTION
    Parses SUMMARY.md at the repo root, walks its tree depth-first to build a
    linear reading order, and rewrites a marker-fenced nav block at the top
    and bottom of every .md file referenced.

    The top block is breadcrumbs (Home > Section > Page) followed by a
    prev / next bar. The bottom block is a horizontal rule and a prev / next
    bar. Both are wrapped in HTML comment markers (`<!-- nav-top -->` /
    `<!-- nav-bottom -->`) so the script can rerun and rewrite cleanly when
    SUMMARY.md changes.

    Also strips the legacy "(this page is part of...)" preamble lines from
    Tutorial pages, since the new breadcrumb supersedes them.

.NOTES
    Re-run after editing SUMMARY.md (adding pages, reordering, renaming).
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

$touched = 0
foreach ($e in $entries) {
    if (-not $e.AbsPath) {
        Write-Host "Skipping missing file: $($e.RelPath)" -ForegroundColor Yellow
        continue
    }
    $path = $e.AbsPath.Path
    $body = [System.IO.File]::ReadAllText($path)

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
    $newBody = $topBlock + $body + $bottomBlock + "`n"

    [System.IO.File]::WriteAllText($path, $newBody, [System.Text.UTF8Encoding]::new($false))
    $touched++
}

Write-Host "Rewrote nav blocks in $touched files." -ForegroundColor Green
