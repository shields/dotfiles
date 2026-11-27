<!--
Copyright © 2020, 2022, 2026 Michael Shields

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

# macOS provisioning

1. `git clone https://github.com/shields/dotfiles.git ~/src/github.com/shields/dotfiles`
   (macOS offers to install git with the command line tools the first time you
   run it)
1. `cd ~/src/github.com/shields/dotfiles`
1. `./provision.sh`
1. Reboot

Additional steps not yet automated:

- Open Karabiner-Elements and grant permissions
- Open Chrome and sign in
- Open System Preferences:
  - Users & Groups > Login Items: add Karabiner-Elements

Many preferences can be translated to `defaults write` settings using
`tools/diff_defaults.py`.

# Dotfile preview and apply

Chezmoi reads its source files from `home/`, selected by `.chezmoiroot`.

| Source                          | Installed file            |
| ------------------------------- | ------------------------- |
| `home/dot_gitconfig.tmpl`       | `~/.gitconfig`            |
| `home/dot_config/git/ignore`    | `~/.config/git/ignore`    |
| `home/dot_config/bat/config`    | `~/.config/bat/config`    |
| `home/dot_config/starship.toml` | `~/.config/starship.toml` |
| `home/dot_config/yt-dlp/config` | `~/.config/yt-dlp/config` |
| `home/dot_inputrc`              | `~/.inputrc`              |
| `home/dot_ripgreprc`            | `~/.ripgreprc`            |
| `home/dot_tmux.conf`            | `~/.tmux.conf`            |
| `home/dot_vimrc`                | `~/.vimrc`                |

Edit the source files and, from this checkout, run:

```sh
make diff
make apply
```

Both targets use this checkout explicitly, so they work from git worktrees too.
Chezmoi reads the working files, including uncommitted and untracked files in
`home/`. Keep only deployable configuration there. Use `./provision.sh` to
install the other dotfiles.

Chezmoi is in the base Brewfile module; on an existing machine, install it
with `brew install chezmoi` before the first preview. `provision.sh` applies
these files after installing packages. Both it and `make apply` fail if an
existing file conflicts, including on the first adoption. After reviewing
`make diff`, resolve the differences in the source files or use:

```sh
chezmoi --source "$PWD" --less-interactive apply
```

That command asks before replacing conflicting files. To keep edits made
directly to an installed, untemplated file, use
`chezmoi --source "$PWD" re-add ~/.config/bat/config` (substituting its path)
and review the git diff. For `.gitconfig`, edit its template or use
`chezmoi --source "$PWD" merge ~/.gitconfig`.

The Git template sets the personal email and GitHub username for `shields`
on Linux and on Macs with neither DEP nor MDM enrollment. Other accounts and
enrolled Macs receive the shared settings without those identity fields. A
failed enrollment query stops the update. `~/.config/git/config`, including
the guest's credential helper, stays outside chezmoi's managed files.

# Whitespace display

Emacs and Vim wrap long lines without inserting newlines. Display markers use
dark blue (`#365773`) on pale blue (`#e8f4ff`) to distinguish them from file
contents, except that a no-break space that is not trailing is gray (`#949494`)
with no background of its own. That is the lightest gray with 3:1 contrast
against white, the background in Vim and graphical Emacs. Terminal Emacs uses
the terminal's own background and foreground, which are white and black in
Ghostty and iTerm2, and assumes that background is white, so the gray keeps its
3:1 contrast there too. Ghostty's `minimum-contrast = 2.95` then keeps the gray
in Vim and terminal Emacs instead of replacing it with black. CommitMonoShields
Nerd Font contains all four glyphs, two of which its build adds (see
`commit-mono/README.md`); iTerm2 uses the same font as graphical Emacs.

| Meaning        | Marker       | Emacs                                        | Vim                                 |
| -------------- | ------------ | -------------------------------------------- | ----------------------------------- |
| Soft wrap      | `⸗` (U+2E17) | End of the screen line                       | Start of the continuation           |
| Tab            | `⇥` (U+21E5) | Preserves tab stops, except a one-column tab | Preserves tab stops                 |
| Trailing space | `␣` (U+2423) | One per space, except at point               | One per space, except at the cursor |
| No-break space | `⍽` (U+237D) | Gray; marker colors if trailing              | Same                                |

In Emacs the tab marker is `⇥` followed by the tab itself, so a tab that is only
one column wide (it starts in the last column before a tab stop) pushes the text
after it a full tab width to the right. With tab width 8, `abcdefg<Tab>X` puts
`X` 16 columns from the left edge instead of 8. Tabs two or more columns wide,
and the blank tabs described below, keep their stops.

Ordinary spaces remain blank. Tabs remain blank in Go source, `go.mod`,
`go.work`, and Makefiles, including trailing tabs. Trailing spaces remain
visible in those files. TSV tabs remain visible as `⇥`. Emacs recognizes these
formats by major mode: `go-ts-mode`, `go-mod-ts-mode`, `go-work-ts-mode`, and
`makefile-mode` with the modes derived from it. Vim recognizes them by
filetype: `go`, `gomod`, `gowork`, `make`, and `automake`. The editors do not
agree on every file name: `Makefile.common` has blank tabs in Vim, where it is a
Makefile, but `⇥` in Emacs, where it is not. For another format that requires
tabs, add its mode to `shields/tab-required-modes` in Emacs or its filetype to
`g:shields_tab_required_filetypes` in Vim. Emacs buffer-local
`shields/show-tabs` and Vim buffer-local `b:shields_show_tabs` can also disable
tab markers. Emacs applies `shields/show-tabs` when a major mode starts and
after file-local and directory-local variables load, so set it there, for
example with `-*- shields/show-tabs: nil -*-` on a file's first line. After
changing it by hand, evaluate `(shields/update-whitespace-style)` with `M-:`; a
hand-set value lasts only until the next major mode change, including
`M-x normal-mode` and `M-x revert-buffer`. In Emacs, a `whitespace-style` set by
`.dir-locals.el` or a file's local variables replaces this configuration's style
for that buffer, tab handling included, and a `space-mark` in it draws U+00A0 as
its own mark (`¤`) instead of `⍽`. The `␣` markers and the `⍽` markers on
trailing no-break spaces are not part of the style: every style keeps them
except `nil`, which turns them off too, leaving every no-break space a gray
`⍽`.

Both editors draw U+00A0 and U+202F as `⍽`: in the marker colors in trailing
whitespace, a run of spaces, tabs, and no-break spaces that ends the line, and
in gray elsewhere. In Emacs, trailing whitespace that ends at point stays
unmarked, so a `␣` does not flash while you type a word and a space; the markers
return when point moves away, but its no-break spaces stay gray. Vim does the
same for trailing whitespace that ends at the cursor when the cursor is past the
end of its line: in Insert and Replace mode, after `$` in Visual mode, in a
prompt buffer, and with `'virtualedit'` past the end. In Normal mode the cursor
rests on the last character, so after `<Esc>` the run is marked, as it is in
Emacs with point before its last blank. Each window follows its own cursor, and
GUI Vim marks trailing whitespace always. Vim's `trail:` draws `␣` only for
spaces followed by nothing but spaces and tabs, so a space before a trailing
no-break space is blank in Vim, where Emacs draws `␣`; Vim gives that cell the
marker colors, so only the glyph differs. Emacs also keeps `whitespace-mode`'s
highlights for a space before a tab (orange), `tab-width` or more spaces after a
tab (yellow), and a missing final newline (olive, on the last character, except
while point is at the end of the buffer), including in buffers where tabs are
blank. They cover the spaces when `indent-tabs-mode` is on, as in Makefiles, and
the tab when it is off. Vim has none of them.

In Emacs the `␣` and trailing `⍽` markers are overlays, not a `display` text
property set by font-lock. Indirect buffers share text properties but each has
its own point and `whitespace-mode` state, and copied text would carry the
property into other buffers. A hook mirrors edits among the buffers that share
text so that each refreshes its own overlays.

Vim paints every cell that `'listchars'` draws for a tab, a trailing space, or a
no-break space with the `SpecialKey` group, in every window. To let tabs be
blank, `.vimrc` gives `SpecialKey` no attributes (set explicitly, because Vim
restores the defaults of a cleared group when `'background'` changes) and colors
the markers with window-local `matchadd()` matches below `hlsearch`. At priority
-1 they cover `⇥`, `␣`, the spaces and no-break spaces in trailing whitespace,
and the characters Vim shows as `^X` or `<xx>`; at priority -2 a gray match
covers every no-break space, so trailing ones keep the marker colors. `⸗`
and `…` come from `NonText`. Vim applies the matches when a buffer is displayed,
when its filetype changes, when a window is created, and when `'list'` is set,
so after changing `b:shields_show_tabs` by hand, run `:doautocmd BufWinEnter`.
The tradeoffs: `:map` listings no longer color special keys, `clearmatches()`
removes the marker and no-break space colors from a window until one of those
events (moving the cursor onto trailing whitespace restores only the trailing
markers), `'cursorcolumn'` and `'colorcolumn'` do not tint marker cells, a line
of a million characters redraws slowly (about 0.35 s), and a long stretch of
alternating spaces and tabs followed by more text takes time that grows with the
square of its length: 0.3 s at 8,000 characters. From about 22,000 characters a
redraw reaches Vim's `'redrawtime'` of 2 s, and Vim stops coloring that line and
the lines below it.

On the line where the cursor ends trailing whitespace, the marker match skips
the line, and a match at priority -3 gives the spaces there `ShieldsHidden`,
whose only attribute is standout. No highlight attribute hides a glyph, and
terminals that enforce a minimum contrast, as Ghostty does here, show a
foreground that matches the background, so in a terminal with colors `.vimrc`
sets `t_so` and `t_se` to SGR 8 and 28, the terminal's invisible attribute, in
place of standout. Strikethrough would hide the text that the syntax files for
Markdown and HTML strike. These matches name the line, and Vim replaces them
when the cursor moves, when Insert mode is entered or left, when a window is
entered, and when an edit moves the cursor of another window on the buffer. The
tradeoffs: any highlight with standout is invisible in a terminal with colors,
though Vim's default colors do not use it there and only the `lss` syntax, the
`quiet` and `sorbet` color schemes, and `blue` with true color do; a terminal
without SGR 8 shows the `␣` there in the default colors (tmux passes SGR 8 on
when the outer terminal's terminfo has `invis`), and one without colors shows it
as it shows the other markers; a search match there shows its `␣`, because the
hiding match is below `hlsearch` too; an edit to a buffer that is not current,
such as `setbufline()` from a timer, leaves the matches in the windows on it
where they were until their cursors move; and an edit from a timer or job
callback while Vim waits in Insert mode, as in a prompt buffer that shows
output, takes effect at the next key.

Vim uses true color in iTerm2 and terminals advertising it through `COLORTERM`;
its 256-color fallback uses colors 24 and 195, and 246 for the gray. Emacs
uses the exact colors in true-color terminals and graphical frames, and the same
palette colors in terminals whose xterm palette it knows: the xterm family and
those it treats alike, such as rxvt, st, screen, and tmux. Other terminals get
the nearest of the colors Emacs registers for them, which for putty-256color,
gnome-256color, vte-256color, mlterm-256color, and nsterm-256color are only 8.
With 16 colors the markers are bright blue on bright white and the gray `⍽` is
bright black, the nearest color to the gray. With 8, or without bright black,
the markers are blue on white and the `⍽` is cyan, because the nearest color to
the gray is the white background. With wrapping explicitly disabled, both
editors use `…` to mark truncated text.

`bin/pager` keeps less's normal line wrapping. Less 710 has no setting for a
soft-wrap marker, custom tab markers, trailing-space markers, or filetype
exceptions. Its `--rscroll` option marks **truncation**, not wrapping, so the
pager uses `…` with colors 24 and 195 when `-S` is requested. It sets
`LESSCHARSET=utf-8` because less otherwise reads only the first byte of `…` in
the C locale or in a UTF-8 locale that is not installed. Input is passed
through unchanged, preserving searching, copying, and ANSI formatting. The
optional `--PROC-TAB` flag can reveal tabs as `^I` for inspection. See
`man less`, the [less release notes][less-rscroll], and
[Vim's display options][vim-display].

These settings change display only. In Emacs, `whitespace-style` does not
include `trailing`, because the `␣` and trailing `⍽` markers replace its
highlight, but `whitespace-cleanup`, `whitespace-cleanup-region`,
`whitespace-report` and `whitespace-report-region` see `trailing` in this
configuration's style while they run. They trim and count trailing spaces, tabs
and U+00A0 as they do with whitespace.el's own style (which does not treat
U+202F as trailing). The style's `space-before-tab`, `space-after-tab` and
`missing-newline-at-eof` apply too, so the cleanup commands add a missing final
newline and rewrite tabs next to spaces. With `indent-tabs-mode` off, as it is
by default here, a tab that follows spaces or precedes `tab-width` or more
spaces becomes spaces. With it on, the spaces before a tab are deleted and
`tab-width` spaces after one become a tab. Do not run the cleanup commands on
tab-separated files. The `cleanup`, `auto-cleanup`, `report-on-bogus` and
`abort-on-bogus` values of `whitespace-action`, which call these commands,
behave the same way.
Only the current buffer sees the added `trailing`, and only while the command
runs, even if it fails or is interrupted. A `whitespace-style` set by
`.dir-locals.el` or a file's local variables is used exactly as given: without
`trailing`, these commands neither trim nor count trailing blanks, and with
`nil` they change and report nothing. On save, ws-butler removes trailing
whitespace from the lines that were edited; `whitespace-cleanup` trims the whole
buffer.

Run `./provision.sh` to install the configuration and the font (it copies the
font into `~/Library/Fonts/` rather than linking it, so run it again after the
font is rebuilt), then restart Emacs and Vim; new pager processes use the
configuration immediately. Restart iTerm2 if it still has the old font cached.

[less-rscroll]: https://www.greenwoodsoftware.com/less/news.661.html
[vim-display]: https://vimhelp.org/options.txt.html#%27listchars%27

# Brewfile modules

What `provision.sh` installs with Homebrew is split into modules,
`brew/<module>.Brewfile`. The `Brewfile` in the root only loads `base`, the
operating system's own module (`macos` or `linux`) and the optional modules
that are selected.

| Module  | Contents                                                                                                       |
| ------- | -------------------------------------------------------------------------------------------------------------- |
| `base`  | Always installed: git, gh, shell tools, Python, Node, Go, uv, and the Claude Code and Codex casks.             |
| `dev`   | Linters, formatters, language servers, compilers and other development tools, and the cargo, go and npm tools. |
| `cloud` | Cloud and Kubernetes command-line tools, including the Google Cloud CLI.                                       |
| `data`  | Databases, notebooks, mail and sync tools.                                                                     |
| `media` | Image, audio and video tools.                                                                                  |
| `macos` | Always installed on macOS: casks, Mac App Store apps, GNU tools, Emacs Plus and other macOS-only packages.     |
| `linux` | Always installed on Linux: Emacs.                                                                              |

```
./provision.sh [--remove-modules] [MODULE... | none]
```

- The selection is saved in `~/.config/dotfiles/brew-modules`, as a
  space-separated list that may be empty. Without arguments, `./provision.sh`
  uses the saved selection. Where none is saved, macOS installs every optional
  module and Linux installs `dev`.
- Module names on the command line replace the saved selection. `none` selects
  no optional modules.
- Naming fewer modules than are installed would uninstall their packages, so
  `provision.sh` stops and prints what `brew bundle cleanup` would uninstall.
  Pass `--remove-modules` to go ahead.
- A module added to `brew/` is not installed on a machine that has a saved
  selection until you name it once, for example
  `./provision.sh dev cloud newmodule`.
- To change what a module installs, edit `brew/<module>.Brewfile`, or run
  `brew bundle add --file=brew/<module>.Brewfile ...` (or `remove`).
- To check a machine against its modules, run `brew bundle check --verbose` and
  `brew bundle cleanup` (without `--force`) in an interactive shell. The cargo
  entries are found only where rustup's `bin` is on the `PATH`, which `.zshrc`
  arranges.
- Do not run `brew bundle dump`: it would replace the loader with a flat list.

Provisioning upgrades formulae through Bundle, which also manages services
declared with `restart_service: :changed` in a Brewfile. Cask upgrades run
separately and exclude Chrome. The macOS Brewfile installs
`shields/tap/mute-on-lock` with `restart_service: :changed` to start its login
service and restart it on upgrades.

# Persistent Linux box

A Linux machine that you reach over SSH gets the same shell, git, Emacs (in the
terminal) and agent setup as the Mac. It needs Debian 13 on x86_64 or arm64 and
a regular user with sudo; Homebrew refuses to run as root.

1. `sudo apt update && sudo apt install git tmux`
1. `git clone https://github.com/shields/dotfiles.git ~/src/github.com/shields/dotfiles`
1. `cd ~/src/github.com/shields/dotfiles`
1. Run `tmux`, then `./provision.sh [MODULE...]` inside it, so that a dropped
   connection does not stop a run that takes tens of minutes. The default is the
   `dev` module; see Brewfile modules above.
1. If sudo asks for a password, expect two prompts: one at the start, for the
   packages that `provision/linux-system.sh` installs from apt, and a second one
   near the end, for the libraries that Playwright's browser needs.

`provision.sh` makes Debian's `/usr/bin/zsh` the login shell, generates the
`en_US.UTF-8` locale and installs Homebrew in `/home/linuxbrew/.linuxbrew`.
Running it again is safe, and takes a few minutes.

Nothing secret is in the repository, so log in once on each machine:

- GitHub: `gh auth login`, or a fine-grained token that is limited to the
  repositories you need: `gh auth login --with-token`. git uses `gh` as its
  credential helper.
- Claude Code: `claude auth login`, or run `claude setup-token` and put the
  token in `~/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN` (mode 0600). Interactive
  zsh shells on Linux export it from there.
- Codex: `codex login --device-auth`

`cg` starts Claude Code like `c`, but on GLM-5.3 through Fireworks'
Anthropic-compatible API, and `cgw` is `cg --worktree`. It needs a Fireworks API
key in `~/.config/secrets/FIREWORKS_API_KEY` (mode 0600), which it reads at
launch and passes only to that `claude`, in place of the Claude login. Outside a
throwaway VM the sandbox hides the key from the commands that `claude` runs, but
its hooks and MCP servers inherit it. A Lima VM made by `cog` holds no key: when
the Mac has one, the file there holds the placeholder `cog`, which `cog`'s
gateway on the Mac replaces with the key (see "Throwaway VMs"), and otherwise
the file is absent and `cg` fails. Every model alias maps to
`accounts/fireworks/models/glm-5p3`, and so does every subagent, whatever model
it names. The status line's cost is at Fireworks' rates, because provisioning
installs `provision/claude-model-pricing.json` as a managed-settings drop-in
(`managed-settings.d/50-model-pricing.json` in
`/Library/Application Support/ClaudeCode/` on a Mac, which takes `sudo` once,
and in `/etc/claude-code/` on Linux); Claude Code reads `modelPricing` from
managed settings only. On GLM, Claude Code's WebSearch, image input and fast
mode do not work.

Login shells set `LANG` to `en_US.UTF-8` when the SSH client did not send one,
but a non-interactive `ssh box command` runs without a `LANG` unless the client
sends it (`SendEnv LANG`). The system `ssh_config` of Debian and of macOS's own
`/usr/bin/ssh` does, but Homebrew's `ssh`, which `provision.sh` installs and
`.zshrc` puts first in `PATH` on a Mac, does not.

# Throwaway VMs

Throwaway VMs, on this Mac with Lima or in Cloudflare containers, where Claude
Code and Codex run with no sandbox and no permission prompts, are made by the
`cog` command from [shields/cog](https://github.com/shields/cog), which also
runs the gateway that keeps a VM's GitHub access to the repositories it is
granted and, for a Lima VM, holds the model credentials. Its README describes
installing and using it. A VM is a clone of a base that `cog base` provisions
from a dotfiles checkout, so what follows is what `cog` needs from this
repository and what this repository reads from a VM.

`cog` needs from a dotfiles checkout:

- a git work tree with an executable `./provision.sh [MODULE...]` that honours
  `DOTFILES_KEEP_BREW_DOWNLOADS` and leaves Homebrew at
  `/home/linuxbrew/.linuxbrew` with `uv`, `gh` and the `claude-code@latest` and
  `codex` casks;
- `.claude/settings.json` with a non-empty `permissions.deny` that includes the
  five `git push` rules the gateway setup removes (`Bash(git push*)`,
  `Bash(git -C * push*)`, `Bash(git -c * push*)`, `Bash(git --git-dir* push*)`
  and `Bash(git --work-tree* push*)`), a `PreToolUse` hook whose command
  contains `git_guard.py`, and optionally `statusLine`;
- `tools/configure_codex.py`, which `cog` runs again after it writes the
  throwaway marker;
- `~/.codex/rules/no-push.rules`, which `cog` removes in a gateway guest.

This repository reads two marker files that `cog` writes in the guest:
`/etc/cog-throwaway`, whose presence makes the machine a throwaway VM, and
`/etc/cog-gateway`, one line holding the VM's UUID, or `cloudflare`.
`.zshrc`, `.zshenv`, `.zsh.d/wt.zsh`, `.codex/hooks/git_guard.py`,
`tools/configure_codex.py` and `provision.sh` read them, and
`tests/test_cog_contract.py` checks that each names its marker and that
`.claude/settings.json` satisfies the list above. Where the second file holds
a UUID, `wt` names a new worktree's branch `cog/UUID/worktree-NAME`, since the
gateway accepts pushes only to `cog/UUID/*`. Where `/etc/cog-gateway`
exists, `provision.sh` leaves the credential helper for `https://github.com` in
`~/.config/git/config` alone, because `cog` puts the gateway's helper there
(replacing the `gh` helper that an earlier run wrote).

A Lima VM holds no Claude token and no Fireworks key. `cog` points
`api.anthropic.com` and `api.fireworks.ai` at its gateway in the VM's
`/etc/hosts` and writes the placeholder `cog` to
`~/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN`, unless `cog new` got
`--no-claude`, and, when the Mac has a Fireworks key, to
`~/.config/secrets/FIREWORKS_API_KEY`; the gateway on the Mac replaces the
placeholder with a real credential. That works because of three things here,
which `cog` relies on:

- on Linux, where `/etc/cog-gateway` is a regular file, `.zshenv`
  exports `SSL_CERT_FILE`, `NODE_EXTRA_CA_CERTS` and `REQUESTS_CA_BUNDLE` as
  the system bundle, which holds the gateway's CA, so Claude Code trusts the
  gateway for those names;
- on Linux, where `~/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN` exists, `.zshrc`
  exports `CLAUDE_CODE_OAUTH_TOKEN` from it, so Claude Code sends
  `Authorization: Bearer cog` and stays in direct mode;
- `cg` sends the content of `FIREWORKS_API_KEY` as the bearer token to
  `https://api.fireworks.ai/inference` and no other credential.

When Claude or Fireworks is wired, `cog new` checks in the VM's interactive zsh,
before it hands over the VM, that `NODE_EXTRA_CA_CERTS` is exported as
`/etc/ssl/certs/ca-certificates.crt` and, unless `--no-claude`, that
`CLAUDE_CODE_OAUTH_TOKEN` is exported as `cog`. A change to either needs the
same change in `cog`.

# Testing on Linux

`make test-linux` needs Docker. It copies the checkout into a temporary
directory outside the repository with `tools/stage_tree.sh`, which includes
files that are not yet committed and scans them with gitleaks. It then builds
`tools/linux-test.Dockerfile` for the machine's own architecture with that
directory as the context, which runs `./provision.sh` just as on a Linux box,
and finally runs `bun install --frozen-lockfile` and `make test lint` in the
image as an unprivileged user. It fails if any step fails.

The Codex tests start Codex's bubblewrap sandbox, which Docker's defaults
prevent, so the container runs with `/proc` unmasked (`systempaths=unconfined`)
and a seccomp profile that is Docker's default plus `clone`, `mount`,
`pivot_root`, `umount2` and `unshare` for every process.
`tools/moby-seccomp-default.json` is that default, unchanged from
[moby/profiles](https://github.com/moby/profiles) `seccomp/v0.2.3`, the version
Docker 29.8 builds in; `make test-linux` needs `jq` to add the rule. It needs
no AppArmor option because OrbStack's Docker runs without AppArmor.

`make test-linux MODULES="dev cloud"` provisions other modules (the default is
`dev`, whose tools `make test lint` needs, so keep it in the list), and
`LINUX_IMAGE` names the image (the default is `dotfiles-linux-test`).
