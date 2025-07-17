#!/bin/bash

set -euo pipefail

# Copy these files.
tar cf - bin Library $(git ls-files | grep '^\.') | (cd "$HOME" && tar xvf -)

# Install Homebrew and Xcode (which will take tens of minutes).
# Use path selection logic from https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh
# We will install full Xcode later from the Mac App Store.
UNAME_MACHINE="$(/usr/bin/uname -m)"
if [[ ${UNAME_MACHINE} == "arm64" ]]; then
    HOMEBREW_PREFIX="/opt/homebrew"
    HOMEBREW_REPOSITORY="${HOMEBREW_PREFIX}"
else
    HOMEBREW_PREFIX="/usr/local"
    HOMEBREW_REPOSITORY="${HOMEBREW_PREFIX}/Homebrew"
fi
if [[ ! -d $HOMEBREW_REPOSITORY ]]; then
    # CI=1 suppresses confirmation prompts.
    CI=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"

brew analytics off

softwareupdate --install --recommended

# Install Rosetta if it's not already working.
if ! arch -x86_64 /usr/bin/true 2>/dev/null; then
    softwareupdate --install-rosetta --agree-to-license
fi

# Oh My Zsh installation. This is the interesting part of
# https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh
if [[ ! -d "$HOME/.oh-my-zsh" ]]; then
    git clone --depth=1 https://github.com/ohmyzsh/ohmyzsh "$HOME/.oh-my-zsh"
fi
"$HOME/.oh-my-zsh/tools/upgrade.sh" -v minimal

# Install or update fzf-tab plugin
if [[ ! -d "$HOME/.oh-my-zsh/custom/plugins/fzf-tab" ]]; then
    git clone --depth=1 https://github.com/Aloxaf/fzf-tab "$HOME/.oh-my-zsh/custom/plugins/fzf-tab"
else
    (cd "$HOME/.oh-my-zsh/custom/plugins/fzf-tab" && git pull)
fi

# Install or update git-prompt-watcher plugin
if [[ ! -d "$HOME/.oh-my-zsh/custom/plugins/git-prompt-watcher" ]]; then
    git clone --depth=1 https://github.com/shields/git-prompt-watcher "$HOME/.oh-my-zsh/custom/plugins/git-prompt-watcher"
else
    (cd "$HOME/.oh-my-zsh/custom/plugins/git-prompt-watcher" && git pull)
fi

# Set email address in .gitconfig.
if [[ "$(whoami)" == shields ]] && ! (profiles status -type enrollment | grep -q ': Yes'); then
    git config --global user.email shields@msrl.com
    git config --global github.user shields # For Magit Forge
fi

brew update

# Homebrew bundle sync. Update using `brew bundle dump -f`.
brew bundle --force | (grep -v '^Using ' || true)
brew bundle cleanup --force
# Homebrew upgrades. Run formulas and casks separately to prevent whiny messages.
brew upgrade --formula
# Suppress upgrade of Chrome since it doesn't like to be upgraded while running.
brew outdated --greedy-auto-updates --cask --quiet | (grep -v '^google-chrome' || true) | xargs brew upgrade --cask
brew autoremove
brew cleanup --prune=all

# Be sure we're using full Xcode instead of the CLI-tools-only subset.
if ! xcrun --find xcodebuild 2>/dev/null; then
    sudo xcode-select --reset
fi
if ! xcodebuild -checkFirstLaunchStatus; then
    sudo xcodebuild -license accept
    sudo xcodebuild -runFirstLaunch
fi

xcodebuild -downloadPlatform iOS

# Set shell to current zsh installed from Homebrew.
if [[ "$(dscl . read /Users/$(whoami) UserShell)" == "UserShell: /bin/zsh" ]]; then
    sudo dscl . change "/Users/$(whoami)" UserShell /bin/zsh "$HOMEBREW_PREFIX/bin/zsh"
fi

# NPM-installed tools
npm install -g @anthropic-ai/claude-code
npm upgrade -g
claude config set -g theme light

# Plugins!
export PIP_DISABLE_PIP_VERSION_CHECK=1
datasette install --upgrade datasette-cluster-map | (grep -v '^Requirement already satisfied:' || true)
llm install --upgrade llm-{gemini,anthropic,perplexity,cmd,openai-plugin} | (grep -v '^Requirement already satisfied:' || true)

# Make sure System Preferences isn't open, since it interferes with other
# processes writing to defaults.
osascript -e 'tell application "System Preferences" to quit'

# Visual preferences
defaults write NSGlobalDomain AppleReduceDesktopTinting true
defaults write NSGlobalDomain AppleShowAllExtensions true
defaults write NSGlobalDomain AppleShowScrollBars -string 'Always'
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
defaults write NSGlobalDomain NSScrollAnimationEnabled -bool false
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
# A lighter blue than standard '0.698039 0.843137 1.000000 Blue', which
# is OKLCH (0.8633, 0.0695, 251.25). This is in linear RGB.
# The sRGB equivalent is #dfecff.
defaults write NSGlobalDomain AppleHighlightColor '0.56153 0.75605 1.01381 Other'
# Avoid having to sudo by checking this value before writing it. For some
# reason, 1.5 is stored as 1.496458570615034, so compare it as a number.
if [[ $(bc -e "abs($(defaults read com.apple.universalaccess mouseDriverCursorSize) - 1.5) < 0.01") != 1 ]]; then
    sudo defaults write com.apple.universalaccess mouseDriverCursorSize -float 1.5
fi

# Attempt to have the menu bar transparent, but not other things. This
# doesn't entirely take effect immediately.
defaults write com.apple.universalaccess reduceTransparency true
defaults write com.apple.Accessibility EnhancedBackgroundContrastEnabled -int 1
defaults write NSGlobalDomain AppleEnableMenuBarTransparency -bool true

# Keyboard
defaults write com.apple.HIToolbox AppleCurrentKeyboardLayoutInputSourceID org.unknown.keylayout.Shields
defaults write NSGlobalDomain com.apple.keyboard.fnState true
defaults write com.apple.universalaccess stickyKey false
defaults write com.apple.universalaccess stickyKeyBeepOnModifier false
defaults write com.apple.universalaccess stickyKeysLocation -int 1

# Trackpad tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write com.apple.AppleMultitouchTrackpad ActuateDetents -int 0
defaults write com.apple.AppleMultitouchTrackpad ActuationStrength -int 0
defaults write com.apple.AppleMultitouchTrackpad FirstClickThreshold -int 0
defaults write com.apple.AppleMultitouchTrackpad ForceSuppressed true
defaults write com.apple.AppleMultitouchTrackpad SecondClickThreshold -int 0
# Disable dictionary lookups
defaults write NSGlobalDomain 'com.apple.trackpad.forceClick' -bool false
# Scroll down using the flexors of the fingers, which are stronger
# than the extensors.
defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false
# Three-finger trackpad drag
defaults write com.apple.AppleMultitouchTrackpad Dragging true
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerHorizSwipeGesture -int 0
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerVertSwipeGesture -int 0
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerHorizSwipeGesture -int 0
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerVertSwipeGesture -int 0
defaults write NSGlobalDomain AppleEnableSwipeNavigateWithScrolls false
# Four-finger swipe between fullscreen apps
defaults write com.apple.AppleMultitouchTrackpad TrackpadFourFingerHorizSwipeGesture -int 2
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadFourFingerHorizSwipeGesture -int 2
# Tracking speed
defaults write NSGlobalDomain 'com.apple.trackpad.scaling' -float 0.875

# Use standard units
defaults write NSGlobalDomain AppleICUDateFormatStrings -dict 1 y-MM-dd
defaults write NSGlobalDomain AppleMeasurementUnits -string 'Centimeters'
defaults write NSGlobalDomain AppleMetricUnits -bool true
defaults write NSGlobalDomain AppleTemperatureUnit -string 'Celsius'

# App Store preferences
defaults write com.apple.appstore ShowDebugMenu -bool true
defaults write com.apple.appstore WebKitDeveloperExtras -bool true
defaults write com.apple.commerce AutoUpdate -bool true
defaults write com.apple.commerce AutoUpdateRestartRequired -bool true

# Only write .DS_Store locally
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Calculator preferences
defaults write com.apple.calculator RPNDefaultsKey true
defaults write com.apple.calculator SeparatorsDefaultsKey true
defaults write com.apple.calculator ViewDefaultsKey Scientific

# Dock preferences
defaults write com.apple.Dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 1e9
defaults write com.apple.dock mru-spaces -bool false
defaults write com.apple.dock orientation left
defaults write com.apple.dock show-recents -bool false
defaults write com.apple.dock showLaunchpadGestureEnabled -int 0
defaults write com.apple.dock 'tilesize' -float 96.0

# Finder preferences
defaults write com.apple.finder FXRemoveOldTrashItems true
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
defaults write com.apple.finder DisableAllAnimations -bool true
defaults write com.apple.finder ShowPathbar -bool true
defaults write com.apple.finder QuitMenuItem -bool true

# Menu bar clock preferences; see https://github.com/tech-otaku/menu-bar-clock
#
# The format string here is not used literally, and only certain complete
# strings are valid. You might expect "d MMM" to display "4 Apr", but in a US
# locale it gets regionalized to "Apr 4". "MMM d" is not accepted.
defaults write NSGlobalDomain AppleICUForce12HourTime -bool true
defaults write com.apple.menuextra.clock.plist DateFormat -string 'EEE d MMM h:mm:ss a'
defaults write com.apple.menuextra.clock.plist IsAnalog -bool false
defaults write com.apple.menuextra.clock.plist Show24Hour -bool false
defaults write com.apple.menuextra.clock.plist ShowAMPM -bool true
defaults write com.apple.menuextra.clock.plist ShowDate -int 1
defaults write com.apple.menuextra.clock.plist ShowDayOfWeek -bool true
defaults write com.apple.menuextra.clock.plist ShowSeconds -bool true

# Software Update preferences
defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1
defaults write com.apple.SoftwareUpdate ConfigDataInstall -int 1
defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# Spotlight
defaults write com.apple.assistant.support 'Search Queries Data Sharing Status' -int 2 # off

# System UI preferences
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3
defaults write NSGlobalDomain AppleSpacesSwitchOnActivate false
defaults write NSGlobalDomain InitialKeyRepeat -int 68
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain com.apple.sound.beep.feedback -int 0
defaults write NSGlobalDomain com.apple.sound.uiaudio.enabled -int 0
defaults write com.apple.WindowManager EnableTiledWindowMargins false
defaults write com.apple.WindowManager EnableTilingByEdgeDrag false
defaults write com.apple.controlcenter 'NSStatusItem Preferred Position Sound' -float 256.0
defaults write com.apple.controlcenter 'NSStatusItem Visible Battery' false # Use Stats instead
defaults write com.apple.controlcenter 'NSStatusItem Visible Item-3' false
defaults write com.apple.controlcenter 'NSStatusItem Visible Sound' true

# Lower right hot corner: lock screen.
defaults write com.apple.dock wvous-br-corner -int 13

# Hide pointer after ten seconds
defaults write com.doomlaser.cursorcerer idleHide -float 10.0

# iTerm2 writes its prefs to ~/.iTerm2/com.googlecode.iterm2.plist,
# but doesn't read from there.
defaults import com.googlecode.iterm2 - <.iTerm2/com.googlecode.iterm2.plist

# Set NTP server to Google Public NTP for smeared leap seconds.
if ! grep -q '^server time\.google\.com$' /etc/ntp.conf; then
    sudo systemsetup -setnetworktimeserver time.google.com
fi

# Restart affected processes
killall ControlCenter Finder cfprefsd

# Set background to navy blue.
#
# The color we want is the actual US Navy's navy blue ("America's Navy Licensing
# Guide", Feb 2024, pages 34J and 35), which claims the hex code should be
# 022a3a.
#
# We get a different result by using the eyedropper from those pages -- 0e2938
# from the PDF viewed in Preview 11.0, or 0f2938 from the same PDF viewed in
# Chrome 130.0.6723.70.
#
# The color picker doesn't mention this, but it's actually using a
# display-specific color space, similar but not identical to P3. Using the color
# calculator tool in ColorSync, we can map from sRGB (2, 42, 58) to the Studio
# Display (14, 51, 56) = 0e2938, matching what we get from Preview. Display P3
# would be (14, 41, 56).
#
# You can also confirm that using `desktoppr color 0e2938` gives a lighter color
# than intended on the wallpaper. The eyedropper reports it as 1a3647.
#
# So that's all a mess, and basically means that `deskttoppr color` doesn't
# work. Instead, we create a PNG with an embedded sRGB colorspace and every
# pixel at 022a3a, and use that as the wallpaper. That gives us a desktop that
# the eyedropper reports as 0e2836. Almost correct, good enough for now.
#
# This directory is where desktoppr stores images that it downloads. We copy the
# image directly there, so it doesn't reference this source directory.
desktoppr "$HOME/Library/Application Support/desktoppr/navy_blue.png"

# Emacs setup
emacs --batch --script .emacs.d/provision.el
rm -rf /Applications/Emacs.app
# Per the emacs-plus instructions:
osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'

# rustup
rustup-init --no-modify-path -y >/dev/null

# Bootstrap TLS trust to GitHub SSH trust.
if [ ! -f "$HOME/.ssh/known_hosts" ] || ! grep -q '^github\.com ' "$HOME/.ssh/known_hosts"; then
    mkdir -p "$HOME/.ssh"
    curl -s -L \
        -H "Accept: application/vnd.github+json" \
        -H "X-GitHub-Api-Version: 2022-11-28" \
        https://api.github.com/meta |
        jq -r '.ssh_keys[]' |
        sed -e 's/^/github.com /' >>"$HOME/.ssh/known_hosts"
fi

# Go setup
go telemetry on
GOBIN="$HOME/bin" go install golang.org/x/tools/cmd/goimports@latest

# Reload launchd config.
launchctl bootstrap gui/$UID
