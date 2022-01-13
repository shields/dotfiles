#!/bin/sh

set -eu

# Copy these files.
tar cf - bin $(find . -type f | grep -v -e '^\./\.git/' -e '^\./[^.]' -e '^\./\.mypy_cache') \
    | (cd "$HOME" && tar xvf -)

# Install Homebrew and Xcode (which will take tens of minutes).
# Use path selection logic from https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh
UNAME_MACHINE="$(/usr/bin/uname -m)"
if [[ "${UNAME_MACHINE}" == "arm64" ]]; then
    HOMEBREW_PREFIX="/opt/homebrew"
    HOMEBREW_REPOSITORY="${HOMEBREW_PREFIX}"
else
    HOMEBREW_PREFIX="/usr/local"
    HOMEBREW_REPOSITORY="${HOMEBREW_PREFIX}/Homebrew"
fi
if [[ ! -d "$HOMEBREW_REPOSITORY" ]]; then
    # CI=1 suppresses confirmation prompts.
    CI=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi
eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"

# The interesting part of
# https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh
if [[ ! -d "$HOME/.oh-my-zsh" ]]; then
    git clone --depth=1 https://github.com/ohmyzsh/ohmyzsh "$HOME/.oh-my-zsh"
fi

# Spaceship Prompt
ZSH_CUSTOM="$HOME/.oh-my-zsh/custom"
if [[ ! -d "$ZSH_CUSTOM/themes/spaceship-prompt" ]]; then
    git clone https://github.com/denysdovhan/spaceship-prompt.git "$ZSH_CUSTOM/themes/spaceship-prompt"
    ln -sf spaceship-prompt/spaceship.zsh-theme "$ZSH_CUSTOM/themes/spaceship.zsh-theme"
fi

# Set email address in .gitconfig.
if [[ "$(whoami)" == shields || "$(whoami)" == michaelshields ]]; then
    # Is this a Freenome-managed Mac?
    if [[ "$(uname)" == Darwin ]] && \
	   system_profiler SPConfigurationProfileDataType | grep -qi freenome; then
	git config --global user.email michael.shields@freenome.com
    else
	git config --global user.email shields@msrl.com
    fi
fi

# Pull things in from Homebrew.
brew tap d12frosted/emacs-plus
brew tap homebrew/cask-drivers
brew tap homebrew/cask-versions
brew tap microsoft/git
brew tap osx-cross/avr
brew update
brew upgrade
brew install \
    airfoil \
    aspell \
    autojump \
    avr-gcc \
    black \
    clang-format \
    coreutils \
    docker \
    dockutil \
    emacs-plus@27 \
    fd \
    findutils \
    gh \
    git \
    git-credential-manager-core \
    gnu-sed \
    gnupg \
    go \
    golangci/tap/golangci-lint \
    google-chrome-beta \
    iterm2 \
    jq \
    karabiner-elements \
    mypy \
    node \
    openssh \
    pinentry-mac \
    pstree \
    ripgrep \
    sd \
    spotify \
    teensy_loader_cli \
    vlc \
    watch \
    wget \
    wireshark \
    youtube-dl \
    yq \
    yubico-yubikey-manager \
    zsh
if [[ "${UNAME_MACHINE}" == "x86_64" ]]; then
    brew install virtualbox
fi

brew services start d12frosted/emacs-plus/emacs-plus@27

# Install pyls and associated linters and debugging tools.
pip3 install 'python-language-server[all]' ptvsd

# Set shell to current zsh installed from Homebrew.
if [[ "$(dscl . read /Users/$(whoami) UserShell)" == "UserShell: /bin/zsh" ]]; then
    sudo dscl . change "/Users/$(whoami)" UserShell /bin/zsh "$HOMEBREW_PREFIX/bin/zsh"
fi

# Make sure System Preferences isn't open, since it interferes with other
# processes writing to defaults.
osascript -e 'tell application "System Preferences" to quit'

# Visual preferences
defaults write NSGlobalDomain AppleShowScrollBars -string 'Always'
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
defaults write NSGlobalDomain NSScrollAnimationEnabled -bool false
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
sudo defaults write com.apple.universalaccess mouseDriverCursorSize -float 1.5

# Trackpad tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write com.apple.AppleMultitouchTrackpad 'ActuationStrength' -int 0
# Disable dictionary lookups
defaults write NSGlobalDomain 'com.apple.trackpad.forceClick' -bool false
# Scroll down using the flexors of the fingers, which are stronger
# than the extensors.
defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false
# Three-finger drag and four-finger swipe between fullscreen apps
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true
defaults write com.apple.AppleMultitouchTrackpad 'TrackpadThreeFingerHorizSwipeGesture' -int 0
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad 'TrackpadThreeFingerHorizSwipeGesture' -int 0
defaults write com.apple.AppleMultitouchTrackpad 'TrackpadFourFingerHorizSwipeGesture' -int 2
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad 'TrackpadFourFingerHorizSwipeGesture' -int 2
# Tracking speed
defaults write NSGlobalDomain 'com.apple.trackpad.scaling' -float 0.875

# Use standard units
defaults write NSGlobalDomain AppleMeasurementUnits -string 'Centimeters'
defaults write NSGlobalDomain AppleMetricUnits -bool true

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
# Clear and reset the persistent apps.
defaults write com.apple.dock persistent-apps -array
dockutil --add '/Applications/Google Chrome.app'
dockutil --add '/Applications/iTerm.app'
dockutil --add "$HOMEBREW_CELLAR"/emacs-plus@27/*/Emacs.app

# Finder preferences
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
defaults write com.apple.finder DisableAllAnimations -bool true
defaults write com.apple.finder ShowPathbar -bool true
defaults write com.apple.finder QuitMenuItem -bool true

# Safari preferences
defaults write com.apple.Safari SendDoNotTrackHTTPHeader -bool true

# Software Update preferences
defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1
defaults write com.apple.SoftwareUpdate ConfigDataInstall -int 1
defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# System UI preferences
defaults write NSGlobalDomain 'AppleKeyboardUIMode' -int 3
defaults write NSGlobalDomain 'com.apple.sound.beep.feedback' -int 1
defaults write NSGlobalDomain 'com.apple.sound.uiaudio.enabled' -int 0
defaults write NSGlobalDomain InitialKeyRepeat -int 68
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write com.apple.systemuiserver 'NSStatusItem Visible com.apple.menuextra.bluetooth' -bool true
defaults write com.apple.systemuiserver 'NSStatusItem Visible com.apple.menuextra.volume' -bool true

# Lower right hot corner: lock screen.
defaults write com.apple.dock wvous-br-corner -int 13

# iTerm2 writes its prefs to ~/.iTerm2/com.googlecode.iterm2.plist,
# but doesn't read from there.
defaults import com.googlecode.iterm2 - < .iTerm2/com.googlecode.iterm2.plist

# Set NTP server to Google Public NTP for smeared leap seconds.
if ! fgrep -q '^server time.google.com$' /etc/ntp.conf; then
    sudo systemsetup -setnetworktimeserver time.google.com
fi

# Restart affected processes
killall Finder cfprefsd

# Bootstrap Emacs packages
if [ ! -f "$HOME/.emacs.d/.bootstrap-stamp" ]; then
    emacs -q --batch --script .emacs.d/provision.el
    touch "$HOME/.emacs.d/.bootstrap-stamp"
fi
