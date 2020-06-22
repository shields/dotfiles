#!/bin/sh

set -eu

# Copy these files.
tar cf - bin $(find . -type f | grep -v -e '^\./\.git/' -e '^\./[^.]' -e '^\./\.mypy_cache') \
    | (cd "$HOME" && tar xvf -)

# Install Homebrew and Xcode (which will take tens of minutes).
if [[ ! -d /usr/local/Homebrew ]]; then
    # CI=1 suppresses confirmation prompts.
    CI=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

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
brew update
brew upgrade
brew install \
    aspell \
    autojump \
    black \
    coreutils \
    dockutil \
    emacs-plus@27 \
    findutils \
    git \
    golangci/tap/golangci-lint \
    gnu-sed \
    gnupg \
    go \
    mypy \
    openssh \
    python@3.8 \
    ripgrep \
    watch \
    wget \
    youtube-dl \
    zsh
brew cask install \
    bettertouchtool \
    docker \
    google-chrome-beta \
    iterm2 \
    karabiner-elements \
    sonos-s1-controller \
    spotify \
    virtualbox \
    wireshark \
    yubico-yubikey-manager
brew services start d12frosted/emacs-plus/emacs-plus@27

# Set shell to current zsh installed from Homebrew.
if [[ "$(dscl . read /Users/$(whoami) UserShell)" == /bin/zsh ]]; then
    sudo dscl . change "/Users/$(whoami)" UserShell /bin/zsh /usr/local/bin/zsh
fi

# Make sure System Preferences isn't open, since it interferes with other
# processes writing to defaults.
osascript -e 'tell application "System Preferences" to quit'

# Visual preferences
defaults write NSGlobalDomain AppleShowScrollBars -string 'Always'
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
defaults write NSGlobalDomain NSScrollAnimationEnabled -bool false
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
if [ ! "$(defaults read com.apple.universalaccess reduceTransparency)" = 1 ]; then
    defaults write com.apple.universalaccess reduceTransparency -bool true
fi

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
# Three-finger drag
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true
# Four-finger swipe between fullscreen apps
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad 'TrackpadThreeFingerHorizSwipeGesture' -int 0
defaults write com.apple.AppleMultitouchTrackpad 'TrackpadThreeFingerHorizSwipeGesture' -int 0
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

# Dock preferences
defaults write com.apple.Dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -float 0
defaults write com.apple.dock mru-spaces -bool false
defaults write com.apple.dock show-recents -bool false
defaults write com.apple.dock showLaunchpadGestureEnabled -int 0
defaults write com.apple.dock 'tilesize' -float 96.0
# Clear and reset the persistent apps.
defaults write com.apple.dock persistent-apps -array
dockutil --add '/Applications/Google Chrome.app'
dockutil --add '/Applications/iTerm.app'
dockutil --add /usr/local/Cellar/emacs-plus@27/*/Emacs.app

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
defaults write com.apple.systemuiserver 'NSStatusItem Visible com.apple.menuextra.bluetooth' -bool true
defaults write com.apple.systemuiserver 'NSStatusItem Visible com.apple.menuextra.volume' -bool true

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
