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

# Preview. This ridiculous string is dark gray.
defaults write com.apple.Preview PVGeneralWindowBackgroundColor_v7 -data 62706c6973743030d4010203040506070a582476657273696f6e592461726368697665725424746f7058246f626a6563747312000186a05f100f4e534b657965644172636869766572d10809575f5f636f6c6f728001a60b0c191f202755246e756c6cd60d0e0f1011121314151617185624636c6173735c4e53436f6d706f6e656e7473554e535247425c4e53436f6c6f7253706163655f10124e53437573746f6d436f6c6f7253706163655f10104e534c696e6561724578706f7375726580054f1027302e3537343134383533353720302e3537343136323432333620302e35373431353439373320314f1027302e3530313935383630383620302e3530313937333536393420302e3530313936353130353500100180024131d31a1b0d1c1d1e544e534944554e534943431007800380044f110c4800000c484c696e6f021000006d6e74725247422058595a2007ce00020009000600310000616373704d5346540000000049454320735247420000000000000000000000000000f6d6000100000000d32d4850202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001163707274000001500000003364657363000001840000006c77747074000001f000000014626b707400000204000000147258595a00000218000000146758595a0000022c000000146258595a0000024000000014646d6e640000025400000070646d6464000002c400000088767565640000034c0000008676696577000003d4000000246c756d69000003f8000000146d6561730000040c0000002474656368000004300000000c725452430000043c0000080c675452430000043c0000080c625452430000043c0000080c7465787400000000436f70797269676874202863292031393938204865776c6574742d5061636b61726420436f6d70616e790000646573630000000000000012735247422049454336313936362d322e31000000000000000000000012735247422049454336313936362d322e31000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000058595a20000000000000f35100010000000116cc58595a200000000000000000000000000000000058595a200000000000006fa2000038f50000039058595a2000000000000062990000b785000018da58595a2000000000000024a000000f840000b6cf64657363000000000000001649454320687474703a2f2f7777772e6965632e636800000000000000000000001649454320687474703a2f2f7777772e6965632e63680000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000064657363000000000000002e4945432036313936362d322e312044656661756c742052474220636f6c6f7572207370616365202d207352474200000000000000000000002e4945432036313936362d322e312044656661756c742052474220636f6c6f7572207370616365202d20735247420000000000000000000000000000000000000000000064657363000000000000002c5265666572656e63652056696577696e6720436f6e646974696f6e20696e2049454336313936362d322e3100000000000000000000002c5265666572656e63652056696577696e6720436f6e646974696f6e20696e2049454336313936362d322e31000000000000000000000000000000000000000000000000000076696577000000000013a4fe00145f2e0010cf140003edcc0004130b00035c9e0000000158595a2000000000004c09560050000000571fe76d6561730000000000000001000000000000000000000000000000000000028f0000000273696720000000004352542063757276000000000000040000000005000a000f00140019001e00230028002d00320037003b00400045004a004f00540059005e00630068006d00720077007c00810086008b00900095009a009f00a400a900ae00b200b700bc00c100c600cb00d000d500db00e000e500eb00f000f600fb01010107010d01130119011f0125012b01320138013e0145014c0152015901600167016e0175017c0183018b0192019a01a101a901b101b901c101c901d101d901e101e901f201fa0203020c0214021d0226022f02380241024b0254025d02670271027a0284028e029802a202ac02b602c102cb02d502e002eb02f50300030b03160321032d03380343034f035a03660372037e038a039603a203ae03ba03c703d303e003ec03f9040604130420042d043b0448045504630471047e048c049a04a804b604c404d304e104f004fe050d051c052b053a05490558056705770586059605a605b505c505d505e505f6060606160627063706480659066a067b068c069d06af06c006d106e306f507070719072b073d074f076107740786079907ac07bf07d207e507f8080b081f08320846085a086e0882089608aa08be08d208e708fb09100925093a094f09640979098f09a409ba09cf09e509fb0a110a270a3d0a540a6a0a810a980aae0ac50adc0af30b0b0b220b390b510b690b800b980bb00bc80be10bf90c120c2a0c430c5c0c750c8e0ca70cc00cd90cf30d0d0d260d400d5a0d740d8e0da90dc30dde0df80e130e2e0e490e640e7f0e9b0eb60ed20eee0f090f250f410f5e0f7a0f960fb30fcf0fec1009102610431061107e109b10b910d710f511131131114f116d118c11aa11c911e81207122612451264128412a312c312e31303132313431363138313a413c513e5140614271449146a148b14ad14ce14f01512153415561578159b15bd15e0160316261649166c168f16b216d616fa171d17411765178917ae17d217f7181b18401865188a18af18d518fa19201945196b199119b719dd1a041a2a1a511a771a9e1ac51aec1b141b3b1b631b8a1bb21bda1c021c2a1c521c7b1ca31ccc1cf51d1e1d471d701d991dc31dec1e161e401e6a1e941ebe1ee91f131f3e1f691f941fbf1fea20152041206c209820c420f0211c2148217521a121ce21fb22272255228222af22dd230a23382366239423c223f0241f244d247c24ab24da250925382568259725c725f726272657268726b726e827182749277a27ab27dc280d283f287128a228d429062938296b299d29d02a022a352a682a9b2acf2b022b362b692b9d2bd12c052c392c6e2ca22cd72d0c2d412d762dab2de12e162e4c2e822eb72eee2f242f5a2f912fc72ffe3035306c30a430db3112314a318231ba31f2322a3263329b32d4330d3346337f33b833f1342b3465349e34d83513354d358735c235fd3637367236ae36e937243760379c37d738143850388c38c839053942397f39bc39f93a363a743ab23aef3b2d3b6b3baa3be83c273c653ca43ce33d223d613da13de03e203e603ea03ee03f213f613fa23fe24023406440a640e74129416a41ac41ee4230427242b542f7433a437d43c044034447448a44ce45124555459a45de4622466746ab46f04735477b47c04805484b489148d7491d496349a949f04a374a7d4ac44b0c4b534b9a4be24c2a4c724cba4d024d4a4d934ddc4e254e6e4eb74f004f494f934fdd5027507150bb51065150519b51e65231527c52c75313535f53aa53f65442548f54db5528557555c2560f565c56a956f75744579257e0582f587d58cb591a596959b85a075a565aa65af55b455b955be55c355c865cd65d275d785dc95e1a5e6c5ebd5f0f5f615fb36005605760aa60fc614f61a261f56249629c62f06343639763eb6440649464e9653d659265e7663d669266e8673d679367e9683f689668ec6943699a69f16a486a9f6af76b4f6ba76bff6c576caf6d086d606db96e126e6b6ec46f1e6f786fd1702b708670e0713a719571f0724b72a67301735d73b87414747074cc7528758575e1763e769b76f8775677b37811786e78cc792a798979e77a467aa57b047b637bc27c217c817ce17d417da17e017e627ec27f237f847fe5804780a8810a816b81cd8230829282f4835783ba841d848084e3854785ab860e867286d7873b879f8804886988ce8933899989fe8a648aca8b308b968bfc8c638cca8d318d988dff8e668ece8f368f9e9006906e90d6913f91a89211927a92e3934d93b69420948a94f4955f95c99634969f970a977597e0984c98b89924999099fc9a689ad59b429baf9c1c9c899cf79d649dd29e409eae9f1d9f8b9ffaa069a0d8a147a1b6a226a296a306a376a3e6a456a4c7a538a5a9a61aa68ba6fda76ea7e0a852a8c4a937a9a9aa1caa8fab02ab75abe9ac5cacd0ad44adb8ae2daea1af16af8bb000b075b0eab160b1d6b24bb2c2b338b3aeb425b49cb513b58ab601b679b6f0b768b7e0b859b8d1b94ab9c2ba3bbab5bb2ebba7bc21bc9bbd15bd8fbe0abe84beffbf7abff5c070c0ecc167c1e3c25fc2dbc358c3d4c451c4cec54bc5c8c646c6c3c741c7bfc83dc8bcc93ac9b9ca38cab7cb36cbb6cc35ccb5cd35cdb5ce36ceb6cf37cfb8d039d0bad13cd1bed23fd2c1d344d3c6d449d4cbd54ed5d1d655d6d8d75cd7e0d864d8e8d96cd9f1da76dafbdb80dc05dc8add10dd96de1cdea2df29dfafe036e0bde144e1cce253e2dbe363e3ebe473e4fce584e60de696e71fe7a9e832e8bce946e9d0ea5beae5eb70ebfbec86ed11ed9cee28eeb4ef40efccf058f0e5f172f1fff28cf319f3a7f434f4c2f550f5def66df6fbf78af819f8a8f938f9c7fa57fae7fb77fc07fc98fd29fdbafe4bfedcff6dffffd2212223245a24636c6173736e616d655824636c61737365735c4e53436f6c6f725370616365a225265c4e53436f6c6f725370616365584e534f626a656374d221222829574e53436f6c6f72a2282600080011001a00240029003200370049004c00540056005d0063007000770084008a009700ac00bf00c100eb011501170119011b01220127012d012f013101330d7f0d840d8f0d980da50da80db50dbe0dc30dcb0000000000000201000000000000002a00000000000000000000000000000dce

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

# Have python3 access ~/Downloads to prompt a TCC authorization, allowing
# clean_downloads.py to work.
python3 -c "import os; os.listdir(os.path.expanduser('~/Downloads'))"

# Reload launchd config.
launchctl bootstrap gui/$UID
