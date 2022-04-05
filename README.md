# macOS provisioning

1. Download https://github.com/shields/dotfiles/archive/main.zip
2. `sudo softwareupdate --install-rosetta`
3. `./provision.sh`
4. Reboot

Additional steps not yet automated:

- Open Karabiner-Elements and grant permissions
- Open Chrome and sign in
- Open System Preferences:
  - FileVault: enable
  - Displays: set up Night Shift
  - Date & Time > Clock: add seconds and date
  - Users & Groups > Login Items: add Karabiner-Elements

Many preferences can be translated to `defaults write` settings using
`diff_defaults.py`.
