# macOS provisioning

1. Download https://github.com/shields/dotfiles/archive/main.zip
1. `cd ~/Downloads/dotfiles-main`
1. `./provision.sh`
1. Reboot

Additional steps not yet automated:

- Open Karabiner-Elements and grant permissions
- Open Chrome and sign in
- Open System Preferences:
  - Users & Groups > Login Items: add Karabiner-Elements

Many preferences can be translated to `defaults write` settings using
`diff_defaults.py`.
