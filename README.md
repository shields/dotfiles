# macOS provisioning

1. System Preferences > Security & Privacy > Full Disk Access > Terminal (mathiasbynens/dotfiles#849)
2. Download https://github.com/shields/dotfiles/archive/main.zip
3. `cd ~/Downloads/dotfiles-main`
4. `./provision.sh`
5. Reboot

Additional steps not yet automated:

- Open Karabiner-Elements and grant permissions
- Open Chrome and sign in
- Open System Preferences:
  - General: disable font smoothing
  - Printers & Scanners: add printer
  - Displays: set up Night Shift
  - Date & Time > Clock: add seconds and date
  - Users & Groups > Login Items: add Karabiner-Elements

Many preferences can be translated to `defaults write` settings using
`diff_defaults.py`.
