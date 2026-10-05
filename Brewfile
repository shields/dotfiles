# Every module except macos must install on Linux too: its formulae are bottled
# or buildable there and its casks have Linux variants.
# Brew filters most variables out of a Brewfile's environment but keeps HOMEBREW_*.
reserved = %w[base macos linux]
# brew evaluates a Brewfile with its path as the filename, so __FILE__ is a real
# path that locates brew/ even when the Brewfile is reached through a symlink.
modules_dir = File.join(File.dirname(File.realpath(__FILE__)), "brew")
available = Dir.glob("*.Brewfile", base: modules_dir).map { |f| File.basename(f, ".Brewfile") }.sort
selection_file = File.join(Dir.home, ".config/dotfiles/brew-modules")
# The selection is a plain space-separated list of names with no comment syntax,
# so stray text fails loudly as an unknown module.
selection = ENV.fetch("HOMEBREW_DOTFILES_BREW_MODULES") do
  if File.exist?(selection_file)
    File.read(selection_file)
  elsif OS.mac?
    (available - reserved).join(" ")
  else
    # Headless Linux boxes need only the development tools; other modules are opt-in.
    "dev"
  end
end
optional = selection.split - ["none"]
unless (bad = optional & reserved).empty?
  raise "Brewfile modules #{bad.join(", ")} are reserved and cannot be selected; base and the OS module always load"
end
unless (unknown = optional - available).empty?
  raise "No such Brewfile module: #{unknown.map { |name| File.join(modules_dir, "#{name}.Brewfile") }.join(", ")}"
end
["base", OS.mac? ? "macos" : "linux", *optional].uniq.each do |name|
  path = File.join(modules_dir, "#{name}.Brewfile")
  instance_eval(File.read(path), path)
end
