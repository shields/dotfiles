Do not try to optimize Emacs startup time. Emacs is started once and then
accessed via emacsclient. Instead, try to have all modules already loaded and
ready to use.
