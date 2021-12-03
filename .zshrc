# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

export PATH=$HOME/bin:$HOME/go/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="spaceship"
SPACESHIP_PROMPT_ORDER=(user host dir git venv kubectl exec_time jobs exit_code char)
SPACESHIP_RPOMPT_ORDER=()
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_PROMPT_PREFIXES_SHOW=false
SPACESHIP_CHAR_SYMBOL='$'
SPACESHIP_CHAR_SUFFIX=' '
SPACESHIP_DIR_COLOR=000000
SPACESHIP_DIR_LOCK_SYMBOL=''
SPACESHIP_DIR_TRUNC=0
SPACESHIP_DIR_TRUNC_REPO=false
SPACESHIP_GIT_SYMBOL=''
SPACESHIP_GIT_BRANCH_PREFIX=''
SPACESHIP_GIT_STATUS_PREFIX=''
SPACESHIP_GIT_STATUS_SUFFIX=''
SPACESHIP_EXIT_CODE_SHOW=true
SPACESHIP_EXIT_CODE_SYMBOL=''

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(autojump docker emacs git kubectl pip python terraform)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias drit='docker run -it --rm'

alias gc='gcloud'

alias gdi='git diff origin/main'

alias kc='kubectl'
alias kcy='kubectl -o yaml'

alias p='python3'

alias tf='terraform'
alias tfa='terraform apply -parallelism=100'
alias tfi='terraform init'
alias tfia='tfi && tfa'
alias tfp='terraform plan -parallelism=100 -refresh=false'
alias tfpr='terraform plan -parallelism=100 -refresh=true'

for f in /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/*.zsh.inc(.N); do
    source $f
done

# Asking about the merge commit message is unnecessary, since in the
# rare cases where I want to edit it, there is always --amend.
export GIT_MERGE_AUTOEDIT=no

export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# Emacs shell dir tracking; see comments in term.el.
if [ -n "$INSIDE_EMACS" ]; then
    chpwd() {
	echo -n '\032/'
	pwd
    }
fi

# Even in 2020, macOS 10.15.6 doesn't fully support UTF-8 by default.
# In particular, pbcopy doesn't work correctly.  The fix for this is
# very obscure.  Because users deserve choice, it can be fixed using a
# file or using an environment variable, neither of which is a
# standard macOS preference mechanism.  Let's just do both.
#
# https://developer.apple.com/documentation/corefoundation/cfstringbuiltinencodings/utf8
# https://superuser.com/questions/82123/mac-whats-cfusertextencoding-for
export __CF_USER_TEXT_ENCODING="$(id -u):134217984:134217984"

# Needed by Terraform:
export KUBE_CONFIG_PATH="$HOME/.kube/config"
