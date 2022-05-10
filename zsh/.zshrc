# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/danilshvalov/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
# COMPLETION_WAITING_DOTS="true"

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

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git zsh-syntax-highlighting zsh-autosuggestions per-directory-history
)

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

alias pdfviewer="/System/Applications/Preview.app/Contents/MacOS/Preview"


path=(
    # tex
    /usr/local/texlive/2021/bin/universal-darwin
    /Library/TeX/texbin

    # apple
    /Library/Apple/usr/bin

    # brew
    /opt/homebrew/bin
    /opt/homebrew/bin
    /opt/homebrew/sbin

    # python
    /opt/homebrew/Cellar/fontforge/20201107/lib/python3.9/site-packages/
    /Library/Frameworks/Python.framework/Versions/3.9/bin
    /Users/danilshvalov/Library/Python/3.9/bin/
    /Users/danilshvalov/venv/lib/python3.9/site-packages
    /Users/danilshvalov/venv/bin

    # cpp
    /opt/homebrew/Cellar/llvm/12.0.0_1/bin
    /opt/homebrew/Cellar/llvm/12.0.1/bin/
    /Users/danilshvalov/Documents/dev/programs/vcpkg

    # rust
    /Users/danilshvalov/.cargo/bin

    # editors
    /opt/local/bin/code
    /Applications/Sublime\ Text.app/Contents/SharedSupport/bin
    /Users/danilshvalov/idea-scripts

    # dotnet
    /usr/local/share/dotnet
    /Users/danilshvalov/.dotnet/tools
    /Library/Frameworks/Mono.framework/Versions/Current/Commands

    $path
)


# WARN: DEPRECATED
export CPATH=/Library/Developer/CommandLineTools/SDKs/MacOSX11.3.sdk/System/Library/Perl/5.30/darwin-thread-multi-2level/CORE:$CPATH

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# history file
HISTFILE=~/.zhistory
# столько команд запоминается в одной сессии zsh и будет выведено на экран по команде history
# history session size
HISTSIZE=1000
# history file size
SAVEHIST=1000
# при закрытии сессии новые команды будут накапливаться в файле истории
# кроме того в файл истории будут попадать команды из разных сессий
# если открыто несколько сессий одновременно
setopt APPEND_HISTORY
# ignore duplicate commands
setopt HIST_IGNORE_ALL_DUPS
# ignore extra whitespaces
setopt HIST_IGNORE_SPACE
# ignore empty lines
setopt HIST_REDUCE_BLANKS

test -r "~/.dir_colors" && eval $(dircolors ~/.dir_colors)
alias pdfviewer="open -a Skim"
export EDITOR=nvim
export MANPAGER="/bin/sh -c \"col -b | nvim -c 'set ft=man ts=8 nomod nolist nonu noma titlestring=MANPAGE' -\""
export LANG=en_GB.UTF-8


TERM=xterm-256color

alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'
eval "$(zoxide init zsh)"

alias tmux='env TERM=screen-256color tmux'

bashcompinit
source /Users/danilshvalov/Documents/dev/programs/vcpkg/scripts/vcpkg_completion.zsh

alias vi=nvim

export VCPKG_ROOT="/Users/danilshvalov/Documents/dev/programs/vcpkg"

# clang
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
