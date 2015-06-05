# Path to your oh-my-zsh installation.
ZSH=$HOME/.oh-my-zsh
ZSH_CUSTOM=~/.oh-my-zsh-custom

ZSH_THEME="magnoster"

COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(git emacs)

export PATH=$HOME/bin:/usr/local/bin:$PATH

source $ZSH/oh-my-zsh.sh
