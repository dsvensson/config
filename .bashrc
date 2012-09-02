# If not running interactively, don't do anything
[ -z "$PS1" ] && return

function emacs ()
{
 local windowsys="${WINDOW_PARENT+sun}"

 windowsys="${windowsys:-${DISPLAY+x}}"

 if [ -n "${windowsys:+set}" ]; then
    # Do not just test if these files are sockets.  On some systems
    # ordinary files or fifos are used instead.  Just see if they exist.
    if [ -e "${HOME}/.emacs_server" -o -e "/tmp/emacs${UID}/server" ]; then
       /usr/bin/emacsclient -n "$@"
       return $?
    else
       echo "edit: starting emacs in background..." 1>&2
    fi

    case "${windowsys}" in
      x ) (/usr/bin/emacs "$@" &) ;;
      sun ) echo "unsupported window system"; return 1 ;;
    esac
 else
    if jobs %emacs 2> /dev/null ; then
       echo "$(pwd)" "$@" >| ${HOME}/.emacs_args && fg %emacs
    else
       emacs "$@"
    fi
 fi
}

function parse_git_branch
{
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "("${ref#refs/heads/}")"
}

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
fi

# load bash completion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# colorized man-pages
export LESS_TERMCAP_md=$'\E[01;34m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'      # end mode
export LESS_TERMCAP_ue=$'\E[0m'      # end underline
export LESS_TERMCAP_us=$'\E[04;34m'  # begin underline

export TERM=xterm

export PATH=$PATH:~/opt/bin

alias grep="grep --color=auto"

alias less="less -R"
eval "$(lesspipe)"

PS1='\[\033]0;\w $(parse_git_branch)\007\]\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$(parse_git_branch)\$ '

wiki() { dig +short txt $1.wp.dg.cx; }
