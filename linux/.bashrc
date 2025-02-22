# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

export HISTTIMEFORMAT="%c "
export HISTIGNORE="ls:history"

bind '"^[[A":"history-search-backward"' # arrow up
bind '"^[[B":"history-search-forward"' # arrow up

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'


## Dir Definition
proDir=/home/leonabil/Documents
rlasDir=/home/leonabil/Code/RLAS
codeDir=/home/leonabil/Code

# Aliases
alias sshifms='sshpass -p "4BR3T3s3s4m0XXM" ssh nabil@ifms'
alias sshttcp='sshpass -p "ttcp" ssh ttcp@ttcpd'
alias cat='batcat --theme=Dracula'
alias em='emacs &'
alias ec='emacsclient'
alias cdp='cd $proDir'
alias cdm='cd $rlasDir/MMI/Display'
alias cdr='cd $rlasDir'
alias cdc='cd $codeDir'
alias python='python3'
alias leotar='tar -czvd'
alias untar='tar -xzvf'
alias cp='cp -iv'                           # Preferred 'cp' implementation
alias mv='mv -iv'                           # Preferred 'mv' implementation
alias mkdir='mkdir -pv'                     # Preferred 'mkdir' implementation
alias less='less -FSRXc'                    # Preferred 'less' implementation
alias cd..='cd ../'                         # Go back 1 directory level (for fast typers)
alias ..='cd ../'                           # Go back 1 directory level
alias ...='cd ../../'                       # Go back 2 directory levels
alias .3='cd ../../../'                     # Go back 3 directory levels
alias .4='cd ../../../../'                  # Go back 4 directory levels
alias .5='cd ../../../../../'               # Go back 5 directory levels
alias .6='cd ../../../../../../'            # Go back 6 directory levels
alias ~="cd ~"                              # ~:            Go Home
alias c='clear'                             # c:            Clear terminal display
alias which='type -a'                       # which:        Find executables
alias path='echo -e ${PATH//:/\\n}'         # path:         Echo all executable Paths
alias show_options='shopt'                  # Show_options: display bash options settings
alias lr='ls -R | grep ":$" | sed -e '\''s/:$//'\'' -e '\''s/[^-][^\/]*\//--/g'\'' -e '\''s/^/   /'\'' -e '\''s/-/|/'\'' | less' # lr:  Full Recursive Directory Listing
alias ttop="top -R -F -s 10 -o rsize"       # Recommended 'top' invocation to minimize resources
alias m="meld -n"
alias resource="source ~/.bashrc"           # Easy refresh bashrc
alias git="git -c http.sslVerify=false"     # Avoid SSL verification when GIT
mcd () { mkdir -p "$1" && cd "$1"; }        # mcd:          Makes new Dir and jumps inside

# Adding directories to path
export PATH=/home/$(whoami)/bin:$PATH

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Exa better ls
alias l="eza --color=always --long --icons --no-user --icons --no-permissions --oneline --sort date -alh"                    # Preferred 'ls' implementation"

# ZOXIDE init
_z_cd() {
    cd "$@" || return "$?"

    if [ "$_ZO_ECHO" = "1" ]; then
        echo "$PWD"
    fi
}

z() {
    if [ "$#" -eq 0 ]; then
        _z_cd ~
    elif [ "$#" -eq 1 ] && [ "$1" = '-' ]; then
        if [ -n "$OLDPWD" ]; then
            _z_cd "$OLDPWD"
        else
            echo 'zoxide: $OLDPWD is not set'
            return 1
        fi
    else
        _zoxide_result="$(zoxide query -- "$@")" && _z_cd "$_zoxide_result"
    fi
}

zi() {
    _zoxide_result="$(zoxide query -i -- "$@")" && _z_cd "$_zoxide_result"
}

alias za='zoxide add'
alias zq='zoxide query'
alias zqi='zoxide query -i'
alias zr='zoxide remove'

zri() {
    _zoxide_result="$(zoxide query -i -- "$@")" && zoxide remove "$_zoxide_result"
}

_zoxide_hook() {
    if [ -z "${_ZO_PWD}" ]; then
        _ZO_PWD="${PWD}"
    elif [ "${_ZO_PWD}" != "${PWD}" ]; then
        _ZO_PWD="${PWD}"
        zoxide add "$(pwd -L)"
    fi
}

case "$PROMPT_COMMAND" in
    *_zoxide_hook*) ;;
    *) PROMPT_COMMAND="_zoxide_hook${PROMPT_COMMAND:+;${PROMPT_COMMAND}}" ;;
esac
eval "$(zoxide init bash)"
alias  cd='z'

# FZF = fuzzy is a better find
export FZF_DEFAULT_OPTS="--height=40% --layout=reverse --info=inline --preview 'batcat {}' --border --margin=1 --padding=1"
export FZF_CTRL_T_OPTS="--preview 'batcat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"

cd_with_fzf() {
    cd $HOME && cd "$(find . -type d | fzf --preview="tree -L 1 {}" --bind="space:toggle-preview" --preview-window=:hidden)"
}

open_with_fzf() {
    find -H -type f | fzf -m --preview="batcat -n --color=always --line-range :500 {}" | xargs -ro -d "\n" xdg-open 2>&-
}

emacs_with_fzf() {
    ec "$(find -H -type f | fzf -m --preview="batcat -n --color=always --line-range :500 {}" --bind="space:toggle-preview" --preview-window=:hidden)"
    }
    #fd -H -i -t f | fzf -m --preview="bat -n --color=always --line-range :500 {}" --bind="space:toggle-preview" --preview-window=":hidden" | xargs -ro -d ec 2>&-
    #find -H -type f | fzf -m --preview="bat -n --color=always --line-range :500 {}" --bind="space:toggle-preview" --preview-window=":hidden" | xargs -ro -d "\n" ec 2>&-
    #}

bind '"\C-f":"cd_with_fzf\n"' # binding for cd
bind '"\C-o":"open_with_fzf\n"' # binding for open
bind '"\C-e":"emacs_with_fzf\n"' # binding for open
alias fz='fzf -m --preview "batcatq -n --color=always --line-range :500 {}" --bind="space:toggle-preview" --preview-window=:hidden'

# fd is a better 
alias fd='fdfind'

# Yazi
export PATH=/home/$(whoami)/yazi:$PATH
alias y="yazi"

# Weather
alias weather='curl wttr.in/chelmsford'

# A better man
alias man='tldr'
