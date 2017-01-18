# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=5000
SAVEHIST=5000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/taizo/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export SVN_EDITOR=/usr/bin/vim
export PATH=/Library/TeX/texbin:$PATH
export PATH=~/bin:$PATH
export ANDROID_HOME=/usr/local/Cellar/android-sdk

case "${TERM}" in
    screen)
        preexec() {
            echo -ne "\ek#${1%% *}\e\\"
        }
        precmd() {
            echo -ne "\ek$(basename $(pwd))\e\\"
            echo -ne "\eP\e]0;$(pwd)\a\e\134"
        }
        ;;
    xterm)
        precmd() {
            eval 'echo -ne "\033]0;$PWD\007"'
        }
        ;;
esac

##### alias begin
alias ll='ls -l'
alias la='ls -a'

setopt extended_glob

typeset -A abbreviations
abbreviations=(
    "L"    "| lv"
    "G"    "| grep"
    "X"    "| xargs"
    "T"    "| tail"
    "C"    "| cat"
    "W"    "| wc"
    "A"    "| awk"
    "S"    "| sed"
    "E"    "2>&1 > /dev/null"
    "N"    "> /dev/null"
)

magic-abbrev-expand() {
    local MATCH
    LBUFFER=${LBUFFER%%(#m)[-_a-zA-Z0-9]#}
    LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
    zle self-insert

}

no-magic-abbrev-expand() {
    LBUFFER+=' '

}

zle -N magic-abbrev-expand
zle -N no-magic-abbrev-expand
bindkey " " magic-abbrev-expand
bindkey "^x " no-magic-abbrev-expand
##### alias end



# C-q でコマンドラインスタックに退避しつつ内容を表示
show_buffer_stack() {
  POSTDISPLAY="
stack: $LBUFFER"
  zle push-line-or-edit
}
zle -N show_buffer_stack
setopt noflowcontrol
bindkey '^Q' show_buffer_stack


fpath=(/usr/local/share/zsh-completions $fpath)

[ -f ~/.zshrc_local ] && . ~/.zshrc_local
