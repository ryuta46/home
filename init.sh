#! /bin/sh

script_dir=$(cd $(dirname ${BASH_SOURCE:-$0}); pwd)

function create_symbolic_link() {
    real=$1
    link=$2
    if [ -e "${link}" ]; then
        echo "unlink ${link}"
        unlink "${link}"
    fi
    ln -sv "${script_dir}/${real}" "${link}"
}

create_symbolic_link .screenrc ~/.screenrc
create_symbolic_link .zshrc ~/.zshrc
create_symbolic_link .emacs.d ~/.emacs.d
create_symbolic_link init.lua ~/.hammerspoon/init.lua
#create_symbolic_link private.xml ~/Library/"Application Support"/Karabiner/private.xml
