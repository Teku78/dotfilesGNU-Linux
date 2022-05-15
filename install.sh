#!/usr/bin/bash
# Install and update my dotfiles with git and github


# it's better with prety colors, right...
resetColor="\e[0m"
fgRed="\033[0;32m"
fgYellow="\033[0;33m"

GITHUB_REPO="https://github.com/tekuHZ/dotfiles.git"
DIR="$HOME/my_dots"

download_dotfiles() {
    echo  -e "Creating a temporal directory"
    mkdir -p "${DIR}"

    echo  -e "Download dotfiles to ${DIR}"
    git clone ${GITHUB_REPO} ${DIR}
}

link_dotfiles() {
    for dots in "$HOME/dotfiles"
    do
        if [ -d $dots ]
        then
            echo " $dots "
        else
            echo " no Exsiste"

        fi

    done


}




install_dots() {

    #download_dotfiles
    link_dotfiles
}

install_dots


