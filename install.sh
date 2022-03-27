#!/bin/bash

#autor: teku

: ' NOTES

'
echo Bash script for install my setup :v

# it's better with prety colors, right...
resetColor="\e[0m"

redB='#dd3333'
gjreenB="\033[0;32m"
yellowB="\033[0;33m"

#######################
#   Install package   #
#######################

declare -a pgks=("vlc"
                 "firefox"
                 "feh"
                 "xmobar"
             )
for pkg in ${pgks[@]}; do
    echo $pkg
done



link_dotfiles() {
# Check for dir, if not found create it use the command mkdir
listDir=(
    "$HOME/.config/picom/"
    "$HOME/.config/rofi/"
    "$HOME/.config/dunst/"
    )
for i in $listDir
do
    if [ ! -d $i] then
        echo $i
    else
        mkdir -vp $i
    fi
done


}


