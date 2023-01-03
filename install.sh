#!/usr/bin/bash

# This my script to install and set up my desktop

function install_pkg () {
	sudo pacman -Syu --noconfirm
	sudo pacman -S --noconfirm $(cat $HOME/.dotfiles/pkglist | grep -v "^#")
}

function install_yay () {
	if ! command -v yay $> /dev/null
	then 
		git clone https://aur.archlinux.org/yay.git $HOME/yay
		(cd $HOME/yay/ && makepkg -si --noconfirm --needed PKGBUILD)
	else
		echo -e "yay is already installed"
		sleep 1; clear
		fi
}

function link_files () {
# link all configurations
	ln -sf $HOME/.dotfiles/alacrity $HOME/.config/
	ln -sf $HOME/.dotfiles/nvim $HOME/.config/
	ln -sf $HOME/.dotfiles/rofi $HOME/.config/
	ln -sf $HOME/.dotfiles/tmux $HOME/.config/
	ln -sf $HOME/.dotfiles/xmonad $HOME/.config/

	ln -sf $HOME/.dotfiles/Xresources $HOME/.Xresources
	ln -sf $HOME/.dotfiles/zsh/zshrc $HOME/.zshrc

}

function clean_up {
	# This when everything finish
	#
	echo " Clean package cache..."
	sudo pacman -Sc --noconfirm &> /dev/null
	echo " Remove unused packages..."
	sudo pacman -Rns $(pacman -Qtdq) --noconfirm &> /dev/null
	echo " Clean cache home..."
	sudo rm -rf $HOME/.cache/* 

}

#install_pkg
#install_yay
#clean_up
