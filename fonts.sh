#!/usr/bin/bash
font_path="$HOME/.local/share/fonts"

echo "Check direcory..."
sleep 1
if [ -d $font_path ]
then
	echo "Is already created"
else
	echo "Create directory..."
	mkdir $font_path 
	sleep 1
	echo "Done"
fi

echo "Download fonts..."
# Dowloand fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/IBMPlexMono.zip

#install fonts 
unzip IBMPlexMono.zip -d $font_path #$HOME/.local/share/fonts/

#rm IBMPlexMono.zip


