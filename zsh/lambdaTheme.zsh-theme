# my custom zsh theme

: '
%n user name
%m machine name
%/ current directory
RPROMPT right information
%B to start bold
%b to stop bold
%# show when are you how super user
%F to start color
%f to stop color
'

username() {
    echo "%(?.$(ok_username).$(err_username))"
}
ok_username() {
    echo "%{$FG[074]%}%n%{$reset_color%}"
}
err_username() {
    echo " x $(ok_username)"
}
directory() {
    echo "%{$FG[117]%}%~%{$reset_color%}"
}

return_status(){
    echo "\n%(?.%{$FG[049]%}λ %{$reset_color%}>>= .%{$FG[197]%}λ %{$reset_color%}>>= ) "
}


# Putting it all together
PROMPT='$(directory) $(git_prompt_info) '
PROMPT+='$(return_status)'
ZSH_THEME_GIT_PROMPT_PREFIX="%{$FG[069]%}git:(%{$FG[197]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$FG[069]%}) %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[078]%})"











# vim:ft=bash
