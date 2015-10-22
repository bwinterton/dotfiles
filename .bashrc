#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

## Load the zsh env here as well
source ~/.zshenv

## Load the zsh aliases here as well
source ~/.zsh_aliases
