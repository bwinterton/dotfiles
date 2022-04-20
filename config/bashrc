#
# ~/.bashrc
#

## Load the zsh env here as well
source ~/.zshenv

# If not running interactively, don't do anything else
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

## Load the zsh aliases here as well
source ~/.zsh_aliases

## Exercism Autocomplete
if [ -f ~/.config/exercism/exercism_completion.bash ]; then
  . ~/.config/exercism/exercism_completion.bash
fi

## Homebrew's Autcompletes
if [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
