#
# ~/.bashrc
#

## Load the zsh env here as well
source ~/.zshenv

# Process PATH from zshenv
## This came around as part of a fix for the way that
## Apple handles the path population in MacOS for ZSH.
## In MacOS the path is modified after .zshenv is processed
## in the /etc/.zprofile. So in order to be able to put things
## before the system paths we create a NEW_PATH var
## with a placeholder for the system path, then at this
## point we process it and insert the system path into
## that placeholder and then set PATH = NEW_PATH.
##
## We also set the PATH_SET var here to prevent a nested path
## being set in the event of subshells

if [ -z ${PATH_SET+x} ]; then
        export PATH=${NEW_PATH/\$PATH/$PATH}
        export PATH_SET=true
        unset NEW_PATH
fi


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
