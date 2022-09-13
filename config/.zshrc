# History Settings
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS

unsetopt beep
bindkey -v

# The following lines were added by compinstall
zstyle :compinstall filename '/home/bcwinter/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Autoload the prompt
autoload -U promptinit
promptinit

# Include aliases
source ~/.zsh_aliases

# Include Work settings if they exist
if [ -f ~/.zsh_work ]; then
	source ~/.zsh_aliases
fi

# Reverse search
bindkey "^R" history-incremental-pattern-search-backward

# Process PATH
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

# Fix home and end key bindings
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line


