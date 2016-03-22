# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
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

# Fix home and end key bindings
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line


