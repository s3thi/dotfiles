HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd
bindkey -e
zstyle :compinstall filename '/home/ankur/.zshrc'

autoload -Uz compinit promptinit
compinit
promptinit
prompt suse

source ~/.zsh_aliases

export EDITOR="vim"
export PATH=$(ruby -rubygems -e "puts Gem.user_dir")/bin:$PATH

# virtualenvwrapper setup
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python2
export WORKON_HOME=$HOME/venv/
export PROJECT_HOME=$HOME/projects/
source /usr/bin/virtualenvwrapper.sh
