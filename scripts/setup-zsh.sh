#!/bin/bash

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    sudo apt install -y zsh
fi

sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# install zsh syntax highlighting plugin
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
    $HOME/.config/oh-my-zsh/plugins/zsh-syntax-highlighting
