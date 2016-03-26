#!/bin/bash

dir=$(readlink -f $(dirname $0))
olddir=~/.dotfiles_old
workdirs="home config"

FAILED='\033[0;31mFAILED\033[0m'

create_dir() {
    if [[ ! -d "$1" ]]; then
        echo -n "Creating $1 for backup of any existing dotfiles in $HOME ..."
        mkdir -p $1
        echo "done"
    fi
}

backup_and_link() {
    prefix=""
    case $1 in
        "home") workdir=~; prefix=. ;;
        "config") workdir=~/.config ;;
    esac

    for file in $(ls $dir/$1); do
        echo -n "Moving $file to $olddir/$1 ..."
        mv $workdir/$prefix$file $olddir/ && echo "done"
        echo -n "Creating symlink to $file in home directory ..."
        ln -s $dir/$1/$file $workdir/$prefix$file && echo "done"
    done
}


create_dir $olddir

echo -n "Going to $dir ..."
cd $dir
echo "done"

for cur_dir in $workdirs; do
    create_dir $olddir/$cur_dir
    backup_and_link $cur_dir
done
