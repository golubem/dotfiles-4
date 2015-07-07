#!/bin/bash

dir=~/dotfiles
olddir=~/dotfiles_old

workdirs="home config"

# Create folder for backups if not exists
create_dir() {
    if [[ ! -d "$1" ]]; then
        echo -n "Creating $1 for backup of any existing dotfiles in $HOME ..."
        mkdir -p $1
        echo "done"
    fi
}

create_dir $olddir

# cd to dotfiles dir
echo -n "Going to $dir ..."
cd $dir
echo "done"

# Move files from ~ to olddir then creating symlink from dir to ~
backup_and_link() {
    prefix=""
    case $1 in
        "home") workdir=~; prefix=. ;;
        "config") workdir=~/.config ;;
    esac

    for file in $(ls $dir/$1); do
        echo -n "Moving $file to $olddir/$1 ..."
        mv $workdir/$prefix$file ~/dotfiles_old/ && echo "done"
        echo -n "Creating symlink to $file in home directory ..."
        ln -s $dir/$file $workdir/$prefix$file && echo "done"
    done
}

for cur_dir in $workdirs; do
    create_dir $olddir/$cur_dir
    backup_and_link $cur_dir
done
