#!/bin/bash
# Switches between dvorak, diktor and basic layouts

if [[ $1 == -d ]]; then
    setxkbmap us,ru dvp,diktor
elif [[ $1 == -b ]]; then
    setxkbmap us,ru
elif [[ $1 == -t ]]; then
    if setxkbmap -print | grep -q "dvp"; then
        setxkbmap us,ru
    else
        setxkbmap us,ru dvp,diktor
    fi
else
    echo "Use -d for dvp and diktor, -b for basic layout, -t to toggle"
fi
