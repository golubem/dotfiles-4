#!/bin/bash

LED=/proc/acpi/ibm/led
BAT=/sys/class/power_supply/BAT0
WARN_LVL=13

CAP=$(<$BAT/capacity)

if ( grep -q Discharging $BAT/status ) && [[ $CAP -le $WARN_LVL ]]; then
    echo "0 blink" > $LED
    twmnc -c "WARNING: battery level is $CAP" -d 5000 --bg "#cc6666"
else
    echo "0 on" > $LED
fi
