#!/bin/bash

#grep first line containing "Logitech USB Receiver", field 2 after tab, field 2 after "=":
g7=$(xinput | grep -m1 "Logitech USB Receiver" | cut -f2 | cut -d= -f2)

#set property
xinput --set-prop $g7 "Device Accel Constant Deceleration" 2.5
