#!/bin/python
import sys
import subprocess
import re
from gi.repository import Notify

Notify.init("WiFi info")
interface = sys.argv[1]

commandIw = "iw dev {} link".format(interface)

outputIw = subprocess.check_output(['bash', '-c', commandIw]).decode()

iwRe = re.compile(r"SSID: (.+?)\n.*signal: \-(\d+).*bitrate: (\d+).*", re.S)

essid, signal, bitrate = iwRe.findall(outputIw)[0]

signal = int(signal) * 100 // 60

message = "ESSID: {essid} | Bitrate: {bitrate}".format(
    essid=essid, bitrate=bitrate
    )

notification = Notify.Notification.new(interface, message)
notification.set_timeout(4000)
notification.show()
