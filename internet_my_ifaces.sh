#!/usr/bin/env bash
# Show active interfaces.
# SOURCE: https://www.linuxquestions.org/questions/linux-newbie-8/conky-and-changing-the-network-interface-for-whatever-it-may-be-4175650256/
ip link | awk '/2:/{print $2}' | rev | cut -c 2- | rev
