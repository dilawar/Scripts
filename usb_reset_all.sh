#!/usr/bin/env bash
echo "Resetting USB2 ports"
for i in /sys/bus/pci/drivers/ehci_hcd/*:*; do
  echo "${i##*/}" > "${i%/*}/unbind"
  echo "${i##*/}" > "${i%/*}/bind"
done

echo "Resetting USB3 ports"
for i in /sys/bus/pci/drivers/xhci_hcd/*:*; do
  echo "${i##*/}" > "${i%/*}/unbind"
  echo "${i##*/}" > "${i%/*}/bind"
done
