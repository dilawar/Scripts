#!/bin/bash
VBoxManage createvm --name "khidki" --ostype WindowsXP --register
VBoxManage modifyvm "khidki" --memory 1024 --acpi on --boot1 dvd --nic1 nat
VBoxManage createhd --filename "khidki.vdi" --size 40000
VBoxManage storagectl "khidki" --name "IDE Controller" --add ide --controller PIIX4
VBoxManage storageattach "khidki" --storagectl "IDE Controller" --port 0 --device 0 --type hdd --medium "khidki.vdi"
VBoxManage storageattach "khidki" --storagectl "IDE Controller" --port 0 --device 1 --type dvddrive --medium /full/path/to/iso.iso
