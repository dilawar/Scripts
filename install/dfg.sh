#!/bin/bash
(
  cd /tmp
  curl -o dfg -L https://github.com/ozankasikci/dockerfile-generator/releases/download/v1.0.0/dfg_v1.0.0_linux_amd64
  chmod +x dfg && sudo mv dfg $HOME/.local/bin
)

