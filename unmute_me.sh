#!/bin/bash
for x in `amixer controls  | grep layback` ; do amixer cset "${x}" on ; done
for x in `amixer controls  | grep layback` ; do amixer cset "${x}" 50% ; done
