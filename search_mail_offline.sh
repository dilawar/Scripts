#!/bin/bash

# This script uses mairix to search and index and then mutt to browse those
# emails. We assume that mairix has been configured beforehand.
mairix $1
mutt -f ~/Mail/mfolder
