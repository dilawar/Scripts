#!/bin/bash

# This script uses mairix to search and index and then mutt to browse those
# emails. We assume that mairix has been configured beforehand.
mairix $@
mutt -f ~/Mail/mfolder
