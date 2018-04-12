#!/bin/bash
set -e
########################################

time  stack build  -j6  "$@" 

########################################

#NOTES
#  --stack-yaml STACK-YAML  Override project stack.yaml file (overrides any
#                           STACK_YAML environment variable)
