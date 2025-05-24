#!/bin/bash

sml @SMLcmdname=$0 @SMLload=bin/c0c.heap.x86-`uname | tr A-Z a-z` $*
