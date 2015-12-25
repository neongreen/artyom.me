#!/bin/sh

rsync --exclude 'John' -PLcr . root@artyom.me:/var/artyom.me/
