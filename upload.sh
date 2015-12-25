#!/bin/sh

rsync --exclude '.git' -PLcr . root@artyom.me:/var/artyom.me/
