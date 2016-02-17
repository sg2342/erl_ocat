#!/bin/sh

ifconfig tun99 destroy
ifconfig tun99 create
ifconfig tun99 inet6 fd87:d87e:eb43:826:7eb9:cee8:50ae:888d prefixlen 48 -auto_linklocal up
route add -inet6 -net fd87:d87e:eb43:: -prefixlen 48 -iface tun99
chown sg /dev/tun99

