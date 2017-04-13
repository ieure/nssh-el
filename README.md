# nSSH for Emacs

nssh is a new SSH mode for Emacs. It takes some inspiration and code
from `ssh-mode`.

## Features

 - Based on TRAMP and shell-mode, can log into any host TRAMP can,
   including proxied hosts (bastion, sudo, etc)
 - History ring of hosts logged into
 - Tab completion of hosts in history and `known_hosts`
 - Tab completion in the remote shell
 - Lightweight & simple
 - Cluster control, can log into & control multiple hosts.

## Installation

Copy `nssh.el` somewhere in your `load-path` and `(require 'nssh)`.

## Usage

`M-x nssh RET` will open a connection. If a live connection is open,
it will be brought to the foreground. If the buffer exists, but the
process is dead, it will reconnect. With a prefix argument, opens a
new connection.

## Clusters

nssh has some basic multi-SSH ability, which you can invoke with
`nssh-cluster`. When you proved a DNS name, nssh resolves it to *N* IP
addresses, and creates one window per address, plus a control
window. Lines entered into the control window are sent to each host.

The control window has some bookkeeping functions:

- `,quit` terminates all connections.
- `,bufs` lists the buffers which `*nssh control*` is sending input to.
- `,tile` retiles the windows.

Cluster mode takes over your frame. If you prefer to keep it in a
dedicated frame, you can use `nssh-cluster-other-frame`.
