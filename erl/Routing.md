# OSC Routing API

The Erlang OSC router-scheduler needs to be able to handle the following scenarios:

1) A well-defined Sonic Pi specific timestamped bundle on port A
2) Arbitrary OSC messages from unknown sources on port B


## Well defined SP bundles

We define a SP bundle to have the following attributes:

1) Received on port A
2) A standard bundle timestamp
3) An 'action' OSC packet
4) One or more optional payload OSC packets

### Bundle timestamp

The timestamp denotes when to schedule the action defined in the OSC
message. This is the time as of the system clock of the machine running
the scheduler. If the time of the timestamp is in the past, the action
is carried out immediately. Otherwise the action is delayed by the
appropriate time (current time - bundle timestamp time) and then carried
out.

### Action packet

This is a standard OSC packet (not a bundle) which contains information
regarding the action wished to be performed. Current actions include:

* `/forward (si) [hostname, port-number]` - forwards the payload packets
  individaually to the hostname:port-number
* `/forward-bundle (isi) [time-stamp, hostname, port-number]` - creates
  a new bundle with the specified timestamp with payload packets as
  contents and forwards the single composite bundle onto
  hostname:port-number



### Payload packets

These are either a standard OSC packet or a number of OSC packets. This
need not be unpacked, but should be treated as opaque binary blobs.


## Aribrary OSC messages

It should be possible to dynamically define regexp-like pattern matchers which
operate on OSC messages received on port B. For example:

* `/foo/*/bar` - forward to host:port

This configuration may take place through a specific OSC API on port C
which could be used to create and remove these pattern
matchers. Alternatively it could be a simple config file which is read
at boot time.




