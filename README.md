# Chaos, a NTP server as an unikernel in OCaml

Chaos is an NTP server implemented as a unikernel that can determine the
current time by combining multiple sources available on a network. Chaos is
heavily inspired by the [Chrony][chrony] project (another NTP server),
particularly in its approach to selecting sources based on the results of a
linear regression analysis of NTP packets from that source.

Like any [Solo5][solo5] unikernel, the resulting binary can be deployed using
[Albatross][albatross] or [Aussi][aussi]. Please refer to these projects for
instructions on how to deploy Chaos. However, here is an example of how to
deploy Chaos using [Docker][docker] (assuming [Aussi][aussi] is installed and
registered as a "runtime").

[solo5]: https://github.com/solo5/solo5
[albatross]: https://github.com/robur-coop/albatross
[aussi]: https://github.com/robur-coop/aussi
[docker]: https://www.docker.com/
