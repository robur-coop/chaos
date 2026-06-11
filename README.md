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
registered as a "runtime"). The user needs to prepare two files (the
[Dockerfile](./docker/Dockerfile) and the [solo5.json](./docker/solo5.json)
file) in order to build and run the unikernel:

**Dockerfile**:
```Dockerfile
FROM ocaml/opam:debian-12-ocaml-5.4 AS builder
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
  pkg-config m4 build-essential libgmp-dev libseccomp-dev \
  && rm -rf /var/lib/apt/lists/*
USER opam
RUN opam update && opam install -y solo5 ocaml-solo5
RUN git clone https://github.com/robur-coop/chaos chaos
WORKDIR /home/opam/chaos
RUN opam pin -yn .
RUN opam install --deps-only chaos
RUN opam exec -- make all

FROM scratch
COPY --from=builder /home/opam/chaos/chaos.hvt /chaos.hvt
COPY solo5.json /solo5.json
ENTRYPOINT ["/chaos.hvt"]
```

**solo5.json**:
```json
{
  "version": 1,
  "type": "solo5.config",
  "mem": 64,
  "nets":   { "service": { "type": "docker", "iface": "eth0" } },
  "argv": [
    "--ipv4=%{solo5.net.service.ip}",
    "--ipv4-gateway=%{solo5.net.service.gw}",
    "--server 0.fr.pool.ntp.org",
    "--server ntp1.jussieu.fr",
    "--color=always"
  ]
}
```

Next, you can simply start the unikernel like this (make sure port 123 is open;
a service like `ntpd` is usually already running):
```shell
$ docker run --runtime=solo5 -p 123:123/udp chaos
            |      ___|
  __|  _ \  |  _ \ __ \
\__ \ (   | | (   |  ) |
____/\___/ _|\___/____/
Solo5: Bindings version v0.11.0
Solo5: Memory map: 65 MB addressable:
Solo5:   reserved @ (0x0 - 0xfffff)
Solo5:       text @ (0x100000 - 0x469fff)
Solo5:     rodata @ (0x46a000 - 0x513fff)
Solo5:       data @ (0x514000 - 0xa37fff)
Solo5:       heap >= 0xa38000 < stack < 0x41f3000
```

After letting it warm-up for a while, you can retrieve the time from Chaos using
`sntp localhost`.

## License

Since the code is heavily inspired by [chrony][chrony] and the project includes
the core code for performing linear regression, this project is licensed under
the GPL-2.0-only. We would like to thank the authors and maintainers of
[chrony][chrony] who, in our humble opinion, have produced very readable C
code (from an avid reader and maintainer of [Solo5][solo5]).

[solo5]: https://github.com/solo5/solo5
[albatross]: https://github.com/robur-coop/albatross
[aussi]: https://github.com/robur-coop/aussi
[docker]: https://www.docker.com/
[chrony]: https://chrony-project.org/
