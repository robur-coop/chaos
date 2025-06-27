```shell
$ sudo ip link add name service type bridge
$ sudo ip addr add 10.0.0.1/24 dev service
$ sudo tuntap add name tap0 mode tap
$ sudo ip link set tap0 master service
$ sudo ip link set service up
$ sudo ip link dev tap0 up
$ sudo iptables -t nat -A POSTROUTING -s 10.0.0.0/24 -o wlan0 -j MASQUERADE
$ sudo iptables -A FORWARD -i service -o wlan0 -j ACCEPT
$ sudo iptables -A FORWARD -i wlan0 -o server -m state --state RELATED,ESTABLISHED -j ACCEPT
$ git clone https://git.robur.coop/robur/chaos
$ cd chaos
$ opam source miou-solo5
$ opam source bstr
$ opam source mirage-crypto-rng-miou-solo5
$ opam source miou-solo5-net
$ dune build ./bin/main.exe
$ solo5-hvt --net:service=tap0 -- \
  --ipv4=10.0.0.2/24 \
  --ipv4-gateway=10.0.0.1 \
  --server 134.57.254.19 \
  --color=always -vvv
```
