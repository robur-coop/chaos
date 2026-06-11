FROM ocaml/opam:debian-12-ocaml-5.4 AS builder
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
  pkg-config m4 build-essential libgmp-dev libseccomp-dev \
  && rm -rf /var/lib/apt/lists/*
USER opam
RUN opam remote add upstream git+https://github.com/ocaml/opam-repository.git
RUN opam update && opam install -y solo5 ocaml-solo5
RUN git clone https://github.com/robur-coop/chaos -b main chaos
WORKDIR /home/opam/chaos
RUN opam pin -yn .
RUN opam install --deps-only chaos
RUN opam exec -- make all

FROM scratch
COPY --from=builder /home/opam/chaos/chaos.hvt /chaos.hvt
COPY solo5.json /solo5.json
ENTRYPOINT ["/chaos.hvt"]
