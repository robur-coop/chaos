type t = {
    ipaddr: Ipaddr.V4.t
  ; domain_name: [ `host ] Domain_name.t
  ; stratum: int
  ; poll: int
}
