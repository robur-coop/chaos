let () = Tscclock.init ()

let test00 =
  Alcotest.test_case "test00" `Quick @@ fun () ->
  let local = Chaos.Local.make ~precision_quantum:1e-08 Tscclock.now in
  let stats = Chaos.Stats.make 0 in
  let sample0 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 47753693498737000L)
    ; offset= -3.170628e-02
    ; peer_delay= 1.917290e-05
    ; peer_dispersion= 1.917349e-06
    ; root_delay= 2.380371e-03
    ; root_dispersion= 3.495789e-02 } in
  Chaos.Stats.accumulate stats sample0;
  Chaos.Stats.regression local stats;
  let sample1 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 47755706584186000L)
    ; offset= -3.176304e-02
    ; peer_delay= 1.874892e-05
    ; peer_dispersion= 1.917349e-06
    ; root_delay= 2.380371e-03
    ; root_dispersion= 3.498840e-02 } in
  Chaos.Stats.accumulate stats sample1;
  Chaos.Stats.regression local stats;
  let sample2 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 47757693726896000L)
    ; offset= -3.134826e-02
    ; peer_delay= 1.956196e-05
    ; peer_dispersion= 1.917349e-06
    ; root_delay= 2.380371e-03
    ; root_dispersion= 3.501892e-02 } in
  Chaos.Stats.accumulate stats sample2;
  Chaos.Stats.regression local stats
;;

let test01 =
  Alcotest.test_case "test01" `Quick @@ fun () ->
  let local = Chaos.Local.make ~precision_quantum:1e-08 Tscclock.now in
  let stats = Chaos.Stats.make 0 in
  let sample0 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54657956339634000L)
    ; offset= -2.087731e-02
    ; peer_delay= 1.963961e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.754211e-02 } in
  Chaos.Stats.accumulate stats sample0;
  Chaos.Stats.regression local stats;
  let sample1 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54659970132960000L)
    ; offset= -2.114112e-02
    ; peer_delay= 2.750661e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.757263e-02 } in
  Chaos.Stats.accumulate stats sample1;
  Chaos.Stats.regression local stats;
  let sample2 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54661958666252000L)
    ; offset= -2.113961e-02
    ; peer_delay= 2.778892e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.760315e-02 } in
  Chaos.Stats.accumulate stats sample2;
  Chaos.Stats.regression local stats;
  let sample3 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54663972303156000L)
    ; offset= -2.071727e-02
    ; peer_delay= 2.833607e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.761841e-02 } in
  Chaos.Stats.accumulate stats sample3;
  Chaos.Stats.regression local stats;
  let sample4 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54665958067796000L)
    ; offset= -2.041962e-02
    ; peer_delay= 2.794037e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.764893e-02 } in
  Chaos.Stats.accumulate stats sample4;
  Chaos.Stats.regression local stats;
  let sample5 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54667972471295000L)
    ; offset= -2.032515e-02
    ; peer_delay= 2.779683e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.767944e-02 } in
  Chaos.Stats.accumulate stats sample5;
  Chaos.Stats.regression local stats;
  let sample6 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54669961406534000L)
    ; offset= -2.025981e-02
    ; peer_delay= 1.877593e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.770996e-02 } in
  Chaos.Stats.accumulate stats sample6;
  Chaos.Stats.regression local stats;
  let sample7 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54671982973051000L)
    ; offset= -2.024449e-02
    ; peer_delay= 1.910073e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.774048e-02 } in
  Chaos.Stats.accumulate stats sample7;
  Chaos.Stats.regression local stats;
  let sample8 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54673963430823000L)
    ; offset= -2.017893e-02
    ; peer_delay= 1.847744e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.777100e-02 } in
  Chaos.Stats.accumulate stats sample8;
  Chaos.Stats.regression local stats;
  let sample9 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54675976921057000L)
    ; offset= -2.013198e-02
    ; peer_delay= 2.846168e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.780151e-02 } in
  Chaos.Stats.accumulate stats sample9;
  Chaos.Stats.regression local stats;
  let sample10 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54677965820847000L)
    ; offset= -1.997788e-02
    ; peer_delay= 1.920434e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.783203e-02 } in
  Chaos.Stats.accumulate stats sample10;
  Chaos.Stats.regression local stats;
  let sample11 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54679980212344000L)
    ; offset= -2.018308e-02
    ; peer_delay= 3.014482e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.786255e-02 } in
  Chaos.Stats.accumulate stats sample11;
  Chaos.Stats.regression local stats;
  let sample12 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54681968846381000L)
    ; offset= -2.142633e-02
    ; peer_delay= 1.843576e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.789307e-02 } in
  Chaos.Stats.accumulate stats sample12;
  Chaos.Stats.regression local stats;
  let sample13 =
    { Chaos.Sample.time= Ptime.unsafe_of_d_ps (20269, 54683982242567000L)
    ; offset= -2.024412e-02
    ; peer_delay= 2.866832e-05
    ; peer_dispersion= 1.923349e-06
    ; root_delay= 2.502441e-03
    ; root_dispersion= 2.792358e-02 } in
  Chaos.Stats.accumulate stats sample13;
  Chaos.Stats.regression local stats
;;

let () = Alcotest.run "regression" [ ("examples", [ test00; test01 ])]
