open Lwt

let sleep_and_run c ms =
    Printf.printf "%Ld: sleeping %d...\n%!" (Event_clock.Mclock.elapsed_ns c) ms;
    Event_clock.Time.sleep_ns (Duration.of_ms ms) >>= fun () ->
    Printf.printf "%Ld: done %d...\n%!" (Event_clock.Mclock.elapsed_ns c) ms;
    Lwt.return_unit

let rec loop x =
    match x with
    | 0 -> Lwt.return_unit
    | n -> Lwt.pause () >>= fun () -> loop (n-1)

let rec wait_loop c ms n =
    match n with
    | 0 -> Lwt.return_unit
    | n -> Printf.printf "%Ld: -- interleaved sleep loop cnt %d..\n%!" (Event_clock.Mclock.elapsed_ns c) n;
           Event_clock.Time.sleep_ns (Duration.of_ms ms) >>= fun () -> 
           Printf.printf "%Ld: -- interleaved done loop cnt %d\n%!" (Event_clock.Mclock.elapsed_ns c) n;
           wait_loop c ms (n-1)

let test = 
    Event_clock.Mclock.connect () >>= fun c ->
    sleep_and_run c 3500 >>= fun () ->
    loop 100 >>= fun () ->
    Lwt.async ( fun f -> wait_loop c 999 10 ) ; 
    loop 5 >>= fun () -> (* only allow half of the results to return before cancelling *)
    Event_clock.Time.cancel_all () >>= fun () ->
    Lwt.async ( fun f -> wait_loop c 999 10 ) ; 
    Lwt.join [
        (loop 100000 >>= fun () -> sleep_and_run c 1000);
        loop 10000 ;
        (loop 1 >>= fun () -> sleep_and_run c 1000) ;
        loop 300 ;
        sleep_and_run c 2000 ; 
        loop 1000 ;
        sleep_and_run c 3000 ; 
        loop 300 ;
        sleep_and_run c 10000 ; 
        sleep_and_run c 2000 ; 
        sleep_and_run c 10000 ; 
        sleep_and_run c 2000 ; 
        (loop 100000 >>= fun () -> sleep_and_run c 1000);
        sleep_and_run c 2500 ; 
        sleep_and_run c 7000 ; 
        sleep_and_run c 2000 ; 
        sleep_and_run c 2500 ; 
        sleep_and_run c 7000 ; 
        sleep_and_run c 9000 ; 
        sleep_and_run c 1100 ] >>= fun () ->
    Printf.printf "time is %Ld\n%!" (Event_clock.Mclock.elapsed_ns c);
    Lwt.return_unit

let () =
    Lwt_main.run test
