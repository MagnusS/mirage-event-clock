open Lwt

let sleep_and_run ms =
    Lwt_io.printf "sleeping %d...\n" ms >>= fun () ->
    Event_clock.Time.sleep_ns (Duration.of_ms ms) >>= fun () ->
    Lwt_io.printf "done %d...\n" ms

let rec loop x =
    match x with
    | 0 -> Lwt.return_unit
    | n -> Lwt_main.yield () >>= fun () -> loop (n-1)

let test = 
    Event_clock.Mclock.connect () >>= function
    | `Error x -> raise (Failure "internal error")
    | `Ok x -> Lwt.return x >>= fun c ->
    (*Lwt.async (fun f -> Event_clock.Time.run ());*)
    sleep_and_run 3500 >>= fun () ->
    loop 100 >>= fun () ->
    Lwt.join [
        (loop 100000 >>= fun () -> sleep_and_run 1000);
        loop 10000 ;
        (loop 1 >>= fun () -> sleep_and_run 10000) ;
        loop 300 ;
        sleep_and_run 2000 ; 
        loop 1000 ;
        sleep_and_run 3000 ; 
        loop 300 ;
        sleep_and_run 10000 ; 
        sleep_and_run 2000 ; 
        sleep_and_run 10000 ; 
        sleep_and_run 2000 ; 
        (loop 100000 >>= fun () -> sleep_and_run 1000);
        sleep_and_run 2500 ; 
        sleep_and_run 7000 ; 
        sleep_and_run 2000 ; 
        sleep_and_run 2500 ; 
        sleep_and_run 7000 ; 
        sleep_and_run 9000 ; 
        sleep_and_run 1100 ] >>= fun () ->
    Lwt_io.printf "time is %Ld\n" (Event_clock.Mclock.elapsed_ns c)

let () =
    Lwt_main.run test
