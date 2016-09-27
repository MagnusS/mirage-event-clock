(*
 * Copyright (c) 2016 Magnus Skjegstad <magnus@docker.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open V1_LWT

module TimeEvent = struct
  type t = {
    waiter : unit Lwt.t;
    waker : unit Lwt.u;
    ts : int64;
  }

  let compare t1 t2 =
    Int64.compare t1.ts t2.ts

  let create ts =
    let waiter, waker = Lwt.wait () in
    { waiter; waker ; ts }

  let wait t =
    t.waiter

  let wake t =
    Lwt.wakeup t.waker ()

  let time t = 
    t.ts

end

module Mclock = struct
  type 'a io = 'a Lwt.t
  type error = unit

  type t = int64 (* reference point *)
  let current_time_in_ns = ref Int64.zero

  (** NOTE: Time only advances on events *)
  let elapsed_ns t =
    Int64.sub (!current_time_in_ns) t

  (** Period is unknown, as it will skip ahead for every event *)
  let period_ns t =
    None (** period unknown *)

  let disconnect t =
    Lwt.return_unit

  let advance_time ns =
    current_time_in_ns := Int64.add (!current_time_in_ns) ns

  let connect () = 
    Lwt.return (`Ok !current_time_in_ns)

end

module Time = struct
  type 'a io = 'a Lwt.t
  module Pqueue = Lwt_pqueue.Make(TimeEvent)

  let timeline = ref (Pqueue.empty)

(*  let run () =
    (match Pqueue.lookup_min (!timeline) with
    | Some te when (Int64.compare (TimeEvent.time te) (!Mclock.current_time_in_ns) <= 0) -> begin
        timeline := Pqueue.remove_min (!timeline) ; 
        (*Mclock.current_time_in_ns := (TimeEvent.time te);*)
        TimeEvent.wake te
      end
    | _ -> begin
        Mclock.advance_time (Duration.of_us 100);
        Printf.printf "len=%d time=%f\n%!" (Lwt_sequence.length Lwt_main.leave_iter_hooks) (Duration.to_f (!Mclock.current_time_in_ns));
        Printf.printf "timer_count=%d writable_count=%d readable_count=%d\n%!" (Lwt_engine.timer_count ()) (Lwt_engine.writable_count ()) (Lwt_engine.readable_count ())
    end) *)

  let rec run () =
    (match Pqueue.lookup_min (!timeline) with
    | Some te -> begin
        timeline := Pqueue.remove_min (!timeline) ; 
        Mclock.current_time_in_ns := (TimeEvent.time te);
        TimeEvent.wake te; 
      end
    | _ -> begin
    end)
    
  let sleep_ns ns = 
    let te = TimeEvent.create (Int64.add ns (!Mclock.current_time_in_ns)) in
    timeline := (Pqueue.add te (!timeline));
    TimeEvent.wait te
end

let () =
  let _ = Lwt_sequence.add_l (fun () -> Time.run ()) Lwt_main.leave_iter_hooks in
(*  Lwt_engine.on_timer 0.0001 true (fun f -> ()); (* this is required to unblock the main loop if all threads are sleeping *)*)
  ()

