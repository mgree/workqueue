let list_of_queue (q:'a Queue.t) : 'a list =
  List.rev (Queue.fold (fun l v -> v::l) [] q)
module WorkQueue = struct
  type 'a t = { thread : Thread.t ; 
             lock : Mutex.t ; 
             tasks : (unit -> 'a) Queue.t;
             results : 'a Queue.t;
             stopped : bool ref;
           }

  exception Stopped

  let with_lock (l:Mutex.t) (f:unit -> unit) : unit =
    Mutex.lock l;
    (try f () with _ -> ());
    Mutex.unlock l

  let wq_run (l:Mutex.t) (q:(unit -> 'a) Queue.t) (r:'a Queue.t) (stopped:bool ref) : unit -> unit =
    let rec loop () =
      Thread.yield ();
      (* find a task *)
      let fo = ref None in
      with_lock l (fun () -> fo := try Some (Queue.take q) with Queue.Empty -> None);
      (* run the task *)
      match !fo with
        | None -> if !stopped then () else loop ()
        | Some f -> 
            with_lock l (fun () -> Queue.add (f ()) r);
            loop ()
    in
    loop  
      
  let create () : 'a t =
    let l = Mutex.create () in
    let q = Queue.create () in
    let r = Queue.create () in
    let s = ref false in
    let t = Thread.create (wq_run l q r s) () in
    { thread = t; lock = l; tasks = q; results = r; stopped = s }

  let stop (wq:'a t) : unit =
    wq.stopped := true

  let enq (f:unit -> 'a) (wq:'a t) : unit =
    if !(wq.stopped) then raise Stopped;
    with_lock wq.lock (fun () -> Queue.add f wq.tasks)

  let wait (wq:'a t) : unit =
    stop wq;
    Thread.join wq.thread

  let results (wq:'a t) : 'a list =
    let r = Queue.create () in
    with_lock wq.lock (fun () -> Queue.transfer wq.results r);
    list_of_queue r
end

module Test = struct
  open Printf
  
  let line =
    let s = String.make 80 '-' in
    fun () -> print_endline s

  let test msg (f:unit->'a) (v:'a) = 
    let s = 
      try if f () = v then "OK" else "FAILED"
      with exn -> "FAILED (ERROR: " ^ (Printexc.to_string exn) ^ ")" in
    printf "  %s: %s\n" msg s
  
  let tests : (unit -> unit) list ref = ref []
  let add_test name f v = tests := (fun () -> test name f v)::!tests

  let touch x = ()

  let test_create () = 
    let wq = WorkQueue.create () in
    touch wq;;
  add_test "Create" test_create ()

  let test_stop () = 
    let wq = WorkQueue.create () in
    WorkQueue.stop wq;;
  add_test "Stop" test_stop ()

  let test_enq () =
    let wq = WorkQueue.create () in
    WorkQueue.enq (fun () -> 5) wq;;
  add_test "Enq" test_enq ()

  let test_results_empty () = 
    let wq = WorkQueue.create () in
    WorkQueue.results wq;;
  add_test "Results (empty)" test_results_empty []

  let test_results_enq () =
    let wq = WorkQueue.create () in
    WorkQueue.enq (fun () -> 5) wq;
    WorkQueue.wait wq;
    WorkQueue.results wq;;
  add_test "Enq/Wait/Results" test_results_enq [5]

  let test_results_enq_delay () =
    let wq = WorkQueue.create () in
    WorkQueue.enq (fun () -> 5) wq;
    Thread.delay 0.1;
    WorkQueue.results wq;;
  add_test "Enq/Delay/Results" test_results_enq_delay [5]

  let test_results_enq_many () =
    let wq = WorkQueue.create () in
    WorkQueue.enq (fun () -> 5) wq;
    WorkQueue.enq (fun () -> 6) wq;
    WorkQueue.enq (fun () -> 7) wq;
    WorkQueue.enq (fun () -> 8) wq;
    WorkQueue.enq (fun () -> 9) wq;
    WorkQueue.wait wq;
    WorkQueue.results wq;;
  add_test "Enq/Wait/Results (many)" test_results_enq_many [5;6;7;8;9]

  let test_results_enq_many_delay () =
    let wq = WorkQueue.create () in
    WorkQueue.enq (fun () -> 5) wq;
    WorkQueue.enq (fun () -> 6) wq;
    WorkQueue.enq (fun () -> 7) wq;
    WorkQueue.enq (fun () -> 8) wq;
    WorkQueue.enq (fun () -> 9) wq;
    Thread.delay 0.1;
    WorkQueue.results wq;;
  add_test "Enq/Delay/Results (many)" test_results_enq_many_delay [5;6;7;8;9]

  let run () =
    line ();
    printf "Running tests...\n";
    List.iter (fun t -> t ()) (List.rev !tests);
    line ();
    printf "All tests passed.\n"
end;;

Test.run ();;
