module Debug_runtime = Minidebug_runtime.Flushing (struct
  let debug_ch = stdout
  let time_tagged = true
end)

let%debug_show test_logging : string = "Hello World"
let () = print_endline test_logging
