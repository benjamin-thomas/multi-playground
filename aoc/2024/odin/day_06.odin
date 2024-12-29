package main

import "core:bufio"
import "core:bytes"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:strings"
import "core:time"

program_start :: proc(filename: string, interactive: bool) {
// Hide cursor
    fmt.print("\x1b[?25l")

    file_bytes, ok := os.read_entire_file(filename)
    if !ok {
        fmt.panicf("Error reading file: '%s'", filename)
    }
    defer delete(file_bytes)

    if file_bytes[len(file_bytes) - 1] == '\n' {
        file_bytes = file_bytes[0:len(file_bytes) - 1]
    }

    lines := bytes.split(file_bytes, []byte{ '\n' })
    defer delete(lines)

    dim_y := len(lines)
    dim_x := len(lines[0])
    grid := make([dynamic]byte, dim_y * dim_y, dim_y * dim_y)
    defer delete(grid)

    for line, y in lines {
        for c, x in line {
            if c != '\n' {
                grid[x + y * dim_x] = c
            }
        }
    }

    guard_pos := -1
    for c, i in grid {
        if c == '^' {
            guard_pos = i
            break
        }
    }
    assert(guard_pos != -1)
    loops := run(grid[:], dim_y, dim_x, guard_pos, 0, interactive)
    trails := make([dynamic]int)
    defer delete(trails)
    for c, i in grid {
        if c == 'X' {
            append(&trails, i)
        }
    }
    //    fmt.printfln("Trail count: %d\nTrail positions:%v", len(trails), trails)

    // First reset the grid
    for t in trails {
        grid[t] = '.'
    }

    for t in trails {
        for c, i in grid {
            if c == 'X' || c == 'O' {
                grid[i] = '.'
            }
        }

        grid[t] = 'O'
        loops = run(grid[:], dim_y, dim_x, guard_pos, loops, interactive)
    }

    fmt.printfln("Found %d trails", len(trails))
    fmt.printfln("Found %d loops", loops)

    // Restore cursor on exit
    fmt.print("\x1b[?25h")
}

/*

`./bin/odin` is the out param used by the IntelliJ plugin so let's use that 'cause it's handy (gitignore)

echo ./day_06.odin | entr -rc odin run /_ -file -out=./bin/odin

Leaving the valgrind instructions out of curiosity.
It's not really needed though, the tracking_allocator technique is much faster! 
odin build ./day_06.odin -file -out=./bin/odin
valgrind --leak-check=full ./bin/odin

*/
main :: proc() {

// get env var LOG
    log_env := os.get_env("LOG")
    interactive := os.get_env("INTERACTIVE") == "1"
    fmt.printf("LOG: >>%v<<\n", log_env)
    log.debug("LOG: >>%v<<\n", log_env)
    // context.logger = log_env != "0" ? log.create_console_logger() : log.nil_logger()

    // Setting the logger via an if statement does not work (bracket scope issue it seems)
    context.logger = log.create_console_logger() if log_env != "0" else log.nil_logger()
    // if log_env != "0" {
    // 	fmt.println("Initializing logger...")
    // 	context.logger = log.create_console_logger()
    // } else {
    // 	fmt.println("Should debug this!")
    // 	context.logger = log.nil_logger()
    // }
    log.info("Booting up...")
    // fmt.assertf(1 == 2, "??")

    default_allocator := context.allocator
    tracking_allocator: mem.Tracking_Allocator
    mem.tracking_allocator_init(&tracking_allocator, default_allocator)
    context.allocator = mem.tracking_allocator(&tracking_allocator)

    /*
    The name doesn't make much sens, not important. 
    
    Got that leak tracking code from here:
        https://github.com/karl-zylinski/odin-raylib-hot-reload-game-template/blob/49825cea9393463e6181d59c0d57fe8c758ea8e5/main_hot_reload/main_hot_reload.odin#L93
    */
    reset_tracking_allocator :: proc(a: ^mem.Tracking_Allocator) -> bool {
        err := false

        if len(a.allocation_map) == 0 {
            fmt.println(">> No leaks!")
        } else {
            for _, value in a.allocation_map {
                fmt.printf("%v: Leaked %v bytes\n", value.location, value.size)
                // fmt.printfln("%#v", value)
                err = true
            }
        }

        mem.tracking_allocator_clear(a)
        return err
    }

    fmt.println("\n\nExample")
    fmt.println("=======")
    program_start("../_inputs/06.example", interactive)

    fmt.println("\n\nReal")
    fmt.println("====")
    program_start("../_inputs/06.txt", interactive)

    fmt.print("\n---\n\n")
    if len(tracking_allocator.bad_free_array) == 0 {
        fmt.println(">> No double frees!")
    } else {
        for b in tracking_allocator.bad_free_array {
            log.errorf("Bad free at: %v", b.location)
        }

        panic("Bad free detected")
    }
    reset_tracking_allocator(&tracking_allocator)
}


Coord :: struct {
    y: int,
    x: int,
}

guard_icon :: proc(dir: Coord) -> byte {
    if (dir.y == -1 && dir.x == 0) {
        return '^'
    } else if (dir.y == 0 && dir.x == 1) {
        return '>'
    } else if (dir.y == 1 && dir.x == 0) {
        return 'v'
    } else if (dir.y == 0 && dir.x == -1) {
        return '<'
    }
    fmt.panicf("Bad dir: %v", dir)
}

rotate :: proc(dir: Coord) -> Coord {
    return Coord{ dir.x, -dir.y }
}

print_grid :: proc(bytes: []byte, dim_y: int, dim_x: int) {
    fmt.println()
    i := 0
    for c in bytes {
        if i >= dim_x {
            i = 0
            fmt.print("\n")
        }
        if c == 'X' {
            fmt.printf("\x1b[90m%c\x1b[0m", c)
        } else {
            fmt.printf("%c", c)
        }
        i += 1
    }
    fmt.println()
}

next_pos :: proc(
dim_y: int,
dim_x: int,
guard_dir: Coord,
guard_pos: int,
) -> ( int /* new_pos */, bool /* valid_pos */) {

    if guard_dir.y == -1 && guard_dir.x == 0 {
        pos := guard_pos - dim_x
        return pos, (pos >= 0)
    }

    if guard_dir.y == 0 && guard_dir.x == 1 {
        pos := guard_pos + 1
        is_valid := (pos % dim_x) != 0
        return pos, is_valid
    }

    if guard_dir.y == 1 && guard_dir.x == 0 {
        pos := guard_pos + dim_x
        return pos, pos < (dim_y * dim_x)
    }

    if guard_dir.y == 0 && guard_dir.x == -1 {
        pos := guard_pos - 1
        return pos, (pos % dim_x != dim_x - 1)
    }

    fmt.panicf("Bad guard dir: %v", guard_dir)
}

run :: proc(bytes: []byte, dim_y: int, dim_x: int, guard_pos: int, loops: int, interactive: bool) -> int {
    guard_pos := guard_pos
    guard_dir := Coord{ -1, 0 }
    iteration := -1
    for {
        iteration += 1
        if interactive {
            fmt.print("\x1b[H\x1b[2J") // clear screen
            fmt.println("Iteration:", iteration)
            fmt.printfln("Guard pos: %d", guard_pos)
            fmt.printfln("Detected loops: %d", loops)
            print_grid(bytes, dim_y, dim_x)
            time.sleep(10 * time.Millisecond)
        }
        next_pos, ok := next_pos(dim_y, dim_x, guard_dir, guard_pos)
        if iteration > dim_y * dim_x {
            return (loops + 1)
        }
        if !ok {
            bytes[guard_pos] = 'X'
            if interactive {
                fmt.print("\x1b[H\x1b[2J") // clear screen
                fmt.println("Iteration: %d", iteration)
                fmt.printfln("Guard pos: %d (would be off-grid at: %d)", guard_pos, next_pos)
                print_grid(bytes, dim_y, dim_x)
            }
            return loops
        }
        if next_pos >= 0 && next_pos < (dim_y * dim_x) {
            switch bytes[next_pos] {
            case '#', 'O':
                guard_dir = rotate(guard_dir)
            case:
                bytes[guard_pos] = 'X'
                guard_pos = next_pos
            }
            bytes[guard_pos] = guard_icon(guard_dir)
        }
    }
}
