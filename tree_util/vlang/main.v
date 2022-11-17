/*
echo main.v | entr -c v run /_ ~/code/explore/elm/turtle/
*/
import os
import strconv

[noreturn]
fn die(msg string) {
	println(msg)
	exit(1)
}

fn ensure_trailing_sep(s string) string {
	return if s.ends_with(os.path_separator) {
		s
	} else {
		s + os.path_separator
	}
}

fn get_valid_root_or_die() string {
	exe := os.base(os.args[0])
	path := os.args[1] or { die('Usage: ./$exe PATH') }

	if !os.is_dir(path) {
		die('Invalid path: $path')
	}

	return ensure_trailing_sep(path)
}

fn get_emoji(is_dir bool) string {
	return if is_dir {
		'📁'
	} else {
		'📝'
	}
}

// I copy/pasted then tweaked the `os.walk` function. Its callback doesn't give access to directories.
fn walk(path string, f fn (string), f2 fn (string)) {
	if path.len == 0 {
		return
	}
	if !os.is_dir(path) {
		return
	}
	mut remaining := []string{cap: 1000}
	clean_path := path.trim_right(os.path_separator)
	$if windows {
		remaining << clean_path.replace('/', '\\')
	} $else {
		remaining << clean_path
	}
	for remaining.len > 0 {
		cpath := remaining.pop()
		hidden := os.base(cpath).starts_with('.')
		if os.is_dir(cpath) {
			if hidden {
				continue
			} else {
				f2(cpath)
			}
		}
		if os.is_link(cpath) || !os.is_dir(cpath) {
			if !hidden {
				f(cpath)
			}
			continue
		}
		mut files := os.ls(cpath) or { continue }
		for idx := files.len - 1; idx >= 0; idx-- {
			remaining << cpath + os.path_separator + files[idx]
		}
	}
}

fn print_path(root string, path string, is_dir bool, size u64) {
	// TODO: use `ensure_no_trailing_sep` at the start of the program and skip this extra fn call
	if ensure_trailing_sep(path) == root {
		return
	}
	short_path := path.replace(root, '')
	depth := short_path.count('/')
	depth_to_ws := '  '.repeat(depth)
	emoji := get_emoji(is_dir)

	// TODO: bytes to human
	size_str := strconv.v_sprintf('%10d B', size)
	depth_str := strconv.v_sprintf('%3d', depth)

	println('[$size_str] $emoji [$depth_str] $depth_to_ws$short_path')
}

fn main() {
	root := get_valid_root_or_die()

	println('Processing: $root\n')

	walk(root, fn [root] (path string) {
		// Files
		print_path(root, path, false, os.file_size(path))
	}, fn [root] (path string) {
		// Directories
		print_path(root, path, true, os.file_size(path))
	})
}
