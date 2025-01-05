#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

// Log levels
#define LOG_LEVEL_TRACE 0
#define LOG_LEVEL_INFO 1
#define LOG_LEVEL_WARN 2
#define LOG_LEVEL_FATAL 3

#ifndef LOG_LEVEL
#define LOG_LEVEL LOG_LEVEL_INFO
#endif

// Terminal detection
#define IS_TERMINAL (isatty(STDERR_FILENO))

// ANSI colors (empty strings on Windows)
#ifdef _WIN32
#define ANSI_RED ""
#define ANSI_YELLOW ""
#define ANSI_BLUE ""
#define ANSI_GRAY ""
#define ANSI_RESET ""
#else
#define ANSI_RED "\033[1;31m"
#define ANSI_YELLOW "\033[1;33m"
#define ANSI_BLUE "\033[1;34m"
#define ANSI_GRAY "\033[1;90m"
#define ANSI_RESET "\033[0m"
#endif

// Requires a buffer size of at least 20 bytes.
// Returns the number of bytes written (0 indicates failure).
static inline int get_timestamp(char* buf, size_t buf_size) {
  time_t now;
  time(&now);
  return strftime(buf, buf_size, "%F %T", gmtime(&now));
}

#define LOG_WITH_LEVEL(level, color, fmt, ...)                             \
  do {                                                                     \
    char timestamp[20];                                                    \
    get_timestamp(timestamp, sizeof(timestamp));                           \
    if (IS_TERMINAL) {                                                     \
      fprintf(stderr, "%s%s  [%s]%s  " fmt "\n  => %s:%d\n\n", timestamp,  \
              color, level, ANSI_RESET, __VA_ARGS__, __FILE__, __LINE__);  \
    } else {                                                               \
      fprintf(stderr, "%s [%s] " fmt "\n  => %s:%d\n\n", timestamp, level, \
              __VA_ARGS__, __FILE__, __LINE__);                            \
    }                                                                      \
  } while (0)

#define die(fmt, ...)                                      \
  do {                                                     \
    LOG_WITH_LEVEL("FATAL", ANSI_RED, fmt, ##__VA_ARGS__); \
    exit(1);                                               \
  } while (0)

#define warn(fmt, ...)                                            \
  do {                                                            \
    if (LOG_LEVEL <= LOG_LEVEL_WARN)                              \
      LOG_WITH_LEVEL("WARNING", ANSI_YELLOW, fmt, ##__VA_ARGS__); \
  } while (0)

#define info(fmt, ...)                                       \
  do {                                                       \
    if (LOG_LEVEL <= LOG_LEVEL_INFO)                         \
      LOG_WITH_LEVEL("INFO", ANSI_BLUE, fmt, ##__VA_ARGS__); \
  } while (0)

#define trace(fmt, ...)                                       \
  do {                                                        \
    if (LOG_LEVEL <= LOG_LEVEL_TRACE)                         \
      LOG_WITH_LEVEL("TRACE", ANSI_GRAY, fmt, ##__VA_ARGS__); \
  } while (0)

#endif /* UTILS_H */
