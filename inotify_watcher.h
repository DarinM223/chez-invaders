#ifndef INOTIFY_WATCHER_H
#define INOTIFY_WATCHER_H

#include <stddef.h>
#include <stdint.h>
#include <sys/inotify.h>

#define EVENT_SIZE (sizeof(struct inotify_event))
#define FILENAME_APPROX_SIZE 16
#define BUF_LEN (1024 * (EVENT_SIZE + FILENAME_APPROX_SIZE))
#define MAX_DIRECTORIES 10

typedef enum inotify_error {
    INOTIFY_SUCCESS = 0,
    INOTIFY_INIT_FAIL,
    INOTIFY_NONBLOCKING_FAIL,
    INOTIFY_ADD_WATCH_FAIL,
    INOTIFY_REMOVE_WATCH_FAIL,
} inotify_error;

typedef struct inotify_watcher {
    int32_t fd;
    int32_t wd[MAX_DIRECTORIES + 1];
    char filenames[FILENAME_APPROX_SIZE][MAX_DIRECTORIES];
    char buf[BUF_LEN];
    void (*callback)(const char *filename, void *);
    size_t wd_start;
    size_t wd_end;
} inotify_watcher;

inotify_error inotify_watcher_init(inotify_watcher *watcher);
inotify_error inotify_watcher_add_watch(inotify_watcher *watcher,
                                        const char *path);
void inotify_watcher_drain_files(inotify_watcher *watcher);
void inotify_watcher_watch_files(inotify_watcher *watcher, void *ctx);

#endif // INOTIFY_WATCHER_H