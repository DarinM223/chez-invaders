#include "inotify_watcher.h"
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

static inotify_error make_non_blocking(int32_t fd)
{
    // Get the current file status flags
    int32_t flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1) {
        return INOTIFY_NONBLOCKING_FAIL;
    }

    // Add the O_NONBLOCK flag and save the new flags back
    if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        return INOTIFY_NONBLOCKING_FAIL;
    }
    return INOTIFY_SUCCESS;
}

inotify_error inotify_watcher_init(inotify_watcher *watcher)
{
    watcher->fd = inotify_init();
    if (watcher->fd < 0) {
        return INOTIFY_INIT_FAIL;
    }
    return make_non_blocking(watcher->fd);
}

inotify_error inotify_watcher_add_watch(inotify_watcher *watcher,
                                        const char *path)
{
    int32_t wd =
        inotify_add_watch(watcher->fd, ".", IN_MODIFY | IN_CREATE | IN_DELETE);
    if (wd < 0) {
        return INOTIFY_ADD_WATCH_FAIL;
    }
    watcher->wd[watcher->wd_end] = wd;
    watcher->wd_end = (watcher->wd_end + 1) % MAX_DIRECTORIES;
    if (watcher->wd_end == watcher->wd_start) {
        int32_t result =
            inotify_rm_watch(watcher->fd, watcher->wd[watcher->wd_start]);
        watcher->wd_start = (watcher->wd_start + 1) % MAX_DIRECTORIES;
        if (result < 0) {
            return INOTIFY_REMOVE_WATCH_FAIL;
        }
    }
    return INOTIFY_SUCCESS;
}

void inotify_watcher_drain_files(inotify_watcher *watcher)
{
    read(watcher->fd, watcher->buf, BUF_LEN);
    memset(watcher->buf, 0, BUF_LEN);
}

void inotify_watcher_watch_files(inotify_watcher *watcher, void *ctx)
{
    int32_t len = read(watcher->fd, watcher->buf, BUF_LEN);
    int32_t i = 0;
    while (i < len) {
        struct inotify_event *event = (struct inotify_event *)&watcher->buf[i];
        if (event->len) {
            watcher->callback(event->name, ctx);
        }
        i += EVENT_SIZE + event->len;
    }
}
