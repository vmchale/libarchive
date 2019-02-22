#include <string.h>
#include <stdlib.h>
#include <archive.h>
#include <archive_entry.h>

// Unpack a tar archive (stored in a bytestring) into a particular directory
void unpack_in_dir(char* dirname, void *buff, size_t size) {

    // step 1: allocate/initialize archive struct
    struct archive *a;
    struct archive_entry *entry;
    a = archive_read_new();

    // autodetect archive format/compression
    archive_read_support_format_all(a);

    // open from memory (bytestring)
    archive_read_open_memory(a, buff, size);

    while (archive_read_next_header(a, &entry) == ARCHIVE_OK || archive_read_next_header(a, &entry) == ARCHIVE_RETRY) {
        const char* pre_path_name = archive_entry_pathname(entry);
        size_t fp_length = strlen(dirname) + strlen(pre_path_name);
        char* fp = malloc(fp_length + 1);
        strcpy(fp, dirname);
        strcat(fp, pre_path_name);
        archive_entry_set_pathname(entry, fp);
        archive_read_extract(a, entry, 0);
        archive_entry_set_pathname(entry, pre_path_name);
        free(fp);
        archive_read_data_skip(a);
    }
    free(dirname);

    archive_read_free(a);
}
