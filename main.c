#include "config.h"
#include "parse.h"
#include "utils.h"
#include "vm.h"

static void cli_help(const char* argv[]) {
    printf("Usage: %s dump|run|repl [filename]\n", argv[0]);
    exit(0);
}

int main(int argc, const char* argv[]) {
    if (argc == 2 && strcmp(argv[1], "repl") == 0) {
        vm_repl();
    } else if (argc == 3) {
        char* source = NULL;
        size_t source_len = 0;
        if (strcmp(argv[2], "-") == 0) {
            read_stdin(&source, &source_len);
        } else {
            read_file(argv[2], &source, &source_len);
        }

        if (strcmp(argv[1], "dump") == 0) {
            return (int)vm_interpret(source, source_len, vm_dump);
        }
        if (strcmp(argv[1], "run") == 0) {
            return (int)vm_interpret(source, source_len, vm_run_bytecode);
        }
        if (strcmp(argv[1], "fmt") == 0) {
            return (int)fmt(source, source_len);
        }
    } else {
        cli_help(argv);
    }
}
